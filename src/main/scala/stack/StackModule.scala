package stack

import chisel3.stage.ChiselStage
import java.nio.file.Paths

// Your code starts here
import chisel3._
import chisel3.util._
class StackModule(val dataWidth: Int, val len: Int) extends Module {
  val io = IO(new Bundle {
    val in        = Input(UInt(32.W))
    val out       = Output(UInt(dataWidth.W))
    val underflow = Output(Bool())
    val overflow  = Output(Bool())
    val isEmpty   = Output(Bool())
    val isFull    = Output(Bool())
    val popped    = Output(Bool())
    val peeked    = Output(Bool())
  })

  private val OP_PUSH = "b0100111".U(7.W)
  private val OP_POP  = "b1000011".U(7.W)
  private val OP_PEEK = "b1000000".U(7.W)

  val mem = Reg(Vec(len, UInt(dataWidth.W)))
  val sp  = RegInit(0.U(log2Ceil(len + 1).W))

  // outputs are registered (what TB samples one cycle after the op)
  val outReg       = RegInit(0.U(dataWidth.W))
  val underflowReg = RegInit(false.B)
  val overflowReg  = RegInit(false.B)
  val poppedReg    = RegInit(false.B)
  val peekedReg    = RegInit(false.B)
  val isEmptyReg   = RegInit(true.B)
  val isFullReg    = RegInit(len.U === 0.U)

  io.out       := outReg
  io.underflow := underflowReg
  io.overflow  := overflowReg
  io.popped    := poppedReg
  io.peeked    := peekedReg
  io.isEmpty   := isEmptyReg
  io.isFull    := isFullReg

  // decode directly from io.in (no extra pipeline stage)
  val opcode = io.in(6, 0)
  val imm25  = io.in(31, 7)
  val pushData =
    if (dataWidth <= 25) imm25(dataWidth - 1, 0)
    else                 Cat(0.U((dataWidth - 25).W), imm25)

  // next-state defaults
  val spNext        = WireDefault(sp)
  val outNext       = WireDefault(0.U(dataWidth.W))
  val underflowNext = WireDefault(false.B)
  val overflowNext  = WireDefault(false.B)
  val poppedNext    = WireDefault(false.B)
  val peekedNext    = WireDefault(false.B)

  switch(opcode) {
    is(OP_PUSH) {
      when(sp === len.U) {
        overflowNext := true.B
      }.otherwise {
        mem(sp) := pushData
        spNext  := sp + 1.U
      }
    }
    is(OP_POP) {
      when(sp === 0.U) {
        underflowNext := true.B
        outNext       := 0.U
      }.otherwise {
        val idx = sp - 1.U
        outNext    := mem(idx)
        poppedNext := true.B
        spNext     := idx
      }
    }
    is(OP_PEEK) {
      when(sp === 0.U) {
        underflowNext := true.B
        outNext       := 0.U
      }.otherwise {
        outNext    := mem(sp - 1.U)
        peekedNext := true.B
      }
    }
  }

  // commit on clock (1-cycle latency)
  outReg       := outNext
  underflowReg := underflowNext
  overflowReg  := overflowNext
  poppedReg    := poppedNext
  peekedReg    := peekedNext
  sp           := spNext
  isEmptyReg   := spNext === 0.U
  isFullReg    := spNext === len.U
}
// Your code ends here

object SVGen extends App {
  val out = Paths.get(
    "out",
    this.getClass
      .getName
      .stripSuffix("$")
  ).toString
  new ChiselStage().emitSystemVerilog(
    new StackModule(args(0).toInt, args(1).toInt),
    Array("--target-dir", out),
  )
}
