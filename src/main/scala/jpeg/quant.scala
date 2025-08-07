package jpeg

import chisel3._
import chisel3.util._

/**
  * Object for Quantization
  */
object QuantizationChisel {
    def apply(params: JPEGParams, data: Valid[Vec[Vec[SInt]]], quantTable: Vec[Vec[SInt]], out: Valid[Vec[Vec[SInt]]]) = {
        val mod = Module(new QuantizationChisel(params))
        mod.io.in := data
        mod.io.out := out
        mod.io.quantTable := quantTable
        mod
    }
}

/**
  * Object for Inverse Quantization
  */
object InverseQuantizationChisel {
    def apply(params: JPEGParams, data: Valid[Vec[Vec[SInt]]], quantTable: Vec[Vec[SInt]], out: Valid[Vec[Vec[SInt]]]) = {
        val mod = Module(new InverseQuantizationChisel(params))
        mod.io.in := data
        mod.io.out := out
        mod.io.quantTable := quantTable
        mod
    }
}

/**
  * Creates states for Quantization and Inverse Quantization
  */
object QuantState extends ChiselEnum {
    val idle, quant = Value
}

/**
  * Performs Quantization
  *
  * @param p JPEG Parameters
  * 
  * IO
  * @param data Data to be use for Quantization
  * @param quantTable Quantization Table to use
  * 
  * @return out Result of Quantization using quantTable and data
  * @return state Current state of state machine
  */
class QuantizationChisel(p: JPEGParams) extends Module {
    val io = IO(new Bundle {
        val in = Flipped(Valid(new Bundle{
            val data = Input(Vec(p.numRows, Vec(p.numCols, SInt(32.W))))
        }))
        val quantTable = Input(Vec(p.numRows, Vec(p.numCols, SInt(12.W))))
        val out = Valid(Vec(p.numRows, Vec(p.numCols, SInt(32.W))))
        val state = Output(QuantState())
    })

    // registers to hold io
    val outputReg = RegInit(VecInit(Seq.fill(p.numRows)(VecInit(Seq.fill(p.numCols)(0.S(32.W))))))
    val dataReg = RegInit(VecInit(Seq.fill(p.numRows)(VecInit(Seq.fill(p.numCols)(0.S(32.W))))))
    val quantTabReg = RegInit(VecInit(Seq.fill(p.numRows)(VecInit(Seq.fill(p.numCols)(0.S(12.W))))))
    val outValid = RegInit(false.B)

    // assigns outputs
    val stateReg = RegInit(QuantState.idle)
    io.out.valid := outValid
    io.state := stateReg
    io.out.bits := outputReg

    // row and col counters
    val rCounter = Counter(p.numRows)
    val cCounter = Counter(p.numCols)

    // Statemachine logic
    switch(stateReg){
        is(QuantState.idle){
            when(io.in.fire){
                dataReg := VecInit(io.in.bits.data.map(row => VecInit(row.map(_ / 1000000.S))))
                quantTabReg := io.quantTable
                stateReg := QuantState.quant
                outValid := false.B
            }
        }

        is(QuantState.quant){
            // performs round to the nearest integer, -.5 -> -1, .49 -> 0.
            // TODO fix -1.49 rounding up to -2 in Chisel
            when(dataReg(rCounter.value)(cCounter.value) < 0.S){
                val remainder = dataReg(rCounter.value)(cCounter.value) % quantTabReg(rCounter.value)(cCounter.value)
                when(remainder <= (quantTabReg(rCounter.value)(cCounter.value) / -2.S)){
                    outputReg(rCounter.value)(cCounter.value) := (dataReg(rCounter.value)(cCounter.value) / quantTabReg(rCounter.value)(cCounter.value)) - 1.S  
                }
                .otherwise{
                    outputReg(rCounter.value)(cCounter.value) := dataReg(rCounter.value)(cCounter.value) / quantTabReg(rCounter.value)(cCounter.value)
                }
            }
            .otherwise{
                val remainder = dataReg(rCounter.value)(cCounter.value) % quantTabReg(rCounter.value)(cCounter.value)
                when(remainder >= (quantTabReg(rCounter.value)(cCounter.value) / 2.S)){
                    outputReg(rCounter.value)(cCounter.value) := (dataReg(rCounter.value)(cCounter.value) / quantTabReg(rCounter.value)(cCounter.value)) + 1.S
                }
                .otherwise{
                    outputReg(rCounter.value)(cCounter.value) := dataReg(rCounter.value)(cCounter.value) / quantTabReg(rCounter.value)(cCounter.value)
                }
            }
            when(cCounter.inc()) {
                rCounter.inc()
            }

            when(rCounter.value === (p.numRows - 1).U && cCounter.value === (p.numCols - 1).U) {
                outValid := true.B
                stateReg := QuantState.idle
            }
        }
    }

}

/**
  * Performs Inverse Quantization
  *
  * @param p JPEG Parameters
  * 
  * IO
  * @param data Data to be use for Inverse Quantization
  * @param quantTable Quantization Table to use
  * 
  * @return out Result of Inverse Quantization using quantTable and data
  * @return state Current state of state machine
  */
class InverseQuantizationChisel(p: JPEGParams) extends Module {
    val io = IO(new Bundle {
        val in = Flipped(Valid(new Bundle{
            val data = Input(Vec(p.numRows, Vec(p.numCols, SInt(12.W))))
        }))
        val out = Valid(Vec(p.numRows, Vec(p.numCols, SInt(12.W))))
        val quantTable = Input(Vec(p.numRows, Vec(p.numCols, SInt(12.W))))
        val state = Output(QuantState())
    })

    // registers to hold io
    val outputReg = RegInit(VecInit(Seq.fill(p.numRows)(VecInit(Seq.fill(p.numCols)(0.S(12.W))))))
    val dataReg = RegInit(VecInit(Seq.fill(p.numRows)(VecInit(Seq.fill(p.numCols)(0.S(12.W))))))
    val quantTabReg = RegInit(VecInit(Seq.fill(p.numRows)(VecInit(Seq.fill(p.numCols)(0.S(12.W))))))

    // assigns outputs
    val stateReg = RegInit(QuantState.idle)
    io.out.valid := false.B
    io.state := stateReg
    io.out.bits := outputReg

    // row and col counters
    val rCounter = Counter(p.numRows)
    val cCounter = Counter(p.numCols)

    // Statemachine logic
    switch(stateReg){
        is(QuantState.idle){
            when(io.in.fire){
                dataReg := io.in.bits.data
                quantTabReg := io.quantTable
                stateReg := QuantState.quant
            }
        }

        is(QuantState.quant){
            // undoing quantization
            outputReg(rCounter.value)(cCounter.value) := dataReg(rCounter.value)(cCounter.value) * quantTabReg(rCounter.value)(cCounter.value)
            
            when(cCounter.inc()) {
                rCounter.inc()
            }

            when(rCounter.value === (p.numRows - 1).U && cCounter.value === (p.numCols - 1).U) {
                io.out.valid := true.B
                stateReg := QuantState.idle
            }
        }
    }

}

