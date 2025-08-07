package jpeg

import chisel3._
import chisel3.util._

/** 
  * Creates states for decoding in RLE
  */
object RLEDecodingState extends ChiselEnum {
    val idle, load, decode = Value
}

/**
  * Performs RLE Decoding
  *
  * @param p JPEG Parameters
  * 
  * IO
  * @param data Data to decode
  * @param length Used space in data
  * 
  * @return out Result of decoding RLE
  * @return state Current state of state machine
  */
class RLEChiselDecode(p: JPEGParams) extends Module {
    val io = IO(new Bundle {
        val in = Flipped(Valid(new Bundle {
            val data = Vec(p.maxOutRLE, SInt(9.W))
        }))
        val length = Input(UInt(9.W))
        val out = Valid(Vec(p.totalElements, SInt(9.W)))
        val state = Output(RLEDecodingState())
    })

    val stateReg = RegInit(RLEDecodingState.idle)

    // Initialize to all zeros
    val dataReg = RegInit(VecInit(Seq.fill(p.maxOutRLE)(0.S(p.w8))))
    
    // Initialize output register
    val outputReg = RegInit(VecInit(Seq.fill(p.totalElements)(0.S(p.w8))))
    val outputIndexCounter = RegInit(0.U(log2Ceil(p.totalElements+1).W))
    val outValid = RegInit(false.B)

    // to keep track of the values from io.in.bits.data
    val freq = RegInit(0.S(p.w8))
    val value = RegInit(0.S(p.w8))
    val freqIndex = RegInit(0.U(log2Ceil(p.maxOutRLE+1).W))
    val valueIndex = RegInit(1.U(log2Ceil(p.maxOutRLE+1).W))

    val freqCounter = RegInit(0.S(log2Ceil(p.totalElements+1).W))
    val lengthCounter = RegInit(0.U(log2Ceil(p.maxOutRLE+1).W))

    // assigns output
    io.state := stateReg
    io.out.valid := outValid
    io.out.bits := outputReg

    // Statemachine logic
    switch(stateReg){
        is(RLEDecodingState.idle){
            when(io.in.fire){
                dataReg := io.in.bits.data
                freq := io.in.bits.data(freqIndex)
                value := io.in.bits.data(valueIndex)
                stateReg := RLEDecodingState.load
                outValid := false.B
                
            }
        }

        is(RLEDecodingState.load){
            when(lengthCounter === io.length){
                when(freqCounter < freq){
                    outputReg(outputIndexCounter) := value
                    outputIndexCounter := outputIndexCounter + 1.U
                    freqCounter := freqCounter + 1.S 
                }
                when(freqCounter === freq){
                    outValid := true.B
                    stateReg := RLEDecodingState.idle
                }
            }
            .otherwise{
                freq := dataReg(freqIndex)
                value := dataReg(valueIndex)
                freqCounter := 0.S
                stateReg := RLEDecodingState.decode
            }
        }

        is(RLEDecodingState.decode){
            when(freqCounter < freq){
                outputReg(outputIndexCounter) := value
                outputIndexCounter := outputIndexCounter + 1.U
                freqCounter := freqCounter + 1.S 
            }
            when(freqCounter === freq){
                freqIndex := freqIndex + 2.U
                valueIndex := valueIndex + 2.U
                stateReg := RLEDecodingState.load
                lengthCounter := lengthCounter + 2.U
            }
        }
    }
}

/** 
  * Creates states for decoding Delta Encoding
  */
object DecodingState extends ChiselEnum {
    val idle, decode = Value
}

/** Decodes Delta Encoding
  * 
  * @param p JPEG Parameters
  * 
  * IO
  * @param data Data to decode
  * 
  * @return out Result of decoding Delta Encoding
  * @return state Current state of state machine
  * 
  */
class DeltaChiselDecode(p: JPEGParams) extends Module {
    val io = IO(new Bundle {
        val in = Flipped(Valid(new Bundle {
            val data = Vec(p.totalElements, SInt(p.w8))
        }))
        val out = Valid(Vec(p.totalElements, SInt(p.w16))) // had to change bit width to 16 to pass tests
        val state = Output(DecodingState())
    })

    val stateReg = RegInit(DecodingState.idle)

    // Initialize to all zeros
    val dataReg = RegInit(VecInit(Seq.fill(p.totalElements)(0.S(p.w8))))
    val dataIndex = RegInit(1.U(log2Ceil(p.totalElements+1).W))

    // Initialize output register
    val outputReg = RegInit(VecInit(Seq.fill(p.totalElements)(0.S(p.w16)))) 
    val outValid = RegInit(false.B)

    // assign output
    io.state := stateReg
    io.out.valid := outValid
    io.out.bits := outputReg

    // Statemachine logic
    switch(stateReg){
        is(DecodingState.idle){
            when(io.in.fire){
                dataReg := io.in.bits.data
                outputReg(0) := io.in.bits.data(0)
                stateReg := DecodingState.decode
                outValid := false.B
            }
        }

        is(DecodingState.decode){
            when (dataIndex < p.totalElements.U) {
                outputReg(dataIndex) := outputReg(dataIndex - 1.U) + dataReg(dataIndex)
                dataIndex := dataIndex + 1.U
            }
            .otherwise{
                outValid := true.B
                stateReg := DecodingState.idle
            }
             
        }
    }
}