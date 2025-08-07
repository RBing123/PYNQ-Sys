package jpeg

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._
import java.rmi.dgc.DGC
import scala.math.round
import chisel3.experimental._
import scala.math.cos
import scala.math.Pi 

/**
  * Object for DCT
  */
object DCTChisel {
    def apply(matrixIn: Valid[Vec[Vec[SInt]]], shiftedOut: Vec[Vec[SInt]], dctOut: Valid[Vec[Vec[SInt]]]) = {
        val mod = Module(new DCTChisel)
        mod.io.in := matrixIn
        mod.io.shiftedOut := shiftedOut
        mod.io.dctOut := dctOut
        mod
    }
}

/**
  * Creates FSM states for DCT
  */
object DCTState extends ChiselEnum { 
    val loading, shifting, calculating, waiting = Value 
}

/** Performs DCT on 8x8 Matrix with scaling
  * 
  * IO
  * @param matrixIn Input matrix to perform DCT on
  * 
  * @return shiftedOut Shifted version of input matrix by -128
  * @return dctOut Resulting scaled output of DCT
  */
class DCTChisel extends Module {
    val io = IO(new Bundle {
        val in = Flipped(Valid(new Bundle {
            val matrixIn = Input(Vec(8, Vec(8, SInt(9.W))))
        }))
        val shiftedOut = Output(Vec(8, Vec(8, SInt(9.W)))) // Test output to check shiftedblock
        val dctOut = Valid(Vec(8, Vec(8, SInt(32.W))))
    })

    // Initializes registers for matrixs and Valid bit 
    val matrixInput  = Reg(Vec(8, Vec(8, SInt(9.W))))
    val shiftedBlock = Reg(Vec(8, Vec(8, SInt(9.W))))
    val matrixOutput = Reg(Vec(8, Vec(8, SInt(32.W))))
    val validOut  = RegInit(false.B)

    // Assignes outputs
    io.dctOut.valid := validOut
    io.dctOut.bits := matrixOutput
    io.shiftedOut := DontCare


    // Function to compute DCT values for each element of input matrix
    def DCT(matrix: Vec[Vec[SInt]]): Vec[Vec[SInt]] = {
        val dctMatrix = Wire(Vec(8, Vec(8, SInt(32.W))))

        // Compute DCT
        for (u <- 0 until 8) {
            for (v <- 0 until 8) {
                var sum = 0.S
                for (i <- 0 until 8) {
                    for (j <- 0 until 8) {
                        val pixelValue = matrix(i)(j)
                        // Scale the cosine values to preserve precision
                        val cosVal = (math.cos((2 * i + 1) * u * Pi / 16) * math.cos((2 * j + 1) * v * Pi / 16) * 100).toInt.S
                        sum = sum +& pixelValue * cosVal
                    }
                }
  
                // Scale alphaU/V to perserve percision
                val alphaU = if (u == 0) (1.0 / math.sqrt(2)) * 100 else 100
                val alphaV = if (v == 0) (1.0 / math.sqrt(2)) * 100 else 100
                val scaledSum = (alphaU.toInt.S * alphaV.toInt.S * sum / 4.S)
                dctMatrix(u)(v) := scaledSum
            }
        }

        dctMatrix
    }

    // Initilizes state and defines FSM
    val state = RegInit(DCTState.waiting)
    switch(state) {
        is(DCTState.waiting) {
            when(io.in.valid) {
                matrixInput := io.in.bits.matrixIn
                state := DCTState.shifting
                validOut := false.B
            }
        }
        is(DCTState.shifting) {
            // Performs shift on input matrix to normalize it before computing DCT  
            for (i <- 0 until 8) {
                for (j <- 0 until 8) {
                    shiftedBlock(i)(j) := io.in.bits.matrixIn(i)(j) -& 128.S
                }
            }
            io.shiftedOut := shiftedBlock
            state := DCTState.calculating
        }
        is(DCTState.calculating) {
            // Assignes output matrix to calculated DCT values
            matrixOutput := DCT(shiftedBlock)
            state := DCTState.waiting
            validOut := true.B
        }
    }
}