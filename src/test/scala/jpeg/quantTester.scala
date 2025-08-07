package jpeg

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.experimental

/**
  * Test harness for Quantization Modules
  */
class QuantizationChiselTest extends AnyFlatSpec with ChiselScalatestTester {
    /**
      * Tests the functionality of Quantization in Chisel
      *
      * @param data Data to Quantify
      * @param qt Quantization table to use
      */
    def doQuantizationChiselTest(data: Seq[Seq[Int]], qt: Int): Unit = {
        val p = JPEGParams(8, 8, qt)
        val quantTable = p.getQuantTable
        test(new QuantizationChisel(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

            // tests initial state
            dut.io.in.valid.poke(true.B)
            dut.io.state.expect(QuantState.idle)


            // loads in data
            for (r <- 0 until p.numRows) {
                for (c <- 0 until p.numCols) {
                    dut.io.in.bits.data(r)(c).poke(data(r)(c).S)
                }
            } 

            // loads in quantization table
            for (r <- 0 until p.numRows) {
                for (c <- 0 until p.numCols) {
                    dut.io.quantTable(r)(c).poke(quantTable(r)(c).S)
                }
            }

            // should go to quant state
            dut.clock.step()
            dut.clock.step(64)
            dut.io.out.valid.expect(true.B)

            // compare expected scala out to chisel out
            val jpegEncoder = new jpegEncode(false, List.empty, 0)
            val expected = jpegEncoder.scaledQuantization(data, quantTable)
            for (r <- 0 until p.numRows) {
                for (c <- 0 until p.numCols) {
                    dut.io.out.bits(r)(c).expect(expected(r)(c).S)
                }
            }

            // returns to idle
            dut.io.state.expect(QuantState.idle)
        }
    }

    /**
      * Tests the functionality of Inverse Quantization in Chisel
      *
      * @param data Data to unQuantify
      * @param qt Quantization table to use
      */
    def doInverseQuantizationChiselTest(data: Seq[Seq[Int]], qt: Int): Unit = {
        val p = JPEGParams(8, 8, qt)
        val quantTable = p.getQuantTable
        test(new InverseQuantizationChisel(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

            // tests initial state
            dut.io.in.valid.poke(true.B)
            dut.io.state.expect(QuantState.idle)

            // loads in data
            for (r <- 0 until p.numRows) {
                for (c <- 0 until p.numCols) {
                    dut.io.in.bits.data(r)(c).poke(data(r)(c).S)
                }
            } 

            // loads in quantization table
            for (r <- 0 until p.numRows) {
                for (c <- 0 until p.numCols) {
                    dut.io.quantTable(r)(c).poke(quantTable(r)(c).S)
                }
            }

            // should go to quant state
            dut.clock.step()
            dut.io.out.valid.expect(false.B)
            dut.clock.step(64)

            // compare expected scala out to chisel out
            val jpegEncoder = new jpegEncode(false, List.empty, 0)
            val expected = jpegEncoder.inverseQuantization(data, quantTable) 
            for (r <- 0 until p.numRows) {
                for (c <- 0 until p.numCols) {
                    dut.io.out.bits(r)(c).expect(expected(r)(c).S)
                }
            }
            
            // returns to idle
            dut.io.state.expect(QuantState.idle)
        }
    }


    behavior of "QuantizationChisel"
    it should "correctly quantize scaled in1 with qt1" in {
        val data = jpeg.DCTData.scaledOut1 
        val qtChoice = 1
        doQuantizationChiselTest(data, qtChoice)
    }

    it should "correctly quantize scaled in1 with qt2" in {
        val data = jpeg.DCTData.scaledOut1 
        val qtChoice = 2
        doQuantizationChiselTest(data, qtChoice)
    }

    it should "correctly quantize scaled in2 with qt1" in {
        val data = jpeg.DCTData.scaledOut2
        val qtChoice = 1
        doQuantizationChiselTest(data, qtChoice)
    }

    it should "correctly quantize scaled in2 with qt2" in {
        val data = jpeg.DCTData.scaledOut2 
        val qtChoice = 2
        doQuantizationChiselTest(data, qtChoice)
    }

    it should "correctly quantize scaled in3 with qt1" in {
        val data = jpeg.DCTData.scaledOut3
        val qtChoice = 1
        doQuantizationChiselTest(data, qtChoice)
    }

    it should "correctly quantize scaled in3 with qt2" in {
        val data = jpeg.DCTData.scaledOut3
        val qtChoice = 2
        doQuantizationChiselTest(data, qtChoice)
    }

    behavior of "InverseQuantizationChisel"
    it should "correctly undo quantize in1 with qt1" in {
        val data = jpeg.QuantizationDecodeData.in1
        val qtChoice = 1
        doInverseQuantizationChiselTest(data, qtChoice)
    }

    it should "correctly undo quantize in1 with qt2" in {
        val data = jpeg.QuantizationDecodeData.in1
        val qtChoice = 2
        doInverseQuantizationChiselTest(data, qtChoice)
    }

    it should "correctly undo quantize in2 with qt1" in {
        val data = jpeg.QuantizationDecodeData.in2
        val qtChoice = 1
        doInverseQuantizationChiselTest(data, qtChoice)
    }

    it should "correctly undo quantize in2 with qt2" in {
        val data = jpeg.QuantizationDecodeData.in2
        val qtChoice = 2
        doInverseQuantizationChiselTest(data, qtChoice)
    }

    it should "correctly undo quantize in3 with qt1" in {
        val data = jpeg.QuantizationDecodeData.in3
        val qtChoice = 1
        doInverseQuantizationChiselTest(data, qtChoice)
    }

    it should "correctly quantize in3 with qt2" in {
        val data = jpeg.QuantizationDecodeData.in3
        val qtChoice = 1
        doInverseQuantizationChiselTest(data, qtChoice)
    }
}

