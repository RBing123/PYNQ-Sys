package jpeg

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import java.beans.beancontext.BeanContextChildSupport

/**
  * Class to hold ZigZag Test functions
  */
class ZigZagChiselTest extends AnyFlatSpec with ChiselScalatestTester {
    /**
      * Performs ZigZag encoding test
      *
      * @param data Input matrix to parse
      */
    def doZigZagChiselTest(data: Seq[Seq[Int]]): Unit = {
        val p = JPEGParams(8, 8, 0)
        test(new ZigZagChisel(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            // Start in idle state and prepare to load in data
            dut.io.in.valid.poke(true.B)
            dut.io.state.expect(ZigZagState.idle)

            // Input matix 
            for (r <- 0 until p.numRows) {
                for (c <- 0 until p.numCols) {
                    dut.io.in.bits.matrixIn(r)(c).poke(data(r)(c).S)
                }
            }
            
            // Step to load in matrix and transition to processing state
            dut.clock.step()
            dut.io.in.valid.poke(false.B)
            dut.io.state.expect(ZigZagState.processing)
            
            // Allow parsing to complete
            dut.clock.step(p.totalElements)

            // Use Scala model to create expected output then verify
            val jpegEncoder = new jpegEncode(false, List.empty, 0)
            val expected = jpegEncoder.zigzagParse(data)
            for(i <- 0 until expected.length){
                dut.io.zigzagOut.bits(i).expect(expected(i).S)
            }
            dut.io.state.expect(ZigZagState.idle)
        }
    }

    /**
      * Performs ZigZag decoding test
      *
      * @param data Input matrix to parse
      */
    def doInverseZigZagChiselTest(data: Seq[Int]): Unit = {
        val p = JPEGParams(8, 8, 0)
        test(new InverseZigZagChisel(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            // Start in idle state and prepare to load in data
            dut.io.in.valid.poke(true.B)
            dut.io.state.expect(ZigZagState.idle)

            // Input 1d array 
            for(i <- 0 until data.length){
                dut.io.in.bits.zigzagIn(i).poke(data(i).S)
            }

            // Transition to procesing state 
            dut.clock.step()
            dut.io.in.valid.poke(false.B)
            dut.io.state.expect(ZigZagState.processing)

            // Allow parsing to complete
            dut.clock.step(p.totalElements)

            // Use Scala model to create expected output then verify
            val jpegEncoder = new jpegEncode(false, List.empty, 0)
            val expected = jpegEncoder.zigzagDecode(data)
            for (r <- 0 until p.numRows) {
                for (c <- 0 until p.numRows) {
                    dut.io.matrixOut.bits(r)(c).expect(expected(r)(c).S)
                }
            }
            dut.io.state.expect(ZigZagState.idle)
        }
    }

    behavior of "ZigZagChisel"
    it should "zig zag encode ZigZagParseData.in8x8" in {
        val test = jpeg.ZigZagParseData.in8x8
        doZigZagChiselTest(test)
    }

    it should "zig zag encode QuantizationData.in2" in {
        val test = jpeg.QuantizationData.in2
        doZigZagChiselTest(test)
    }

    it should "zig zag encode QuantizationData.in3" in {
        val test = jpeg.QuantizationData.in3
        doZigZagChiselTest(test)
    }

    behavior of "InverseZigZagChisel"
    it should "zig zag decode out8x8" in {
        val test = jpeg.ZigZagParseData.out8x8
        doInverseZigZagChiselTest(test)
    }

    it should "zig zag decode QuantizationData.in2" in {
        val test = jpeg.QuantizationData.in2.flatten
        doInverseZigZagChiselTest(test)
    }

    it should "zig zag decode QuantizationData.in3" in {
        val test = jpeg.QuantizationData.in3.flatten
        doInverseZigZagChiselTest(test)
    }
}

