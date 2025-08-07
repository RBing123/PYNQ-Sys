package jpeg

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.experimental

/**
  * Class to test Decoding functionality
  */
class DecodingChiselTest extends AnyFlatSpec with ChiselScalatestTester {
    /**
      * Performs RlE Decoding Test
      *
      * @param data Data to Decode
      * @param length Length of used indices for data since we pad wiht 0s
      */
    def doRLEChiselDecodeTest(data: Seq[Int], length: Int): Unit = {
        val p = JPEGParams(8, 8, 0)
        test(new RLEChiselDecode(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

            // Grab the frequencies
            val freq = data.zipWithIndex.filter { case (_, index) => index % 2 == 0 }.map(_._1)

            // tests initial state
            dut.io.in.valid.poke(true.B)
            dut.io.length.poke(length.asUInt)
            dut.io.state.expect(RLEDecodingState.idle)
            
            // load data in
            for (i <- 0 until length) {
                dut.io.in.bits.data(i).poke(data(i).S)
            }

            // should be in load state
            dut.clock.step()
            dut.io.state.expect(RLEDecodingState.load)

            // goes between load and decode and stays in decode for freq(i)
            for (i <- 0 until (length / 2)) {
                dut.clock.step()
                dut.io.state.expect(RLEDecodingState.decode)

                dut.clock.step(freq(i))
                dut.clock.step()
                dut.io.state.expect(RLEDecodingState.load)
            }

            // done decoding valid out and back to idle
            dut.io.in.valid.poke(false.B)
            dut.clock.step()
            dut.io.out.valid.expect(true.B)
            dut.io.state.expect(RLEDecodingState.idle)            
            
            // compares output to expected
            val jpegEncoder = new jpegEncode(false, List.empty, 0)
            val expected = jpegEncoder.decodeRLE(data)
            for( i <- 0 until (length / 2)){
                dut.io.out.bits(i).expect(expected(i).S)
            }
        }
    }

    /**
      * Performs Delta Decoding Tests
      *
      * @param data Data to decode
      */
    def doDeltaChiselDecodeTest(data: Seq[Int]): Unit = {
        val p = JPEGParams(8, 8, 0)
        test(new DeltaChiselDecode(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            // tests initial state
            dut.io.in.valid.poke(true.B)
            dut.io.state.expect(DecodingState.idle)

            // load data in
            for (i <- 0 until p.totalElements) {
                dut.io.in.bits.data(i).poke(data(i).S)
            }

            // should be in encoding after one cycle
            dut.clock.step()
            dut.io.state.expect(DecodingState.decode)

            // cycles number of calculations
            dut.io.in.valid.poke(false.B)
            dut.clock.step(p.totalElements)
            dut.io.out.valid.expect(true.B)
            dut.io.state.expect(DecodingState.idle)
                
            // compare with scala model
            val jpegEncoder = new jpegEncode(false, List.empty, 0)
            val expected = jpegEncoder.decodeDelta(data)
            for( i <- 0 until p.totalElements){
                dut.io.out.bits(i).expect(expected(i).S)
            }
        }
    }

    behavior of "RLEChiselDecode"
    it should "decode 3:1, 7:2, 3:3, 3:4, 9:5, 2:6, 4:7, 3:8, 23:9, 4:10" in {
        val test1 = Seq(3, 1, 7, 2, 3, 3, 3, 4, 9, 5, 2, 6, 4, 7, 3, 8, 23, 9, 4, 10)
        val sumFreq: Int = test1.zipWithIndex.collect {
            case (value, index) if index % 2 == 0 => value
        }.sum
        val test2 = Seq.fill(0)(sumFreq)
        val test = test1 ++ test2
        doRLEChiselDecodeTest(test, 20)
    }

    it should "decode 3:1, 5:2, 2:3, 6:4, 1:5, 8:6" in {
        val test1 = Seq(3, 1, 5, 2, 2, 3, 6, 4, 1, 5, 8, 6)
        val sumFreq: Int = test1.zipWithIndex.collect {
            case (value, index) if index % 2 == 0 => value
        }.sum
        val test2 = Seq.fill(0)(sumFreq)
        val test = test1 ++ test2
        doRLEChiselDecodeTest(test, 12)
    }

    it should "decode 4:1, 4:2, 4:3, 4:4, 4:5, 5:6, 5:7, 3:8, 2:9, 5:10" in {
        val test1 = Seq(4, 1, 4, 2, 4, 3, 4, 4, 4, 5, 5, 6, 5, 7, 3, 8, 2, 9, 5, 10)
        val sumFreq: Int = test1.zipWithIndex.collect {
            case (value, index) if index % 2 == 0 => value
        }.sum
        val test2 = Seq.fill(0)(sumFreq)
        val test = test1 ++ test2
        doRLEChiselDecodeTest(test, 20)
    }

    it should "decode 3:1, 5:2, 2:3, 6:4, 1:5, 8:6, 2:8, 1:10, 5:4, 7:3" in {
        val test1 = Seq(3, 1, 5, 2, 2, 3, 6, 4, 1, 5, 8, 6, 2, 8, 1, 10, 5, 4, 7, 3)
        val sumFreq: Int = test1.zipWithIndex.collect {
            case (value, index) if index % 2 == 0 => value
        }.sum
        val test2 = Seq.fill(0)(sumFreq)
        val test = test1 ++ test2
        doRLEChiselDecodeTest(test, 20)
    }

    it should "decode no dupes" in {
        val test: Seq[Int] = (1 to 64).flatMap(i => Seq(1, i))
        doRLEChiselDecodeTest(test, 128)
    }
    
    behavior of "DeltaChiselDecode"
    it should "decode 1 to 64" in {
        val test = Seq(
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
            19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 
            35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 
            51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64
        )
        doDeltaChiselDecodeTest(test)
    }
    it should "decode 64 to 1" in {
        val test = Seq(
            64, 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49,
            48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33,
            32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17,
            16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1
        )
        doDeltaChiselDecodeTest(test)
    }

    it should "decode 64 of the same int(10 and 0)" in {
        val test1 = Seq.fill(64)(10)
        doDeltaChiselDecodeTest(test1)
        val test2 = Seq.fill(64)(0)
        doDeltaChiselDecodeTest(test2)
    }

}
