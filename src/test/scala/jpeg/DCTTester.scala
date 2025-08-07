package jpeg

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.math.round

/**
  * Test data used for verifying parts of Chisel DCT Implementation 
  */
object DCTDataChisel {
  // Resulting matrix of DCTData.in1 after normalization shift of -128
  val shifted = Seq(
    Seq(-66.S, -73.S, -73.S, -74.S, -79.S, -80.S, -81.S, -73.S),
    Seq(-66.S, -71.S, -74.S, -76.S, -80.S, -81.S, -80.S, -75.S),
    Seq(-67.S, -68.S, -76.S, -79.S, -80.S, -81.S, -79.S, -74.S),
    Seq(-65.S, -67.S, -68.S, -68.S, -65.S, -63.S, -60.S, -63.S),
    Seq(-61.S, -61.S, -58.S, -54.S, -49.S, -43.S, -37.S, -36.S),
    Seq(-46.S, -33.S, -27.S, -22.S, -14.S, -13.S, -16.S, -11.S),
    Seq(-32.S, -17.S, -13.S, -9.S, 0.S, 0.S, 2.S, -1.S),
    Seq(-19.S, -7.S, -1.S, 5.S, 11.S, 13.S, 12.S, 5.S))

  val shiftedInt = Seq(
    Seq(-66, -73, -73, -74, -79, -80, -81, -73),
    Seq(-66, -71, -74, -76, -80, -81, -80, -75),
    Seq(-67, -68, -76, -79, -80, -81, -79, -74),
    Seq(-65, -67, -68, -68, -65, -63, -60, -63),
    Seq(-61, -61, -58, -54, -49, -43, -37, -36),
    Seq(-46, -33, -27, -22, -14, -13, -16, -11),
    Seq(-32, -17, -13, -9, 0, 0, 2, -1),
    Seq(-19, -7, -1, 5, 11, 13, 12, 5))

  val dctOutput = Seq(
    Seq(-369.63, -29.67, -2.64, -2.47, -1.13, -3.71, -1.48, -0.08),
    Seq(-231.08, 44.92, 24.49, -0.27, 9.3, 3.91, 4.29, -1.35),
    Seq(62.85, 8.53, -7.58, -2.66, 0.32, -0.41, 0.51, -0.83),
    Seq(12.5, -14.61, -3.48, -3.44, 2.43, -1.33, 2.72, -0.38),
    Seq(-4.88, -3.86, 0.87, 3.56, 0.13, 5.12, 1.13, 0.48),
    Seq(-0.48, 3.19, -1.43, 0.2, -1.06, -1.48, -1.13, 0.9),
    Seq(4.41, 2.28, -1.74, -1.57, 1.09, -2.74, 1.08, -1.41),
    Seq(-10.19, -1.82, 5.91, -0.42, 0.3, 0.42, -0.98, 0.0))
}


/**
  * Class to hold DCT tests and function
  */
class DCTChiselTest extends AnyFlatSpec with ChiselScalatestTester {
  /**
    * Performs DCT Test
    *
    * @param data Input matrix to test DCT implementation on
    */
  def doDCTChiselTest(data: Seq[Seq[Int]]): Unit = {
    test(new DCTChisel) { dut =>
      // Compute DCT in Scala for comparison
      val jpegEncoder = new jpegEncode(false, List.empty, 0)
      val expectedDCT = jpegEncoder.DCT(data)
      val convertedMatrix: Seq[Seq[SInt]] = expectedDCT.map(row => row.map(value => value.toInt.S))

      // Set input valid/ready bits
      dut.io.in.valid.poke(true.B)
      // dut.io.in.ready.expect(true.B)
      // Load input matrix
      for (i <- 0 until 8) {
        for (j <- 0 until 8) {
          dut.io.in.bits.matrixIn(i)(j).poke(data(i)(j))
        }
      }

      // Take step to load in matrix
      dut.clock.step()

      // Take step to load shifted block/go to calc state
      dut.clock.step()

      // Take step to go to waiting/load calculation
      dut.clock.step()

      // Check DCT output
      dut.io.dctOut.valid.expect(true.B)
      for (i <- 0 until 8) {
        for (j <- 0 until 8) {
          dut.io.dctOut.bits(i)(j).expect(convertedMatrix(i)(j))
        }
      }
    }
  }

  behavior of "DCTChisel"
  it should "compute DCT correctly 1 (Baseline & Shifted Block)" in {
    test(new DCTChisel) { dut =>
      // Define input and special test for shifted matrix
      val inputMatrix = DCTData.in1
      val shiftedBlock = DCTDataChisel.shifted
      
      // Use model to create expected valid output
      val jpegEncoder = new jpegEncode(false, List.empty, 0)
      val dctOut = jpegEncoder.DCT(inputMatrix)
      // DCT Scala model returns values with precision, performing conversion to Int for Chisel
      val convertedMatrix: Seq[Seq[SInt]] = dctOut.map(row => row.map(value => value.toInt.S))


      dut.io.in.valid.poke(true.B)
      // Load in input matrix
      for (i <- 0 until 8) {
        for (j <- 0 until 8) {
          dut.io.in.bits.matrixIn(i)(j).poke(inputMatrix(i)(j))
        }
      }

      // Take step to load in input matrix
      dut.clock.step()

      // Take step to load shifted block/go to calc state
      dut.clock.step()

      // Compare DUT ShiftedBlock with expected out
      for (i <- 0 until 8) {
        for (j <- 0 until 8) {
          dut.io.shiftedOut(i)(j).expect(shiftedBlock(i)(j))
        }
      }

      // Take step to go to waiting/load calculation
      dut.clock.step()
      dut.io.dctOut.valid.expect(true.B)
      for (i <- 0 until 8) {
        for (j <- 0 until 8) {
          dut.io.dctOut.bits(i)(j).expect(convertedMatrix(i)(j))
        }
      }
    }
  }

  it should "compute DCT correctly in2" in {
    val inputMatrix = DCTData.in2
    doDCTChiselTest(inputMatrix)
  }

  it should "compute DCT correctly in3" in {
    val inputMatrix = DCTData.in3
    doDCTChiselTest(inputMatrix)
  }

  it should "compute DCT correctly in4" in {
    val inputMatrix = DCTData.in4
    doDCTChiselTest(inputMatrix)
  }

  it should "compute DCT correctly in5" in {
    val inputMatrix = DCTData.in5
    doDCTChiselTest(inputMatrix)
  }

  it should "compute DCT correctly in6" in {
    val inputMatrix = DCTData.in6
    doDCTChiselTest(inputMatrix)
  }

  it should "compute DCT correctly in7" in {
    val inputMatrix = DCTData.in7
    doDCTChiselTest(inputMatrix)
  }

  it should "compute DCT correctly in8" in {
    val inputMatrix = DCTData.in8
    doDCTChiselTest(inputMatrix)
  }
}
