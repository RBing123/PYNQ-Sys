package jpeg

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import java.beans.beancontext.BeanContextChildSupport

class ZigZagModelTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "ZigZagModel"
    it should "Zig Zag 2x2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.zigzagParse(ZigZagParseData.in2x2) == ZigZagParseData.out2x2)
    }

    it should "Zig Zag 3x3" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.zigzagParse(ZigZagParseData.in3x3) == ZigZagParseData.out3x3)
    }

    it should "Zig Zag 4x4" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.zigzagParse(ZigZagParseData.in4x4) == ZigZagParseData.out4x4)
    }

    it should "Zig Zag 8x8" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.zigzagParse(ZigZagParseData.in8x8) == ZigZagParseData.out8x8)
    }

    behavior of "InverseZigZagModel"
    it should "Produce out 2x2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.zigzagDecode(ZigZagParseData.out2x2) == ZigZagParseData.in2x2)
    }

    it should "Produce out 3x3" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.zigzagDecode(ZigZagParseData.out3x3) == ZigZagParseData.in3x3)
    }

    it should "Produce out 4x4" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.zigzagDecode(ZigZagParseData.out4x4) == ZigZagParseData.in4x4)
    }

    it should "Produce out 8x8" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.zigzagDecode(ZigZagParseData.out8x8) == ZigZagParseData.in8x8)
    }
}


class RLEModelEncodeTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "RLEModelEncode"
    it should "RLE test 1" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.RLE(RLEData.in1) == RLEData.out1)
    }

    it should "RLE test 2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.RLE(RLEData.in2) == RLEData.out2)
    }

    it should "RLE test 3" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.RLE(RLEData.in3) == RLEData.out3)
    }

    it should "RLE test no dupes" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.RLE(RLEData.in4) == RLEData.out4)
    }

    behavior of "RLEModelDecode"
    it should "decode RLE test 1" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.decodeRLE(RLEData.out1) == RLEData.in1)
    }

    it should "decode RLE test 2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.decodeRLE(RLEData.out2) == RLEData.in2)
    }

    it should "decode RLE test 3" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.decodeRLE(RLEData.out3) == RLEData.in3)
    }

    it should "decode RLE test no dupes" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.decodeRLE(RLEData.out4) == RLEData.in4)
    }
}

class DeltaModelTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "DeltaModelEncode"
    it should "delta test 1" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.delta(deltaData.in1) == deltaData.out1)
    }

    it should "delta test 2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.delta(deltaData.in2) == deltaData.out2)
    }

    it should "delta test 3" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.delta(deltaData.in3) == deltaData.out3)
    }

    it should "delta test empty" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.delta(deltaData.in4) == deltaData.out4)
    }

    it should "delta test single elem" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.delta(deltaData.in5) == deltaData.out5)
    }

    behavior of "DeltaModelDecode"
    it should "delta decode test 1" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.decodeDelta(deltaData.out1) == deltaData.in1)
    }

    it should "delta decode test 2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.decodeDelta(deltaData.out2) == deltaData.in2)
    }

    it should "delta decode test 3" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.decodeDelta(deltaData.out3) == deltaData.in3)
    }

    it should "delta decode test empty" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.decodeDelta(deltaData.out4) == deltaData.in4)
    }

    it should "delta decode test single elem" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.decodeDelta(deltaData.out5) == deltaData.in5)
    }

}

class QuantizationModelTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "QuantizationModel"
    it should "in1 / quant table 1" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.quantization(QuantizationData.in1, QuantizationTables.qt1) == QuantizationData.out1qt1)
    }
  
    it should "in1 / quant table 2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.quantization(QuantizationData.in1, QuantizationTables.qt2) == QuantizationData.out1qt2)
    }

    it should "in2 / quant table 1" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.quantization(QuantizationData.in2, QuantizationTables.qt1) == QuantizationData.out2qt1)
    }

    it should "in2 / quant table 2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.quantization(QuantizationData.in2, QuantizationTables.qt2) == QuantizationData.out2qt2)
    }

    it should "in3 / quant table 1" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.quantization(QuantizationData.in3, QuantizationTables.qt1) == QuantizationData.out3qt1)
    }

    it should "in3 / quant table 2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.quantization(QuantizationData.in3, QuantizationTables.qt2) == QuantizationData.out3qt2)
    }

    behavior of "InverseQuantizationModel"
    it should "in1 / qt1 * qt1" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        val data = jpegEncoder.quantization(QuantizationData.in1, QuantizationTables.qt1)
        assert(jpegEncoder.inverseQuantization(data, QuantizationTables.qt1) == QuantizationDecodeData.out1qt1)
    }
  
    it should "in1 / qt2 * qt2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        val data = jpegEncoder.quantization(QuantizationData.in1, QuantizationTables.qt2)
        assert(jpegEncoder.inverseQuantization(data, QuantizationTables.qt2) == QuantizationDecodeData.out1qt2)
    }

    it should "in2 / qt1 * qt1" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        val data = jpegEncoder.quantization(QuantizationData.in2, QuantizationTables.qt1)
        assert(jpegEncoder.inverseQuantization(data, QuantizationTables.qt1) == QuantizationDecodeData.out2qt1)
    }

    it should "in2 / qt2 * qt2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        val data = jpegEncoder.quantization(QuantizationData.in2, QuantizationTables.qt2)
        assert(jpegEncoder.inverseQuantization(data, QuantizationTables.qt2) == QuantizationDecodeData.out2qt2)
    }

    it should "in3 / qt1 * qt1" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        val data = jpegEncoder.quantization(QuantizationData.in3, QuantizationTables.qt1)
        assert(jpegEncoder.inverseQuantization(data, QuantizationTables.qt1) == QuantizationDecodeData.out3qt1)
    }

    it should "in3 / qt2 * qt2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        val data = jpegEncoder.quantization(QuantizationData.in3, QuantizationTables.qt2)
        assert(jpegEncoder.inverseQuantization(data, QuantizationTables.qt2) == QuantizationDecodeData.out3qt2)
    }
}

class DCTModelTest extends AnyFlatSpec with ChiselScalatestTester {

    it should "dct test 1" in {
        val jpegEncode = new jpegEncode(false, List.empty, 0)
        val dctResult = jpegEncode.DCT(DCTData.in1)
        val rounded = jpegEncode.roundToInt(dctResult)

        assert(rounded == DCTData.scaledOut1)
    }


    it should "dct test 2" in {
        val jpegEncode = new jpegEncode(false, List.empty, 0)
        val dctResult = jpegEncode.DCT(DCTData.in2)
        val rounded = jpegEncode.roundToInt(dctResult)
        
        assert(rounded == DCTData.scaledOut2)
    }

    it should "dct test 3" in {
        val jpegEncode = new jpegEncode(false, List.empty, 0)
        val dctResult = jpegEncode.DCT(DCTData.in3)
        val rounded = jpegEncode.roundToInt(dctResult)
        
        assert(rounded == DCTData.scaledOut3)
    }

    it should "dct test 4" in {
        val jpegEncode = new jpegEncode(false, List.empty, 0)
        val dctResult = jpegEncode.DCT(DCTData.in4)
        val rounded = jpegEncode.roundToInt(dctResult)
        
        assert(rounded == DCTData.scaledOut4)
    }

    it should "dct test 5" in {
        val jpegEncode = new jpegEncode(false, List.empty, 0)
        val dctResult = jpegEncode.DCT(DCTData.in5)
        val rounded = jpegEncode.roundToInt(dctResult)
        
        assert(rounded == DCTData.scaledOut5)
    }

    it should "dct test 6" in {
        val jpegEncode = new jpegEncode(false, List.empty, 0)
        val dctResult = jpegEncode.DCT(DCTData.in6)
        val rounded = jpegEncode.roundToInt(dctResult)
    
        assert(rounded == DCTData.scaledOut6)
    }

    it should "dct test 7" in {
        val jpegEncode = new jpegEncode(false, List.empty, 0)
        val dctResult = jpegEncode.DCT(DCTData.in7)
        val rounded = jpegEncode.roundToInt(dctResult)

        assert(rounded == DCTData.scaledOut7)
    }

    it should "dct test 8" in {
        val jpegEncode = new jpegEncode(false, List.empty, 0)
        val dctResult = jpegEncode.DCT(DCTData.in8)
        val rounded = jpegEncode.roundToInt(dctResult)

        assert(rounded == DCTData.scaledOut8)
    }
}

