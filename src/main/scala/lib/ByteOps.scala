package lib

import scala.math.BigInt

object ByteOps {

    private object valueToByteExtensions {
        extension (integer: Int) def toByteArr: Array[Byte] = BigInt(integer).toByteArray.reverse.padTo(4, 0.toByte).reverse
        extension (char: Char) def toByteArr: Array[Byte] = char.toString.getBytes("UTF-16")
    }

    import valueToByteExtensions.*

    val huffmanCodeToBytes: String => Array[Byte] = bitString => {
        bitString.grouped(8).map(s => Integer.parseInt(s, 2).toByte).toArray
    }

    val frequencyTableToByteArr: Map[Char, Int] => Array[Byte] = freqTable => {
        freqTable.toList.map((c, i) => c.toByteArr ++ i.toByteArr).flatten.toArray
    }

    val headerEndChars = ('0', 0)

    val generateHeaderEndBytes: () => Array[Byte] = () => '0'.toByteArr ++ 0.toByteArr

}
