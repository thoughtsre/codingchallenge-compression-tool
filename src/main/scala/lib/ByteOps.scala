package lib

import java.nio.ByteBuffer
import scala.math.BigInt

object ByteOps {

    private object valueToByteExtensions {
        extension (integer: Int) def toByteArr: Array[Byte] = BigInt(integer).toByteArray.reverse.padTo(4, 0.toByte).reverse
        extension (char: Char) def toByteArr: Array[Byte] = Array(0.toByte, char.toByte)
    }

    import valueToByteExtensions.*

    val huffmanCodeToBytes: String => Array[Byte] = bitString => {
        bitString.grouped(8).map(s => Integer.parseInt(s, 2).toByte).toArray
    }

    val frequencyTableToByteArr: Map[Char, Int] => Array[Byte] = freqTable => {
        // It will be in groups of 6 bytes where first 2 bytes are the key Char
        // next 4 bytes will be an Int
        freqTable.toList.map((c, i) => c.toByteArr ++ i.toByteArr).flatten.toArray
    }

    val headerEndChars = ('0', 0)

    val generateHeaderEndBytes: () => Array[Byte] = () => '0'.toByteArr ++ 0.toByteArr

}
