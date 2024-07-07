package lib

import lib.Utils.*
import lib.ByteOps.*
import lib.HuffmanTree.*
import cats.effect.IO
import java.nio.ByteBuffer
import scala.annotation.tailrec

object Decompression {

    def parseBytes(bytes: Array[Byte]): (String, Map[Char, Int]) = {
        val byteBuffer = ByteBuffer.wrap(bytes)

        @tailrec
        def go(bb: ByteBuffer, header: Map[Char, Int] = Map.empty[Char, Int]): (String, Map[Char, Int]) = {
            val key = bb.getChar
            val value = bb.getInt

            if ((key, value) == headerEndChars) then {
                val curPos = bb.position()

                (bb.asCharBuffer().toString, header)

            } else {

                go(bb, header + (key -> value))

            }
        }

        go(byteBuffer)
    }

    val readCompressedFile: String => IO[(String, Map[Char, Int])] = fileName => {
        compressedFileResource(fileName).use(file => IO(parseBytes(file.readAllBytes())))
    }

    def retrieveChars[A<: HuffmanTreeBaseNode](bitString: String, root: A): IO[String] = root match {
        case node: HuffmanNode => IO(decodeBits(node, node, bitString.toList))
        case _ => IO.raiseError(RuntimeException("The root Huffman tree should be of HuffmanNode type."))
    }

}
