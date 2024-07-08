package lib

import lib.Utils.*
import lib.ByteOps.*
import lib.HuffmanTree.*
import lib.Preprocess.*
import cats.effect.IO

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.collection.immutable.Map

object Decompression {

    object ByteBufferExtension {
        extension (bb: ByteBuffer) def take(n: Int): Array[Byte] = {

            val byteArr = Array.ofDim[Byte](n)
            var counter = n

            while (counter > 0) {
                byteArr(n - counter) = bb.get
                counter -= 1
            }

//            println(byteArr.length)

            byteArr
        }
    }

    import ByteBufferExtension.*

    def parseBytes(bytes: Array[Byte]): (String, Map[Char, Int]) = {
        val byteBuffer = ByteBuffer.wrap(bytes)

        @tailrec
        def go(bb: ByteBuffer, header: Map[Char, Int] = Map.empty[Char, Int]): (String, Map[Char, Int]) = {
            val key = (new String(bb.take(4), StandardCharsets.UTF_16)).toCharArray()(0)
            val value = bb.getInt

//            println(key)
//            println(value)

            if ((key, value) == headerEndChars) then {

                val curPos = bb.position()

                (bb.array().drop(curPos).map(_.toInt.toBinaryString.takeRight(8)).map(s => s.reverse.padTo(8, '0').reverse).mkString, header)

            } else {

                go(bb, header + (key -> value))

            }
        }

        go(byteBuffer)
    }

    def parseBytes2(bytes: Array[Byte]): (List[Byte], Map[Char, Int]) = {
        val byteBuffer = ByteBuffer.wrap(bytes)

        @tailrec
        def go(bb: ByteBuffer, header: Map[Char, Int] = Map.empty[Char, Int]): (List[Byte], Map[Char, Int]) = {
            val key = (new String(bb.take(4), StandardCharsets.UTF_16)).toCharArray()(0)
            val value = bb.getInt

            //            println(key)
            //            println(value)

            if ((key, value) == headerEndChars) then {

                val curPos = bb.position()

                (bb.array().drop(curPos).toList, header)

            } else {

                go(bb, header + (key -> value))

            }
        }

        go(byteBuffer)
    }

    val readCompressedFile: String => IO[(String, Map[Char, Int])] = fileName => {
        compressedFileResource(fileName).use(file => IO(parseBytes(file.readAllBytes())))
    }

    val readCompressedFile2: String => IO[(List[Byte], Map[Char, Int])] = fileName => {
        compressedFileResource(fileName).use(file => IO(parseBytes2(file.readAllBytes())))
    }

    def retrieveChars[A<: HuffmanTreeBaseNode](bitString: String, root: A): IO[String] = root match {
        case node: HuffmanNode => IO(decodeBits(node, node, bitString.toList))
        case _ => IO.raiseError(RuntimeException("The root Huffman tree should be of HuffmanNode type."))
    }

    def retrieveChars2[A <: HuffmanTreeBaseNode](bytes: List[Byte], root: A): IO[String] = root match {
        case node: HuffmanNode => IO(decodeBytes2(node, bytes))
        case _ => IO.raiseError(RuntimeException("The root Huffman tree should be of HuffmanNode type."))
    }

    val writeDecodedFile: (String, String) => IO[Unit] = (outputFileName, content) => {
        decompressedFileResource(outputFileName).use(file => IO(file.write(content)))
    }

    val decompress: (String, String) => IO[Unit] = (compressedFileName, outputFileName) => for {
        _ <- IO.println("Begin decompression...")
        (huffmanCode, freqTable) <- readCompressedFile2(compressedFileName)
        - <- IO.println("Compressed file read...")
        tree <- generateFileHuffmanTree(freqTable)
        _ <- IO.println("Huffman tree generated...")
        content <- retrieveChars2(huffmanCode, tree)
        _ <- IO.println("Text decoded...")
        _ <- writeDecodedFile(outputFileName, content)
        _ <- IO.println("File decompressed!")
    } yield ()

}
