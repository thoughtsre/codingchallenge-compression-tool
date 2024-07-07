import scala.io.{BufferedSource, Source}
import cats.effect.{IO, IOApp, Resource}

import java.io.{File, FileInputStream, FileOutputStream}
import lib.Utils.*
import lib.Preprocess.*
import lib.HuffmanTree.*
import lib.Compression.compress
import lib.Decompression.*
import lib.Decompression.*

import scala.annotation.tailrec

object CompressionTool extends IOApp.Simple {

    val fileName = "src/main/resources/testText.txt"
    val outFileName = "src/main/resources/compressedText.ct"

    val decodeFileResource = ""


    def decompress() = for {
        (huffmanCode, freqTable) <- readCompressedFile(outFileName)
        _ <- IO.println(freqTable)
        _ <- IO.println(huffmanCode)
        tree <- generateFileHuffmanTree(freqTable)
        content <- retrieveChars(huffmanCode, tree)
        _ <- IO.println(content)
    } yield ()

    val doCompress = false

    override def run: IO[Unit] = if doCompress then compress(fileName, outFileName) else decompress()

//    override def run: IO[Unit] = calculateFrequencyTable(fileName)
//        .map(m => initializeHuffmanLeafNodes(m))
//        .map(r => generateHuffmanTree(r) match {
//            case Right(t) => generatePrefixTable(t, "")
//        })
//        .flatMap(m => IO.println(f"Length: ${m.toList.length}") >> IO.println(m))

//    override def run: IO[Unit] = calculateFrequencyTable(fileName)
//        .map(m => initializeHuffmanLeafNodes(m))
//        .map(r => generateHuffmanTree(r))
//        .flatMap(t => t match {
//            case Right(tree) => tree match {
//                case root: HuffmanNode => IO.println(root.left) >> IO.println(root.right)
//                case _ => IO.println("Nothing found")
//            }
//            case Left(err) => IO.println("Error")
//        })

}