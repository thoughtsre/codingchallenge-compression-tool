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

    val fileName = "src/main/resources/lesmiserables.txt"
    val compressFileName = "src/main/resources/compressedText.ct"
    val outFileName = "src/main/resources/decompressed.txt"

    val doCompress = false

    override def run: IO[Unit] = if doCompress then compress(fileName, compressFileName) else decompress(compressFileName, outFileName)

}