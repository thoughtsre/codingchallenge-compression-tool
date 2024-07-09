package lib

import lib.Utils.*
import lib.ByteOps.{huffmanCodeToBytes, frequencyTableToByteArr, generateHeaderEndBytes}
import lib.Preprocess.{calculateFrequencyTable, generateFileHuffmanTree, generateFilePrefixTable}
import cats.effect.IO

object Compression {

    val encodeFile: (Map[Char, String], String) => IO[String] = (prefixTable, fileName) => {

        inputFileResource(fileName).use(file => IO(file.grouped(100).map(chars => chars.map(c => prefixTable(c)).mkString).mkString))

    }

    val packTextToBytes: String => IO[Array[Byte]] = bitString => {
        IO(huffmanCodeToBytes(bitString))
    }

    val writeEncodedFile: (Array[Byte], Map[Char, Int], String) => IO[Unit] = (textBytes, header, outFileName) => {
        outFileResource(outFileName).use(writer => {

            val headerBytes = frequencyTableToByteArr(header)
            val headerEndBytes = generateHeaderEndBytes()

            IO(writer.write(headerBytes ++ headerEndBytes ++ textBytes))

        })
    }

    val compress: (String, String) => IO[Unit] = (fileName, outFileName) => for {
        _ <- IO.println("Begin compression...")
        freqTable <- calculateFrequencyTable(fileName)
        _ <- IO.println("Frequency table computed...")
        root <- generateFileHuffmanTree(freqTable)
        _ <- IO.println("Huffman tree generated...")
        prefix <- generateFilePrefixTable(root)
        _ <- IO.println("Prefix table generated...")
        encodedText <- encodeFile(prefix, fileName)
        - <- IO.println("Text encoded....")
        byteText <- packTextToBytes(encodedText)
        _ <- IO.println(f"Estimated compressed file size: ${byteText.length / 1e6}MB")
        _ <- writeEncodedFile(byteText, freqTable, outFileName)
        _ <- IO.println("Compressed file written!")
    } yield ()

}
