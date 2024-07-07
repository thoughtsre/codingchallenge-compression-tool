package lib

import lib.Utils.*
import lib.HuffmanTree.*
import cats.effect.IO

object Preprocess {

    val calculateFrequencyTable: String => IO[Map[Char, Int]] = fileName => {
        inputFileResource(fileName).use(file => {
            IO(file.toList.groupBy(identity).view.mapValues(chars => chars.length).toMap)
        })
    }

    val generateFileHuffmanTree: Map[Char, Int] => IO[HuffmanTreeBaseNode] = freqTable => {
        val initHuffmanLeafNodes = initializeHuffmanLeafNodes(freqTable)

        generateHuffmanTree(initHuffmanLeafNodes) match {
            case Right(tree) => IO(tree)
            case Left(err) => IO.raiseError(err)
        }
    }

    val generateFilePrefixTable: HuffmanTreeBaseNode => IO[Map[Char, String]] = root => IO(generatePrefixTable(root, ""))

}
