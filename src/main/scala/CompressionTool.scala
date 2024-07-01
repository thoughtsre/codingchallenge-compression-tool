import scala.io.{BufferedSource, Source}
import cats.effect.{IO, IOApp, Resource}
import lib.HuffmanTree.*


import java.io.{File, FileWriter}

object CompressionTool extends IOApp.Simple {

    val fileName = "src/main/resources/testText.txt"
    val outFileName = "src/main/resources/compressedText.ct"

    val initFileResource: String => Resource[IO, BufferedSource] = fileName =>
        Resource.make(IO(Source.fromFile(fileName)))(resource => IO(resource.close()))

    val outFileResource: String => Resource[IO, FileWriter] = fileName =>
        Resource.make(IO(new FileWriter(new File(fileName))))(writer => IO(writer.close()))

    val calculateFrequencyTable: String => IO[Map[Char, Int]] = fileName => {
        initFileResource(fileName).use( file => {
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

    val encodeFile: (Map[Char, String], String) => IO[String] = (prefixTable, fileName) => {

        initFileResource(fileName).use(file => IO(file.grouped(100).map(chars => chars.map(c => prefixTable(c)).mkString).mkString))

    }

    val packToBytes: String => IO[List[Int]] = bitString => {
        IO(bitString.grouped(16).map( s => Integer.parseInt(s, 2)).toList)
    }

    val writeEncodedFile: (List[Int], Map[Char, Int], String) => IO[Unit] = (textBytes, header, outFileName) => {
        outFileResource(outFileName).use( writer => {
            writer.write("###\n")

            for (item <- header.toSeq) {
                writer.write(item(0) + " " + item(1).toString + "\n")
            }

            writer.write("###\n")

            for (b <- textBytes) {
                writer.write(b.toByte)
            }

            IO.unit
        })
    }

//    override def run: IO[Unit] = (for {
//        file <- initFileResource(fileName)
//    } yield file).use({ case f =>
//        f.foreach(println)
//
//        IO.unit
//    })

    override def run: IO[Unit] = for {
        _ <- IO.println("Begin encoding...")
        freqTable <- calculateFrequencyTable(fileName)
        _ <- IO.println("Frequency table computed...")
        root <- generateFileHuffmanTree(freqTable)
        _ <- IO.println("Huffman tree generated...")
        prefix <- generateFilePrefixTable(root)
        _ <- IO.println("Prefix table generated...")
        encodedText <- encodeFile(prefix, fileName)
        - <- IO.println("Text encoded....")
        byteText <- packToBytes(encodedText)
        _ <- IO.println(byteText)
        _ <- writeEncodedFile(byteText, freqTable, outFileName)
    } yield ()

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