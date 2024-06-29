import scala.io.{BufferedSource, Source}
import cats.effect.{IO, IOApp, Resource}

import lib.HuffmanTree.*

object CompressionTool extends IOApp.Simple {

    val fileName = "testText.txt"

    val initFileResource: String => Resource[IO, BufferedSource] = fileName =>
        Resource.make(IO(Source.fromResource(fileName)))(resource => IO(resource.close()))

    val calculateFrequencyTable: String => IO[Map[Char, Int]] = fileName => {
        initFileResource(fileName).use( file => {
            IO(file.toList.groupBy(identity).view.mapValues(chars => chars.length).toMap)
        })
    }

    override def run: IO[Unit] = calculateFrequencyTable(fileName)
        .map(m => initializeHuffmanLeafNodes(m))
        .map(r => generateHuffmanTree(r))
        .flatMap(IO.println)

}