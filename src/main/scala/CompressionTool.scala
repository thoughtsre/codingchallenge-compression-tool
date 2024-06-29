import scala.io.{BufferedSource, Source}
import cats.effect.{IO, IOApp, Resource}

object CompressionTool extends IOApp.Simple {

    val fileName = "testText.txt"

    val initFileResource: String => Resource[IO, BufferedSource] = fileName =>
        Resource.make(IO(Source.fromResource(fileName)))(resource => IO(resource.close()))

    val calculateFrequencyTable: String => IO[Map[Char, Int]] = fileName => {
        initFileResource(fileName).use( file => {
            IO(file.toList.groupBy(identity).view.mapValues(chars => chars.length).toMap)
        })
    }

    val testCount: String => IO[Unit] = fileName =>
        calculateFrequencyTable(fileName).flatMap(m => IO.println(f"Count of 'X': ${m('X')}\nCount of 't': ${m('t')}"))

    object HuffmanTree {

        trait HuffmanTreeBaseNode

        case class HuffmanLeaf(char: Char, weight: Int)

        case class HuffmanNode(left: HuffmanTreeBaseNode, right: HuffmanTreeBaseNode, weight: Int)

    }

    override def run: IO[Unit] = calculateFrequencyTable(fileName).map(m => m.toSeq.sortBy(_(1))).flatMap(IO.println)

//    val freq: Map[Char, Int] = Map.empty[Char, Int]
//
//    val freqTable: Map[Char, Int] = file.toList.groupBy(identity).view.mapValues(chars => chars.length).toMap
//
//    println(freqTable('X'))
}