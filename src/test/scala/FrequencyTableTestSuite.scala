import scala.io.Source
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import lib.Utils.*
import lib.Preprocess.*
import lib.HuffmanTree.*

class FrequencyTableTestSuite extends AsyncFlatSpec with AsyncIOSpec with Matchers {

    val testFileName = "src/test/resources/testText.txt"
    val testFreqTable = Map(
        'a' -> 4,
        'B' -> 2,
        'c' -> 2,
        'C' -> 2,
        '!' -> 3,
        '\n' -> 4,
        '.' -> 1
    )

    "A frequency table" should "have the correct character counts given a file" in {
        calculateFrequencyTable(testFileName).asserting(m => m should contain theSameElementsAs(testFreqTable))
    }
}