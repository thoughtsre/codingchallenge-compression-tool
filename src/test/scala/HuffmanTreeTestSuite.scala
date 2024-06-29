import org.scalatest.flatspec.AsyncFlatSpec
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import CompressionTool.calculateFrequencyTable
import lib.HuffmanTree.*

class HuffmanTreeTestSuite extends AsyncFlatSpec with AsyncIOSpec with Matchers {
    val testFileName = "testText.txt"

    val leaf1: HuffmanLeaf = HuffmanLeaf('X', 3)
    val leaf2: HuffmanLeaf = HuffmanLeaf('Y', 2)
    val leaf3: HuffmanLeaf = HuffmanLeaf('Z', 1)
    val node: HuffmanNode = HuffmanNode(leaf2, leaf3, 3)

    "A list of Huffman binary tree nodes" should "be able to be sorted" in {
        val nodeList = List(node, leaf2, leaf3, leaf1).sorted

        nodeList should contain theSameElementsInOrderAs List(leaf3, leaf2, node, leaf1)
    }

    "A list of sorted Huffman binary tree leaf nodes" should "result from frequency table initialisation" in {
        calculateFrequencyTable(testFileName).asserting( m => {

            val ans = List(
                HuffmanLeaf('a', 4),
                HuffmanLeaf('B', 2),
                HuffmanLeaf('c', 2),
                HuffmanLeaf('C', 2),
                HuffmanLeaf('!', 3),
                HuffmanLeaf('\n', 4),
                HuffmanLeaf('.', 1)
            ).sorted

            println(ans)

            initializeHuffmanLeafNodes(m) should contain theSameElementsInOrderAs ans
        })
    }
}