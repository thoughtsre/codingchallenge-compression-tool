import org.scalatest.flatspec.AsyncFlatSpec
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import lib.Utils.*
import lib.Preprocess.*
import lib.HuffmanTree.*

class HuffmanTreeTestSuite extends AsyncFlatSpec with AsyncIOSpec with Matchers {
    val testFileName = "src/test/resources/testText.txt"
    val testFileName2 = "testText2.txt"

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

            initializeHuffmanLeafNodes(m) should contain theSameElementsInOrderAs ans
        })
    }

    "Given a list of bit characters and a Huffman tree" should "result in the correct string" in {
        val tree = HuffmanNode(HuffmanLeaf('d', 2), HuffmanLeaf('e', 2), 4)

        val decodedStr = decodeBits(tree, tree, "0110".toList)

        assert(decodedStr === "deed")
    }
}