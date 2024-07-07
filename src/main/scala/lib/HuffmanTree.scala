package lib

import scala.annotation.tailrec

object HuffmanTree {

    trait HuffmanTreeBaseNode

    case class HuffmanLeaf(char: Char, weight: Int) extends HuffmanTreeBaseNode

    case class HuffmanNode(left: HuffmanTreeBaseNode, right: HuffmanTreeBaseNode, weight: Int) extends HuffmanTreeBaseNode

    def mergeNodes[A <: HuffmanTreeBaseNode, B <: HuffmanTreeBaseNode](l:A, r:B): HuffmanNode = l match {
        case lLeaf: HuffmanLeaf => r match {
            case rLeaf: HuffmanLeaf => HuffmanNode(lLeaf, rLeaf, lLeaf.weight + rLeaf.weight)
            case rNode: HuffmanNode => HuffmanNode(lLeaf, rNode, lLeaf.weight + rNode.weight)
        }
        case lNode: HuffmanNode => r match {
            case rLeaf: HuffmanLeaf => HuffmanNode(lNode, rLeaf, lNode.weight + rLeaf.weight)
            case rNode: HuffmanNode => HuffmanNode(lNode, rNode, lNode.weight + rNode.weight)
        }
    }

    given nodeOrdering: Ordering[HuffmanTreeBaseNode] with {

        def _compare(lWeight: Int, lChar: Option[Char], rWeight: Int, rChar: Option[Char]) = {
            lWeight.compare(rWeight) match {
                case 0 => lChar match {
                    case None => 0
                    case Some(lc) => rChar match {
                        case None => 0
                        case Some(rc) => lc.compare(rc)
                    }
                }
                case c => c
            }
        }

        override def compare(leftNode: HuffmanTreeBaseNode, rightNode: HuffmanTreeBaseNode) = leftNode match {
            case lLeaf: HuffmanLeaf => {
                rightNode match {
                    case rLeaf: HuffmanLeaf => _compare(lLeaf.weight, Some(lLeaf.char), rLeaf.weight, Some(rLeaf.char))
                    case rNode: HuffmanNode => _compare(lLeaf.weight, Some(lLeaf.char), rNode.weight, None)
                }
            }
            case lNode: HuffmanNode => {
                rightNode match {
                    case rLeaf: HuffmanLeaf => _compare(lNode.weight, None, rLeaf.weight, Some(rLeaf.char))
                    case rNode: HuffmanNode => _compare(lNode.weight, None, rNode.weight, None)
                }
            }
        }
    }

    val initializeHuffmanLeafNodes: Map[Char, Int] => List[HuffmanLeaf] = m => {
        m.toList.map((c, i) => HuffmanLeaf(c, i)).sorted
    }

    @tailrec
    def generateHuffmanTree(nodeList: List[HuffmanTreeBaseNode]): Either[Throwable, HuffmanTreeBaseNode] = nodeList match {
        case l if l.length == 1 => Right(l.head)
        case l if l.length > 1 => {
            val firstTwo = l.take(2)
            val rest = l.drop(2)
            val mergedNode = mergeNodes(firstTwo.head, firstTwo.last)

            generateHuffmanTree((mergedNode :: rest).sorted)
        }
        case _ => Left(RuntimeException("List is empty!"))
    }

    def generatePrefixTable(root: HuffmanTreeBaseNode, currentPath: String = ""): Map[Char, String] = {
        root match {
            case leaf: HuffmanLeaf => List((leaf.char, currentPath)).toMap
            case node: HuffmanNode => {
                generatePrefixTable(node.left, currentPath + "0") ++ generatePrefixTable(node.right, currentPath + "1")
            }
        }
    }

    @tailrec
    def decodeBits(
                      root: HuffmanNode,
                      startNode: HuffmanTreeBaseNode,
                      bits: List[Char],
                      curString: String = ""
                  ): String = {

        bits match {
            case (h: Char) :: t => h match {
                case '0' => {
                    startNode match {
                        case leaf: HuffmanLeaf => decodeBits(root, root, bits, curString)
                        case branch: HuffmanNode => branch.left match {
                            case lLeaf: HuffmanLeaf => decodeBits(root, lLeaf, t, curString + lLeaf.char)
                            case lBranch: HuffmanNode => decodeBits(root, lBranch, t, curString)
                        }
                    }
                }
                case '1' => {
                    startNode match {
                        case leaf: HuffmanLeaf => decodeBits(root, root, bits, curString)
                        case branch: HuffmanNode => branch.right match {
                            case rLeaf: HuffmanLeaf => decodeBits(root, rLeaf, t, curString + rLeaf.char)
                            case rBranch: HuffmanNode => decodeBits(root, rBranch, t, curString)
                        }
                    }
                }
            }
            case Nil => curString
        }
    }
}
