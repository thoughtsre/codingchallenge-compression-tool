import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.io.{File, FileOutputStream, FileInputStream}

object WriteHeader extends App {

    val m = Map("x"->1, "y"->2)

    val tmpF = Paths.get("src/main/scala/testHeader.ct")

//    val writer = Files.newOutputStream(tmpF, StandardOpenOption.CREATE)
    val writer = new FileOutputStream(new File("src/main/scala/testHeader.ct"))
//    println(f"L: ${m.toSeq.mkString("\n").getBytes.length}")
//    writer.write(m.toSeq.mkString("\n").getBytes)
    val bitString = "00000000000000001000010010011100"
    println(Integer.parseInt(bitString, 2).toByte.toInt.toBinaryString)
    println(bitString.getBytes.length)
    println(bitString.sliding(8, 8).map(Integer.parseInt(_, 2).toByte).toArray.length)

    val byteArr = bitString.sliding(8, 8).map(Integer.parseInt(_, 2).toByte)

    val reconString = byteArr.map(_.toInt.toBinaryString.takeRight(8)).map(s => s.reverse.padTo(8, '0').reverse).mkString

    println(reconString)
    println(reconString == bitString)

    writer.write(("###\n" + m.toSeq.mkString("\n") + "\n###\n").map(_.toByte).toArray)
    writer.close()

    val reader = new FileInputStream(new File("src/main/scala/testHeader.ct"))

    val content: Array[Byte] = reader.readAllBytes()

    println(content.map(_.toChar).mkString)

    def pBS(bitString: List[Char]): Unit = {
        bitString match {
            case (h: Char)::t => h match {
                case '0' => {
                    println("Zero")
                    pBS(t)
                }
                case '1' => {
                    println("One")
                    pBS(t)
                }
            }
            case _ => ()
        }
    }

    pBS(bitString.toList)

    println('m'.toByte.toChar)

//    val reader = Files.newInputStream(tmpF, StandardOpenOption.READ)
//
//    val s: Array[Byte] = reader.readAllBytes()
//
//    val strings = s.reverse.take(2).reverse

//
//    println(s.last.toInt.toBinaryString)
//    println(s.map(_.toChar).reverse.take(2).reverse.mkString.toInt.toBinaryString)

}
