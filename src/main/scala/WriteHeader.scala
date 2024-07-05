import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.io.{File, FileOutputStream}

object WriteHeader extends App {

    val m = Map("x"->1, "y"->2)

    val tmpF = Paths.get("src/main/scala/testHeader.ct")

//    val writer = Files.newOutputStream(tmpF, StandardOpenOption.CREATE)
    val writer = new FileOutputStream(new File("src/main/scala/testHeader.ct"))
    println(f"L: ${m.toSeq.mkString("\n").getBytes.length}")
    println('鲜'.toInt.toChar)
    writer.write(m.toSeq.mkString("\n").getBytes)
    writer.write("\n".getBytes)
    writer.write(Array(Integer.parseInt("1001110010011100", 2).toByte))
    println(f"L2: ${Integer.parseInt("1001110010011100", 2).toChar}")
    println(Integer.parseInt("1001110010011100", 2).toChar.toInt.toBinaryString)
    writer.close()

    val reader = Files.newInputStream(tmpF, StandardOpenOption.READ)

    val s: Array[Byte] = reader.readAllBytes()

    val strings = s.reverse.take(2).reverse

    println(((strings(0)) | strings(1)).toChar)
//
//    println(s.last.toInt.toBinaryString)
//    println(s.map(_.toChar).reverse.take(2).reverse.mkString.toInt.toBinaryString)

}
