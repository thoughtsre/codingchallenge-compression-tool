import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.math.BigInt

val bigIntArr = BigInt(12398325).toByteArray
val smallIntArr = BigInt(2).toByteArray.reverse.padTo(4, 0.toByte).reverse

val byteArr = Array(0.toByte, 'a'.toByte) ++ bigIntArr ++ Array(0.toByte, 'b'.toByte) ++ smallIntArr ++ smallIntArr ++ Array(0.toByte, 'a'.toByte) ++ Array(0.toByte, 'a'.toByte) ++ Array(0.toByte, 'a'.toByte)

val byteBuffer = ByteBuffer.wrap(byteArr)

val firstChar = byteBuffer.getChar
val firstInt = byteBuffer.getInt

val secondChar = byteBuffer.getChar
val secondInt = byteBuffer.getInt

val thirdInt = byteBuffer.getInt

val chars = byteBuffer.asCharBuffer().toString

val pos = byteBuffer.position()
val L = byteArr.length - pos
val remaining = byteArr.drop(pos)

val b = 10010001.toByte

val bs = b.toInt.toBinaryString

val sdf = (new String('䕖'.toString.getBytes("UTF-16"), StandardCharsets.UTF_16)).charAt(0)

val abc = new String('a'.toString.getBytes("UTF-16"), StandardCharsets.UTF_16)

val aa = '1'.toString.getBytes("UTF-16")

val sdfbb = ByteBuffer.allocate(10)

sdfbb.put(sdf)

sdfbb.rewind()

println(sdfbb.getChar())

byteBuffer.put(sdf)

'"'.toString.getBytes("UTF-8")

103.toByte.toInt.toBinaryString.takeRight(8).reverse.padTo(8, "0").reverse.mkString

100.toBinaryString

byteBuffer.array
byteBuffer.array.drop(byteBuffer.position())