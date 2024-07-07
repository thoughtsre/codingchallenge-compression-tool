import java.nio.ByteBuffer
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

s -> 2,
-> 2, n -> 3, y -> 3, t -> 3, u -> 1, ! -> 1, a -> 6, m -> 1,   -> 8, ? -> 1, k -> 1, I -> 1, i -> 2, g -> 1, l -> 1, W -> 1, h -> 1, o -> 3, d -> 2