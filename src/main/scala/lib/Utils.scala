package lib

import cats.effect.{IO, Resource}

import scala.io.{BufferedSource, Source}
import java.io.{File, FileInputStream, FileOutputStream, FileWriter}

object Utils {

    val inputFileResource: String => Resource[IO, BufferedSource] = fileName =>
        Resource.make(IO(Source.fromFile(fileName)))(resource => IO(resource.close()))

    val outFileResource: String => Resource[IO, FileOutputStream] = fileName =>
        Resource.make(IO(new FileOutputStream(new File(fileName))))(writer => IO(writer.close()))
        
    val compressedFileResource: String => Resource[IO, FileInputStream] = fileName => 
        Resource.make(IO(new FileInputStream(new File(fileName))))(writer => IO(writer.close()))

    val decompressedFileResource: String => Resource[IO, FileWriter] = fileName =>
        Resource.make(IO(new FileWriter(new File(fileName))))(resource => IO(resource.close()))

}
