import cats.effect.{IO, ExitCode}
import cats.implicits.*
import com.monovore.decline.*
import com.monovore.decline.effect.*

import lib.Compression.compress
import lib.Decompression.decompress

object CompressionTool extends CommandIOApp(
    name = "Compression Tool",
    header = "Compression Tool",
    version = "0.1.0"
) {

    override def main: Opts[IO[ExitCode]] = {
        val doCompress = Opts.flag("compress", short = "c", help = "Compress file.").orFalse
        val doDecompress = Opts.flag("decompress", short = "d", help = "Decompress file").orFalse
        val inputFile = Opts.argument[String](metavar = "inputFileName")
        val outputFile = Opts.argument[String](metavar = "outputFileName")

        (doCompress, doDecompress, inputFile, outputFile).mapN((c, d, inFileName, outFileName) => {
            if (c & d) then {
                IO.raiseError(new RuntimeException("Compress and decompress flags cannot both be true!")) >> IO(ExitCode(1))
            } else if (!c & !d) then {
                IO.raiseError(new RuntimeException("Compress and decompress flags cannot both be false!")) >> IO(ExitCode(1))
            } else {
                if c then {
                    compress(inFileName, outFileName) >> IO(ExitCode(0))
                } else {
                    decompress(inFileName, outFileName) >> IO(ExitCode(0))
                }
            }
        })
    }

}