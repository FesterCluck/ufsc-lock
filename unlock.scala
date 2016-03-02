import java.util.zip.ZipInputStream
import java.io.FileInputStream
import javax.imageio.ImageIO

val Width = 50
val Height = 50

// read frames from the archive to speed up reading time
val zis = new ZipInputStream(new FileInputStream("input/♐LOCK-frames.zip"))
readFrames(0 to 100)
zis.close()

def readFrames(range: Range): Unit = {
  val ze = zis.getNextEntry
  if (ze ne null) {
    val frameNumber = ze.getName.replace(".png", "")
    if (range.contains(frameNumber.toInt)) {
      val id = "♐LOCK - frame #" + frameNumber

      println(s"$id: Reading ...")
      val frame = ImageIO.read(zis)
    }

    // recursion, no change in range
    readFrames(range)
  }
}

sys.exit(0)
