import java.awt.image.BufferedImage
import java.util.zip.ZipInputStream
import java.io.FileInputStream
import javax.imageio.ImageIO

val Width = 50
val Height = 50

val BaseAreaMinThreshold = 0.85
val KeyAreaMaxThreshold = 0.002

// read frames from the archive to speed up reading time
val zis = new ZipInputStream(new FileInputStream("input/♐LOCK-frames.zip"))
readFrames(7200 to 7220)
zis.close()
sys.exit(0)

def readFrames(range: Range): Unit = {
  val ze = zis.getNextEntry
  if (ze ne null) {
    val frameNumber = ze.getName.replace(".png", "")
    if (range.contains(frameNumber.toInt)) {
      val id = "♐LOCK - frame #" + frameNumber

      println(s"$id: Reading ...")
      val frame = ImageIO.read(zis)
      // sanity check on frame dimensions
      require(frame.getWidth == Width && frame.getHeight == Height)

      println(s"$id: Processing frame ...")
      val keyInfo = processFrame(frame)
      println(s"$id: $keyInfo")
    }

    // recursion, no change in range
    readFrames(range)
  }
}

// Some util classes
case class RGB private(r: Int, g: Int, b: Int) {
  def distance(other: RGB) = {
    val dr = other.r - r
    val dg = other.g - g
    val db = other.b - b
    dr * dr + dg * dg + db * db
  }
}

object RGB {
  def apply(rgb: Int) = new RGB(rgb & 0xff, (rgb >>> 8) & 0xff, (rgb >>> 16) & 0xff)
}

def processFrame(frame: BufferedImage): (RGB, Option[(RGB, Int)]) = {
  val pixels = frame.getRGB(0, 0, Width, Height, null, 0, Width)
  val colors = pixels.groupBy(RGB(_)).mapValues(_.size)

  val baseColor = {
    val (color, occurrences) = colors.maxBy(_._2)
    require(occurrences > BaseAreaMinThreshold * Width * Height)
    color
  }

  val keyCandidates = {
    val others = colors - baseColor
    others filter { case (color, occurrences) =>
      occurrences < KeyAreaMaxThreshold * Width * Height
    }
  }

  val key =
    if (keyCandidates.nonEmpty) {
      Some(keyCandidates.maxBy(_._1.distance(baseColor)))
    } else {
      None
    }

  baseColor -> key
}
