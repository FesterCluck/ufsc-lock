import java.awt.{Color, Font, RenderingHints}
import java.awt.image.BufferedImage
import java.io.{File, FileInputStream}
import java.util.concurrent.Executors
import java.util.zip.ZipInputStream
import javax.imageio.ImageIO

import scala.collection.immutable.VectorBuilder
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

val Width = 50
val Height = 50

val BaseAreaMinThreshold = 0.85
val KeyAreaMaxThreshold = 0.002
val KeyReplacementColor = 0

// Job queue, we're using all CPU cores
val jobs = new VectorBuilder[Future[Unit]]
implicit val executionContext = ExecutionContext.fromExecutor(
  Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors))

// read frames from the archive to speed up reading time
val zis = new ZipInputStream(new FileInputStream("input/LOCK-frames.zip"))
processFrames(0 to 50000)
zis.close()

// Wait for jobs to complete
Await.result(Future.sequence(jobs.result), Duration.Inf)
sys.exit(0)

def processFrames(range: Range): Unit = {
  val ze = zis.getNextEntry
  if (ze ne null) {
    val frameNumber = ze.getName.replace(".png", "")
    if (range.contains(frameNumber.toInt)) {
      val id = "LOCK - frame #" + frameNumber

      println(s">-- $id: Reading ...")
      val original = ImageIO.read(zis)

      jobs += Future {
        require(original.getWidth == Width)
        require(original.getHeight == Height)

        val frameInfo = findKey(original)
        println(s"--> $id: Cleaning ...")
        val cleaned = cleanFrame(original, frameInfo)

        println(s"--< $id: Writing ...")
        writeDebugFrame(id, original, frameInfo, cleaned)

        println(s"<-- $id: Done!")
      }
    }

    // recursion, no change in range
    processFrames(range)
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

  def rgb = (b << 16) + (g << 8) + r
}

object RGB {
  def apply(rgb: Int) = new RGB(rgb & 0xff, (rgb >>> 8) & 0xff, (rgb >>> 16) & 0xff)
}

case class FrameInfo(baseColor: RGB, key: Option[(RGB, Int)])

def findKey(frame: BufferedImage): FrameInfo = {
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

  FrameInfo(baseColor = baseColor, key = key)
}

def cleanFrame(original: BufferedImage, frameInfo: FrameInfo): BufferedImage = {
  val pixels = original.getRGB(0, 0, Width, Height, null, 0, Width)

  val fixedPixels = frameInfo.key match {
    case Some((key, _)) =>
      // in we have a key, place replacement color on top of those pixels, baseColor everything else
      pixels map { p =>
        if (RGB(p) == key) KeyReplacementColor else frameInfo.baseColor.rgb
      }
    case _ =>
      // in case no key was detected, fill entire screen with baseColor
      Array.fill(pixels.length)(frameInfo.baseColor.rgb)
  }

  val img = new BufferedImage(Width, Height, BufferedImage.TYPE_INT_BGR)
  img.setRGB(0, 0, Width, Height, fixedPixels, 0, Width)
  img
}

lazy val sagittarius = ImageIO.read(new File("sagittarius.png"))

// Some graphics for visual comparison
def writeDebugFrame(id: String, original: BufferedImage, frameInfo: FrameInfo, cleaned: BufferedImage): Unit = {
  val debugFrame = new BufferedImage(1200, 1000, BufferedImage.TYPE_INT_BGR)

  val g2d = debugFrame.createGraphics()
  g2d.drawImage(sagittarius, 50, 22, null)

  g2d.setColor(Color.white)
  g2d.setFont(new Font("Arial", Font.PLAIN, 35))
  g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
  g2d.drawString(id, 85, 50)

  g2d.drawString("Original", 50, 130)
  g2d.drawRect(49, 149, Width + 1, Height  + 1)
  g2d.drawImage(original, 50, 150, null)

  g2d.drawString("Original (x10)", 50, 270)
  g2d.drawRect(49, 299, 501, 501)
  g2d.drawImage(original, 50, 300, 550, 800, 0, 0, Width, Height, null)

  g2d.drawString("Cleaned", 600, 130)
  g2d.drawRect(599, 149, Width + 1, Height  + 1)
  g2d.drawImage(cleaned, 600, 150, null)

  g2d.drawString("Cleaned (x10)", 600, 270)
  g2d.drawRect(599, 299, 501, 501)
  g2d.drawImage(cleaned, 600, 300, 1100, 800, 0, 0, Width, Height, null)

  g2d.drawString("Base color: #%06X" format frameInfo.baseColor.rgb, 50, 850)

  val keyInfo = frameInfo.key map { k =>
    val cleanedPixels = cleaned.getRGB(0, 0, Width, Height, null, 0, Width)
    val positions = cleanedPixels.zipWithIndex.flatMap { case (pixel, pos) =>
      if (RGB(pixel).rgb != KeyReplacementColor) None else {
        Some("(%02d:%02d)".format(pos % Width, pos / Width))
      }
    }
    "Found keys: " + positions.mkString(", ")
  } getOrElse("No keys found!")
  g2d.drawString(keyInfo, 50, 890)

  g2d.dispose()

  ImageIO.write(debugFrame, "png", new File(s"output/$id.png"))
}
