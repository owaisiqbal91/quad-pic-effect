import java.awt.Color
import java.awt.image.{BufferedImage, DataBufferByte}
import java.io.File
import javax.imageio.ImageIO
import javax.imageio.stream.FileImageOutputStream

import scala.collection.mutable


object QuadPic extends App{

  case class Quad(startX: Int, startY: Int, width: Int, height: Int, score: Double)

  implicit object QuadOrdering extends Ordering[Quad] {
    override def compare(q1: Quad, q2: Quad) = q1.score compare q2.score
  }

  val quadPriorityQueue = new mutable.PriorityQueue[Quad]

  val img = ImageIO.read(new File(args(0)))

  val pixelBytes = img.getData.getDataBuffer.asInstanceOf[DataBufferByte].getData
  val redArray = Array.ofDim[Int](img.getWidth, img.getHeight)
  val greenArray = Array.ofDim[Int](img.getWidth, img.getHeight)
  val blueArray = Array.ofDim[Int](img.getWidth, img.getHeight)

  for (y <- 0 until img.getHeight;
       x <- 0 until img.getWidth) {
    val clr = img.getRGB(x, y)
    blueArray(x)(y) = clr & 0xff
    greenArray(x)(y) = (clr >> 8) & 0xff
    redArray(x)(y) = (clr >> 16) & 0xff
  }

  val workingImage = new BufferedImage(img.getWidth, img.getHeight, BufferedImage.TYPE_INT_RGB)
  val overlay = new BufferedImage(img.getWidth, img.getHeight, BufferedImage.TYPE_INT_ARGB)
  val overlayGraphics = overlay.getGraphics
  overlayGraphics.setColor(Color.BLACK)
  val finalImg = new BufferedImage(img.getWidth, img.getHeight, BufferedImage.TYPE_INT_ARGB)
  val finalGraphics = finalImg.getGraphics

  val output = new FileImageOutputStream(new File("out//finalgif.gif"))
  val writer = new GifSequenceWriter(output, BufferedImage.TYPE_INT_ARGB, 1, false)

  quadPriorityQueue += Quad(0, 0, img.getWidth, img.getHeight, 0)
  val n = 5000
  for (i <- 0 until n) {
    val q = quadPriorityQueue.dequeue
    splitIntoQuads(q.startX, q.startY, q.width, q.height, workingImage)

    if (i % getModuloNumber(i) == 0) {
      finalGraphics.drawImage(workingImage, 0, 0, null)
      finalGraphics.drawImage(overlay, 0, 0, null)
      writer.writeToSequence(finalImg)
    }
  }

  finalGraphics.drawImage(workingImage, 0, 0, null)
  finalGraphics.drawImage(overlay, 0, 0, null)
  ImageIO.write(finalImg, "png", new File("out//finalOutput.png"))

  overlayGraphics.dispose
  finalGraphics.dispose
  writer.close
  output.close

  def splitIntoQuads(startX: Int, startY: Int, width: Int, height: Int, bufferedImage: BufferedImage) = {
    val quadW = width / 2
    val quadH = height / 2
    paintQuad(startX, startY, quadW, quadH, bufferedImage)
    paintQuad(startX + quadW, startY, quadW, quadH, bufferedImage)
    paintQuad(startX, startY + quadH, quadW, quadH, bufferedImage)
    paintQuad(startX + quadW, startY + quadH, quadW, quadH, bufferedImage)

    overlayGraphics.drawLine(startX + quadW, startY, startX + quadW, startY + height)
    overlayGraphics.drawLine(startX, startY + quadH, startX + width, startY + quadH)
  }

  def paintQuad(startX: Int, startY: Int, width: Int, height: Int, bufferedImage: BufferedImage) = {
    val (avgR, avgG, avgB) = getAverageValues(startX, startY, width, height)
    val totalValues = height * width
    val rgbArray = new Array(totalValues) map {
      x: Int => getRGB(avgR, avgG, avgB)
    }
    bufferedImage.setRGB(startX, startY, width, height, rgbArray, 0, width)

    val score = getScore(startX, startY, width, height, avgR, avgG, avgB)
    quadPriorityQueue += Quad(startX, startY, width, height, score)
  }

  def getAverageValues(startX: Int, startY: Int, width: Int, height: Int): (Int, Int, Int) = {
    var totalRed, totalGreen, totalBlue = 0
    val totalValues = height * width
    for (y <- startY until (startY + height);
         x <- startX until (startX + width)) {
      totalRed += redArray(x)(y)
      totalGreen += greenArray(x)(y)
      totalBlue += blueArray(x)(y)
    }
    (totalRed / totalValues, totalGreen / totalValues, totalBlue / totalValues)
  }

  def getScore(startX: Int, startY: Int, width: Int, height: Int, avgRed: Int, avgGreen: Int, avgBlue: Int): Double = {
    var redError, greenError, blueError = 0.0
    for (y <- startY until (startY + height);
         x <- startX until (startX + width)) {
      redError += Math.pow(redArray(x)(y) - avgRed, 2)
      greenError += Math.pow(greenArray(x)(y) - avgGreen, 2)
      blueError += Math.pow(blueArray(x)(y) - avgBlue, 2)
    }
    redError = Math.sqrt(redError / (width * height))
    greenError = Math.sqrt(greenError / (width * height))
    blueError = Math.sqrt(blueError / (width * height))
    val error = (redError * 0.3) + (greenError * 0.6) + (blueError * 0.11)
    error * Math.pow(width * height, 0.25)
  }

  def getRGB(r: Int, g: Int, b: Int): Int = ((r & 0x0ff) << 16) | ((g & 0x0ff) << 8) | (b & 0x0ff)

  def getModuloNumber(x: Int): Int = {
    if (x <= 10) 1
    else if (x <= 100) 10
    else 100
  }
}
