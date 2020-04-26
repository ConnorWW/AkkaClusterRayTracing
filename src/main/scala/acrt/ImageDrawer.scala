package acrt

import akka.actor.Actor
import swiftvis2.raytrace._
import akka.actor.Props

class ImageDrawer(lights: List[PointLight], img: rendersim.RTBufferedImage, numRays: Int) extends Actor {
  import ImageDrawer._
  //Aspect Ratio  
  val aspect = img.width.toDouble / img.height
  private var pixelsSet = 0
  private val totalPixels = img.width * img.height * numRays
  private val start = System.nanoTime()

  def receive = {
    case Start(eye, topLeft, right, down) => {
      for (i <- (0 until img.width); j <- (0 until img.height)) {
        //Creates a new child actor assigned to the given (x,y) pixel
        val pix = context.actorOf(Props(new PixelHandler(lights, i, j, numRays)), s"PixelHandler$i,$j")
        //Sends numRays Rays to the new PixelHandler to be sent to the Geometry and averaged into a color
        (0 until numRays).map(index => {
          pix ! PixelHandler.AddRay(Ray(eye, topLeft + right * (aspect * (i + (if (index > 0) math.random * 0.75 else 0)) / img.width) + down * (j + (if (index > 0) math.random * 0.75 else 0)) / img.height))
        })
      }
    }

    case SetColor(i, j, color) => {
      //Assigns the (x,y) pixel of the BufferedImage to be the supplied color
      img.setColor(i, j, color)
      pixelsSet += 1
      if (pixelsSet >= totalPixels) {
        println((System.nanoTime() - start) * 1e-9)
      }
    }

    case m => "ImageDrawer received unhandled message: " + m
  }
}

object ImageDrawer {
  case class Start(eye: Point, topLeft: Point, right: Vect, down: Vect)
  case class SetColor(i: Int, j: Int, color: RTColor)
}