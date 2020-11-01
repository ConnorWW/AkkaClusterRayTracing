package acrt.cluster.untyped

import akka.actor.{Actor, Props}
import swiftvis2.raytrace.{PointLight, Ray, Point, Vect, RTColor}
import akka.actor.ActorRef

class ImageDrawer(lights: List[PointLight], img: rendersim.RTBufferedImage, numRays: Int, organizer: ActorRef) extends Actor {
  import ImageDrawer._
  
  val aspect = img.width.toDouble / img.height
  
  private var pixelsSet = 0
  private val totalPixels = img.width * img.height * numRays

  private var startTime: Long = 0

  def receive = {
    case Start(eye, topLeft, right, down) => {
      startTime = System.nanoTime()

      for (i <- (0 until img.width); j <- (0 until img.height)) {
        val pix = context.actorOf(Props(new PixelHandler(lights, i, j, numRays, organizer)), s"PixelHandler$i,$j")
        
        (0 until numRays).map(index => {
          pix ! PixelHandler.AddRay(Ray(eye, topLeft + right * (aspect * (i + (if (index > 0) math.random * 0.75 else 0)) / img.width) + down * (j + (if (index > 0) math.random * 0.75 else 0)) / img.height))
        })
      }
    }

    case SetColor(i, j, color) => {
      img.setColor(i, j, color)
      
      pixelsSet += 1
      if (pixelsSet >= totalPixels) {
        println((System.nanoTime() - startTime) * 1e-9)
      }
    }

    case m => "ImageDrawer received unhandled message: " + m
  }
}

object ImageDrawer {
  case class Start(eye: Point, topLeft: Point, right: Vect, down: Vect) extends CborSerializable
  case class SetColor(i: Int, j: Int, color: RTColor) extends CborSerializable
}