package acrt.photometry.typed

import acrt.geometrymanagement.typed.GeometryOrganizer
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import swiftvis2.raytrace.{Point, RTColor, RTImage, Vect}


object ImageDrawer {
  sealed trait Command
  case object MoreRays extends ImageDrawer.Command
  case class Start(startPixels: Array[Array[RTColor]]) extends Command
  case class UpdateColor(x: Int, y: Int, col: RTColor) extends Command
  case class Bounds(xmin: Double, xmax: Double, ymin: Double, ymax: Double) extends Command
  case class AcquireBounds(org: ActorRef[GeometryOrganizer.Command[PhotonCreator.PhotonCreatorIntersectResult]]) extends Command



  def apply(sources:List[PhotonSource], viewLoc:Point, forward: Vect, up: Vect, img: RTImage): Behavior[Command] = {
    var howManyRays: Long = sources.map(m => m.numPhotons).sum
    val threads = 8
    var pixels: Array[Array[RTColor]] = null

    var xmin = 0.0
    var xmax = 0.0
    var ymin = 0.0
    var ymax = 0.0

    var changedPixels = 0

    Behaviors.receive { (context, message) =>
      message match {
        case MoreRays => {
          howManyRays += 1

        } case Start(startPixels) => {
          println("Starting!")
          pixels = startPixels

          for(c <- 1 to threads; light <- sources) {
            val id = light.numPhotons + scala.util.Random.nextLong()
            val child: ActorRef[PhotonCreator.PhotonCreatorCommand] = context.spawn(PhotonCreator(xmin, xmax, ymin, ymax, light, viewLoc, forward, up, img, context.self), s"PhotonSender$c,$id")
            child ! PhotonCreator.Render
          }
        } case UpdateColor(x, y, col) => {
          changedPixels += 1
          howManyRays -= 1

          val startingColor = pixels(x)(y)

          if(col.a != 0.0 || col.b != 0.0 || col.g != 0.0 || col.r != 0.0) pixels(x)(y) = col + startingColor

          if(changedPixels >= 100 /*img.width * img.height*/) { //"pixel buffer" of 100 pixels
            writeToImage(pixels, img)
            changedPixels = 0
          }

          if(howManyRays <= 0) println("All rays drawn.")

        } case Bounds(xmin, xmax, ymin, ymax) => {
          println(s"smin: $xmin, xmax: $xmax, ymin: $ymin, ymax: $ymax")
          val startPixels = Array.fill(img.width, img.height)(RTColor.Black)
          context.self ! ImageDrawer.Start(startPixels)

        } case AcquireBounds(org) => {
          org ! GeometryOrganizer.GetBounds(context.self)

        }
      }
      Behaviors.same
    }
  }

  def writeToImage(pixels: Array[Array[RTColor]], image: RTImage): Unit = {
    val maxPix = pixels.foldLeft(0.0)((m,row) => m max row.foldLeft(0.0)((m2, p) => m2 max p.r max p.g max p.b))
    for (px <- 0 until image.width; py <- 0 until image.height) {
      image.setColor(px, py, (pixels(px)(py) / maxPix * 2.0).copy(a = 1.0))
    }
  }
}
