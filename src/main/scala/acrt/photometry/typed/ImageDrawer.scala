package acrt.photometry.typed

import acrt.geometrymanagement.typed.GeometryOrganizer
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import swiftvis2.raytrace.{Point, RTColor, RTImage, Vect}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object ImageDrawer {
  sealed trait Command
  case object MoreRays extends ImageDrawer.Command
  case class Start(startPixels: Array[Array[RTColor]]) extends Command
  case class UpdateColor(x: Int, y: Int, col: RTColor) extends Command
  case class Bounds(xmin: Double, xmax: Double, ymin: Double, ymax: Double, zmin:Double, zmax:Double) extends Command
  case class AcquireBounds(org: ActorRef[GeometryOrganizer.Command[PhotonCreator.PhotonCreatorIntersectResult]]) extends Command
  case object RayBlocked extends Command


  def apply(sources:List[PhotonSource], viewLoc:Point, forward: Vect, up: Vect, img: RTImage, start:Long) (implicit ec: ExecutionContext): Behavior[Command] = {
    var howManyRays: Long = sources.map(m => m.numPhotons).sum
    val threads = 8
    var pixels: Array[Array[RTColor]] = null

    var xmin = -1.0
    var xmax = 1.0
    var ymin = -1.0
    var ymax = 1.0
    var zmin = -1.0
    var zmax = 1.0

    var changedPixels = 0

    Behaviors.receive { (context, message) =>
      message match {
        case MoreRays => {
          howManyRays += 1

        } case Start(startPixels) => {
          println("Starting!")
          pixels = startPixels
          // context.log.info(s"threads: $threads, |sources|: ${sources.length}")
          for(/*c <- 1 to threads;*/ light <- sources) {
            // context.log.info(s"xmin: $xmin xmax: $xmax ymin $ymin ymax $ymax zmin $zmin zmax $zmax")
            val id = light.numPhotons + scala.util.Random.nextLong()
            val child: ActorRef[PhotonCreator.PhotonCreatorCommand] = context.spawn(PhotonCreator(xmin, xmax, ymin, ymax, zmin, zmax, light, viewLoc, forward, up, img, context.self), s"PhotonCreator,$id")
            // context.log.info("Making call to render")
            child ! PhotonCreator.Render
          }
        } case RayBlocked => {
            howManyRays -= 1
            if(howManyRays % 100 == 0) context.log.info(s"$howManyRays rays left at ${(System.nanoTime() - start) *1e-9} seconds")
        } case UpdateColor(x, y, col) => {
          //context.log.info(s"Updating color $x, $y, $col")
          changedPixels += 1
          howManyRays -= 1

          val startingColor = pixels(x)(y)

          if(col.a != 0.0 || col.b != 0.0 || col.g != 0.0 || col.r != 0.0) pixels(x)(y) = col + startingColor

          if(changedPixels >= 100 /*img.width * img.height*/) { //"pixel buffer" of 100 pixels
            writeToImage(pixels, img)
            changedPixels = 0
          }
          context.log.info(s"$howManyRays rays left at ${(System.nanoTime() - start) *1e-9} seconds")
          if(howManyRays <= 0) context.log.info(s"All rays drawn in ${(System.nanoTime() - start) *1e-9} seconds")

        } case Bounds(bxmin, bxmax, bymin, bymax, bzmin, bzmax) => {
          xmin = bxmin
          xmax = bxmax
          ymin = bymin
          ymax = bymax
          zmin = bzmin
          zmax = bzmax
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
