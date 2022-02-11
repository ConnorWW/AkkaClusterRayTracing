package acrt.photometry.typed

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import swiftvis2.raytrace.{Point, RTColor, RTImage, Ray, Vect}

object PhotonCreator {
  trait PhotonCreatorCommand
  case object Render extends PhotonCreatorCommand
  case class SetColor(x: Int, y: Int, col: RTColor) extends PhotonCreatorCommand

  def apply(xmin: Double, xmax: Double, ymin: Double, ymax: Double, source: PhotonSource, viewLoc: Point, forward: Vect, up: Vect, image: RTImage):Behavior[PhotonCreatorCommand] = {
    val right = forward.cross(up)
    var pixels = Array.fill(image.width, image.height)(RTColor.Black)
    val organizer = ??? //Main.organizer
    val rays = collection.mutable.Map[Long, Ray]()
    var returnedRays = 0L



    Behaviors.same
  }
}
