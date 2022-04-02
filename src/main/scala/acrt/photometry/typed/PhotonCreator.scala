package acrt.photometry.typed

import acrt.geometrymanagement.typed.GeometryOrganizer
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import swiftvis2.raytrace._

object PhotonCreator {
  sealed trait PhotonCreatorCommand
  case object Render extends PhotonCreatorCommand
  case class SetColor(x: Int, y: Int, col: RTColor) extends PhotonCreatorCommand
  case class PhotonCreatorIntersectResult(k: Long, intD: Option[IntersectData]) extends PhotonCreatorCommand

  def apply(xmin: Double, xmax: Double, ymin: Double, ymax: Double, zmin: Double, zmax: Double, source: PhotonSource,
            viewLoc: Point, forward: Vect, up: Vect, image: RTImage, parent: ActorRef[ImageDrawer.Command]):Behavior[PhotonCreatorCommand] = {
    Behaviors.setup { context => 
      val right = forward.cross(up)
      var pixels = Array.fill(image.width, image.height)(RTColor.Black)
      val organizer = Main.organizer
      val rays = collection.mutable.Map[Long, Ray]()
      var returnedRays = 0L
      var createdScatterers = 0
    
      Behaviors.receiveMessage { message:PhotonCreatorCommand =>
        
        message match {
          case SetColor(x, y, col) => {
            parent ! ImageDrawer.UpdateColor(x, y, pixels(x)(y) + col)
            pixels(x)(y) = pixels(x)(y) + col
            //context.log.info("Set pixel color")
          }
          case PhotonCreatorIntersectResult(k, oid:Option[IntersectData]) => {
            // context.log.info("PhotonCreatorIntersectResult received, isEmpty: " + oid.isEmpty)
            oid.map { iData:IntersectData =>
              //context.log.info("Creating newScatterer")
              createdScatterers += 1
              context.log.info(s"createdScatterers: $createdScatterers")
              val newScatterer = context.spawn(Scatterer(source, viewLoc, forward, up, iData, image.width, image.height, rays(k).dir, context.self), s"Scatterer$k")
              //println(s"Ray $k was Scattered")
            }
          }
          case Render => {
            for (_ <- 0L until source.numPhotons) {
              context.log.info(s"xmin: $xmin xmax: $xmax ymin: $ymin ymax: $ymax ")
              val ray = Ray(
                source.light.point,
                Point(
                  xmin + math.random() * (xmax - xmin),
                  ymin + math.random() * (ymax - ymin),
                  zmin + math.random() * (zmax - zmin), 
                )
              )
              context.log.info(s"Casting $ray")
              val k = scala.util.Random.nextLong()
              organizer ! GeometryOrganizer.CastRay(context.self, k, ray)
              rays += (k -> ray)
            }
          }
        }
        Behaviors.same
      }
    }
  }
}
