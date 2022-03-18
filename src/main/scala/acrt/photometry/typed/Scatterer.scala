package acrt.photometry.typed

import acrt.geometrymanagement.typed.GeometryOrganizer
import acrt.raytracing.typed.PixelHandler
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import photometry.ScatterGeometry
import swiftvis2.raytrace.{IntersectData, Point, Ray, Vect}

object Scatterer {
  trait ScatterCommand

  def apply(source: PhotonSource, viewLoc: Point, forward: Vect, up: Vect, id: IntersectData, width: Int, height: Int, dir: Vect, parent:ActorRef[PhotonCreator.PhotonCreatorCommand]):Behavior[PhotonCreator.PhotonCreatorIntersectResult] = {
    Behaviors.setup{ context => 
      val interPoint = id.point + id.norm * 1e-8
      val right = forward.cross(up)
      context.log.info("Scatterer created")
      Main.organizer ! GeometryOrganizer.CastRay[PhotonCreator.PhotonCreatorIntersectResult](context.self, scala.util.Random.nextLong(), Ray(interPoint, viewLoc))

      Behaviors.receiveMessage {  message =>

        message match {
          case PhotonCreator.PhotonCreatorIntersectResult(k, intD) => {
            context.log.info("Scatterer Got Intersect Result.")
            if (intD.isEmpty) {
              val inRay = (viewLoc - interPoint).normalize
              //val scatter = id.geom.asInstanceOf[ScatterGeometry].fractionScattered(dir, inRay, id)
              //at one point I had errors from the above line converting from GeomSpheres - pulled the math out to here (we already had the required values).
              val scatter = inRay.normalize.dot(id.norm)
              if (scatter > 0.0) {
                val fracForward = inRay dot forward
                //val px = ((inRay.dot(right) / fracForward / 0.707 + 1.0) * width / 2).toInt
                //val py = ((-inRay.dot(up) / fracForward / 0.707 + 1.0) * height / 2).toInt
                //
                val px = ((inRay.dot(right) / fracForward / 1.0 + 1.0) * width / 2).toInt
                val py = ((-inRay.dot(up) / fracForward / 1.0 + 1.0) * height / 2).toInt
                context.log.info(s"Pixel location and color calculated. px: $px py: $py width: $width height: $height")
                if (px >= 0 && px < width && py >= 0 && py < height) {
                  context.log.info("color set")
                  parent ! PhotonCreator.SetColor(px, py, source.light.col * id.color * scatter)
                }
              }
            }
            Behaviors.same
          }

        }
      }
    }
  }
}
