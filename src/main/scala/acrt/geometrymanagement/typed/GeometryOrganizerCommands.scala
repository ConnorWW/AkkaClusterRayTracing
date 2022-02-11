package acrt.geometrymanagement.typed

import acrt.photometry.typed.ImageDrawer
import acrt.raytracing.typed.PixelHandler
import akka.actor.typed.ActorRef
import swiftvis2.raytrace.{IntersectData, Ray}

object GeometryOrganizer {
  sealed trait Command
  case class CastRay(recipient: ActorRef[PixelHandler.IntersectResult], k: Long, r: Ray) extends Command
  case class RecID(recipient: ActorRef[PixelHandler.IntersectResult], k: Long, id: Option[IntersectData]) extends Command

  case class GetBounds(imgDrawer: ActorRef[ImageDrawer.Command])

}
