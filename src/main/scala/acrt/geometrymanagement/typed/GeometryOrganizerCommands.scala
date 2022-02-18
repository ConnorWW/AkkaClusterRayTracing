package acrt.geometrymanagement.typed

import acrt.photometry.typed.ImageDrawer
import acrt.photometry.typed.PhotonCreator.PhotonCreatorIntersectResult
import acrt.raytracing.typed.PixelHandler.PixelWork
import akka.actor.typed.ActorRef
import swiftvis2.raytrace.{IntersectData, Ray}

object GeometryOrganizer {
  sealed trait Command
  case class CastRay(recipient: ActorRef[PixelWork], k: Long, r: Ray) extends Command
  case class RecID(recipient: ActorRef[PhotonCreatorIntersectResult], k: Long, id: Option[IntersectData]) extends Command

  case class GetBounds(imgDrawer: ActorRef[ImageDrawer.Command]) extends Command

}
