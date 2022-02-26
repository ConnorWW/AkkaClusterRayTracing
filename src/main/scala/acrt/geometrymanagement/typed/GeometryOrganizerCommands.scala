package acrt.geometrymanagement.typed

import acrt.photometry.typed.ImageDrawer
import acrt.photometry.typed.PhotonCreator.PhotonCreatorIntersectResult
import acrt.raytracing.typed.PixelHandler.PixelWork
import akka.actor.typed.ActorRef
import swiftvis2.raytrace.{IntersectData, Ray}

object GeometryOrganizer {
  sealed trait Command[A]
  case class CastRay[A](recipient: ActorRef[A], k: Long, r: Ray) extends Command[A]
  case class RecID[A](recipient: ActorRef[A], k: Long, id: Option[IntersectData]) extends Command[A]

  case class GetBounds[A](imgDrawer: ActorRef[ImageDrawer.Command]) extends Command[A]

}
