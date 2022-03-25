package acrt.geometrymanagement.typed

import acrt.photometry.typed.PhotonCreator.PhotonCreatorIntersectResult
import acrt.raytracing.typed.PixelHandler.PixelWork
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import swiftvis2.raytrace.{Geometry, Ray}

object Intersector {
  //case class CastRay[A](k: Long, ray: Ray, rec: ActorRef[A], geomOrg: ActorRef[GeometryOrganizer.RecID[A]])
  //only accepts one message: CastRay
  def apply[A](geom: Geometry): Behavior[GeometryManager.CastRay[A]] = Behaviors.receive { (context, message) =>
    val k = message.k
    // context.log.info(s"Intersect Geometry $k.")
    //sends the original geometryOrganizer the IntersectResult, the id, and the intersectData of the potential collision
    message.geomOrg ! GeometryOrganizer.RecID(message.recipient, message.k, geom intersect message.ray)
    Behaviors.same
  }
}