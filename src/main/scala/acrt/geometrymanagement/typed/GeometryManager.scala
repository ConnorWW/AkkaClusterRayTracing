package acrt.geometrymanagement.typed

import acrt.raytracing.typed.PixelHandler
import akka.actor.typed.scaladsl.{Behaviors, Routers}
import akka.actor.typed.{ActorRef, Behavior, SupervisorStrategy}
import swiftvis2.raytrace.{Geometry, Ray}

object GeometryManager {
  case class CastRay(recipient: ActorRef[PixelHandler.IntersectResult], k: Long, ray: Ray, geomOrg: ActorRef[GeometryOrganizer.RecID])

  def apply(geom: Geometry): Behavior[CastRay] = 
  Behaviors.receive { (context, message) =>
    //creates a router of intersectors
    val pool = Routers.pool(poolSize = Runtime.getRuntime().availableProcessors())(
        // make sure the workers are restarted if they fail
        Behaviors.supervise(Intersector(geom)).onFailure[Exception](SupervisorStrategy.restart))
    val router = context.spawn(pool, "IntersectRouter")
    val k = message.k
    context.log.info(s"Casting Ray $k to router.")
    //sends the castRay message to the intersector pool
    router ! Intersector.CastRay(message.k, message.ray, message.recipient, message.geomOrg)
    Behaviors.same
  }
}