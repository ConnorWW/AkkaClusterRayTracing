package acrt.geometrymanagement.typed

import acrt.photometry.typed.PhotonCreator.PhotonCreatorIntersectResult
import acrt.raytracing.typed.PixelHandler.PixelWork
import akka.actor.typed.scaladsl.{Behaviors, Routers}
import akka.actor.typed.{ActorRef, Behavior, SupervisorStrategy}
import swiftvis2.raytrace.{Geometry, Ray}
import akka.actor.typed.scaladsl.PoolRouter

object GeometryManager {
  case class CastRay[A](recipient: ActorRef[A], k: Long, ray: Ray, geomOrg: ActorRef[GeometryOrganizer.RecID[A]])

  def apply[A](geom: Geometry): Behavior[CastRay[A]] = 
  Behaviors.setup { context => 
    //creates a router of intersectors
    println("Creating pool of interceptors")
    val pool = Routers.pool(poolSize = Runtime.getRuntime().availableProcessors())(
        // make sure the workers are restarted if they fail
        Behaviors.supervise(Intersector[A](geom)).onFailure[Exception](SupervisorStrategy.restart))
    val router = context.spawn(pool, "IntersectRouter")
    
    Behaviors.receiveMessage {  message =>
      val k = message.k
      // context.log.info(s"Casting Ray $k to router.")
      //sends the castRay message to the intersector pool
      router ! message
      Behaviors.same
    }
  }
}