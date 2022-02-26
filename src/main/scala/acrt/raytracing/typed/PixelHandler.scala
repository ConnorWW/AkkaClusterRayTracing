package acrt.raytracing.typed

import acrt.geometrymanagement.typed._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import swiftvis2.raytrace._

import scala.collection.mutable

object PixelHandler {
  sealed trait PixelWork
  case class AddRay(r: Ray, geomOrg: ActorRef[GeometryOrganizer.CastRay[PixelWork]]) extends PixelWork
  case class SetColor(col: RTColor) extends PixelWork
  case class IntersectResult(k: Long, intD: Option[IntersectData]) extends PixelWork// with PhotonCreatorCommand
  case class StartLightMerger(geomOrg: ActorRef[GeometryOrganizer.CastRay[PixelWork]]) extends PixelWork

  private val buff = mutable.ArrayBuffer[RTColor]()
  private var geometryOrg: ActorRef[GeometryOrganizer.CastRay[PixelWork]] = null

  def apply(lights: List[PointLight], i: Int, j: Int, numRays: Int, parent: ActorRef[ImageDrawer.SetColor]): Behavior[PixelWork] = Behaviors.receive { (context, message) => 
    var count = 0

    message match {
      case AddRay(r, geomOrg) => {
        //Casts original Ray for (x,y)
        //sends ray to geometryOrganizer with reference to self
        geomOrg ! GeometryOrganizer.CastRay(context.self, scala.util.Random.nextLong(), r)
        //sets the geometryOrganizer for future reference
        geometryOrg = geomOrg
      }
      case IntersectResult(k: Long, intD: Option[IntersectData]) => {
        intD match {
          //If receives back no intersection, color is Black, else sends Rays to lights
          //Based on prior comment from Kurt, this definitely isn't photometric.
          case None => parent ! ImageDrawer.SetColor(i, j, RTColor.Black)
          case Some(id) => {
            //Uses count so no duplicate names
            //creates a LightMerger with the lights, the interesectData, and the id
            val chld = context.spawn(LightMerger(lights, id, context.self), s"LightMerger$i,$j,$count")
            count+=1
          }
        }
      }
      case SetColor(col: RTColor) => {
        //Adds color to buffer and if buffer is full, sends message of merged colors to be set
        buff += col
        if(buff.length >= numRays) {
          parent ! ImageDrawer.SetColor(i, j, buff.reduceLeft(_ + _) / numRays)
          context.stop(context.self)
        }
      }
      case StartLightMerger(g) => {
        context.log.warn("SOMETHING WENT WRONG WITH PIXELHANDLER")
      }
    }
    Behaviors.same
  }
}
