package acrt.cluster.untyped

import akka.actor.Actor
import swiftvis2.raytrace._
import akka.actor.Props
import collection.mutable
import akka.actor.ActorSelection
import akka.actor.ActorRef

class PixelHandler(lights: List[PointLight], i: Int, j: Int, numRays: Int, organizer: ActorRef) extends Actor {
  import PixelHandler._
  //Buffer of Colors to be merged
  private val buff = mutable.ArrayBuffer[RTColor]() 
  private var count = 0
  def receive = {
    case AddRay(r) => {
      //Casts original Ray for (x,y)
      organizer ! GeometryOrganizerAll.CastRay(self, scala.util.Random.nextLong(), r)
    }
    case IntersectResult(k: Long, intD: Option[IntersectData]) => {
      intD match {
        //If receives back no intersection, color is Black, else sends Rays to lights
        case None =>  context.parent ! ImageDrawer.SetColor(i, j, RTColor.Black)
        case Some(id) => {
          //Uses count so no duplicate names
          val chld = context.actorOf(Props(new LightMerger(lights, id, organizer)), s"LightMerger$i,$j,$count")
          count+=1
        }
      }
    }
    case SetColor(col: RTColor) => {
      //Adds color to buffer and if buffer is full, sends message of merged colors to be set
      buff += col
      if(buff.length >= numRays) {
        context.parent ! ImageDrawer.SetColor(i, j, buff.reduceLeft(_ + _) / numRays)
        context.stop(self)
      }
    }
    case m => "me pixelhandler. me receive " + m
  }
}
object PixelHandler {
  case class AddRay(r: Ray) extends Serializable
  case class SetColor(col: RTColor) extends Serializable
  case class IntersectResult(k: Long, intD: Option[IntersectData]) extends Serializable
}
