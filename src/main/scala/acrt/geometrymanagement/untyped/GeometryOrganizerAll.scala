package acrt.geometrymanagement.untyped

import akka.actor.{Actor, ActorRef, Props}
import swiftvis2.raytrace.{Geometry, Ray, KDTreeGeometry, Vect, BoxBoundsBuilder, SphereBoundsBuilder, IntersectData}
import acrt.raytracing.untyped.PixelHandler
import acrt.photometry.untyped.ImageDrawer

class GeometryOrganizerAll(simpleGeom: Seq[Geometry]) extends Actor {
  import GeometryOrganizerAll._

  //Alternate Lines for BoxBoundsBuilder - Replace all to swap
  //val geoms = geomSeqs.mapValues(gs => new KDTreeGeometry(gs, builder = BoxBoundsBuilder))
  
  //Change this line for more/less breakup of geometry
  val numManagers = 1

  //Gets the Bounds of the Geometry
  val xmin = simpleGeom.minBy(_.boundingSphere.center.x).boundingSphere.center.x
  val xmax = simpleGeom.maxBy(_.boundingSphere.center.x).boundingSphere.center.x
  val ymin = simpleGeom.minBy(_.boundingSphere.center.y).boundingSphere.center.y
  val ymax = simpleGeom.maxBy(_.boundingSphere.center.y).boundingSphere.center.y

  //Groups the Geometry into slices and creates Managers for those pieces of Geometry
  val geomSeqs = simpleGeom.groupBy(g => ((g.boundingSphere.center.y - ymin) / (ymax-ymin) * numManagers).toInt min (numManagers - 1))
  val geoms = geomSeqs.mapValues(gs => new KDTreeGeometry(gs, builder = SphereBoundsBuilder))
  val geomManagers = geoms.map { case (n, g) => n -> context.actorOf(Props(new GeometryManager(g)), "GeometryManager" + n) }

  //Map of IDs to Buffers of IntersectDatas
  private val buffMap = collection.mutable.Map[Long, collection.mutable.ArrayBuffer[Option[IntersectData]]]() 
  
  private var num = 1

  def receive = {
    case GetBounds => {
      sender ! ImageDrawer.Bounds(xmin, xmax, ymin, ymax)
    }
    //Casts Rays to every Geometry and adds the ray to the Map
    case CastRay(rec, k, r) => {
      num += 1
      if (num % 100 == 0) {
        println("castRay")
        num = 1
      }
      buffMap += (k -> new collection.mutable.ArrayBuffer[Option[IntersectData]])
      geomManagers.foreach(_._2 ! GeometryManager.CastRay(rec, k, r, self))
    }
    //Receives back IntersectDatas from the Managers 
    case RecID(rec, k, id) => {
      //Adds the ID to the Buffer based on the associated Key
      val buffK = buffMap(k)
      buffK += id

      //When the buffer is full of data from each Manager, chooses the first hit and sends it back,
      //or sends back None if no hits
      if(buffK.length < numManagers) {
        buffMap -= k
        buffMap += (k -> buffK)
      } else {
        val editedBuff = buffK.filter(_ != None)

        if(editedBuff.isEmpty){
          rec ! PixelHandler.IntersectResult(k, None)
        } else {
          var lowest: IntersectData = editedBuff.head match {
            case Some(intD) => intD
            case None => null
          }

          for(i <- editedBuff) {
            i match {
              case Some(intD) => {
                if(intD.time < lowest.time) {
                  lowest = intD
                }
              }
              case None => println("how did we get here?")
            }
          }

          rec ! PixelHandler.IntersectResult(k, Some(lowest))
        }
      }
    }
    case m => "GeometryManager received unhandled message: " + m
  }
}

object GeometryOrganizerAll {
  case class CastRay(recipient: ActorRef, k: Long, r: Ray)
  case class RecID(recipient: ActorRef, k: Long, id: Option[IntersectData])
  case object GetBounds
}