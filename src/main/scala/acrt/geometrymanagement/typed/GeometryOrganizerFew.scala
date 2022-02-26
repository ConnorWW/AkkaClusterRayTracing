package acrt.geometrymanagement.typed

import acrt.photometry.typed.PhotonCreator.PhotonCreatorIntersectResult
import acrt.raytracing.typed.PixelHandler
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import swiftvis2.raytrace._

object GeometryOrganizerFew {
  import GeometryOrganizer._

  def apply[A](simpleGeom: Seq[Geometry], intersectResultMaker: (Long, Option[IntersectData]) => A): Behavior[Command[A]] = Behaviors.receive { (context, message) =>
    val numManagers = 10
  
    val ymin = simpleGeom.minBy(_.boundingSphere.center.y).boundingSphere.center.y
    val ymax = simpleGeom.maxBy(_.boundingSphere.center.y).boundingSphere.center.y
    //geomSeqs: Map[Int, Seq[Geometry]], groups the geometry based on y location
    val geomSeqs = simpleGeom.groupBy(g => ((g.boundingSphere.center.y - ymin) / (ymax-ymin) * numManagers).toInt min (numManagers - 1))
    //geoms: Map[Int, KDTreeGeometry[BoundingSphere]], turns each Seq from geomSeqs into a KDTreeGeometry[BoundingSphere]
    val geoms = geomSeqs.map { case (n, gs) => n -> new KDTreeGeometry(gs, builder = SphereBoundsBuilder) }
    //geomManagers: Map[Int, ActorRef[GeometryManager.CastRay]], spawns a GeometryManager from each kdTree
    val geomManagers = geoms.map { case (n, g) => n -> context.spawn(GeometryManager[A](g), "GeometryManager" + n) }
    
    val intersectsMap = collection.mutable.Map[Long, (Ray, Array[(Int, (Double, Vect, Double, Vect))])]()
    
    message match {
      case CastRay(rec, k, r) => {
        val intersects = geoms.map(g => g._1 -> g._2.boundingSphere.intersectParam(r)).filter(g => g._2.map(_._3 > 0).getOrElse(false)).toArray.sortBy(_._2.get._3)
      
        if(intersects.nonEmpty) {
          geomManagers(intersects(0)._1) ! GeometryManager.CastRay(rec, k, r, context.self)
          if(intersects.length > 1) intersectsMap += (k -> (r -> intersects.tail.map(i => i._1 -> i._2.get)))
        } else {
          rec ! intersectResultMaker(k, None)
        }
        context.log.info(s"Cast ray $k to GeometryManagers.")
      }
      case GetBounds(imgDrawer) => {} // Nothing needed for this approach

      case RecID(rec, k, id) => {
        id match {
          case Some(intD) => {
            rec ! intersectResultMaker(k, id)
          } 
          case None => {
            if(intersectsMap.contains(k)) {
              val (r, intersects) = intersectsMap(k)
              geomManagers(intersects(0)._1) ! GeometryManager.CastRay(rec, k, r, context.self)
            
              if(intersects.length > 1) {
                intersectsMap += (k -> (r, intersects.tail))
              } else 
                intersectsMap -= k
            } else {
              rec ! intersectResultMaker(k, None)
            }
          }
        }
      }
    }
    Behaviors.same
  }
}