package acrt.geometrymanagement.typed

import acrt.photometry.typed.PhotonCreator.PhotonCreatorIntersectResult
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import swiftvis2.raytrace.{Geometry, IntersectData, KDTreeGeometry, SphereBoundsBuilder}

object GeometryOrganizerAll {
  import GeometryOrganizer._
  def apply[A](simpleGeom: Seq[Geometry], intersectResultMaker: (Long, Option[IntersectData]) => A): Behavior[Command[A]] = Behaviors.receive { (context, message) =>
    val numManagers = 10

    val ymin = simpleGeom.minBy(_.boundingSphere.center.y).boundingSphere.center.y
    val ymax = simpleGeom.maxBy(_.boundingSphere.center.y).boundingSphere.center.y

    val geomSeqs = simpleGeom.groupBy(g => ((g.boundingSphere.center.y - ymin) / (ymax-ymin) * numManagers).toInt min (numManagers - 1))
    val geoms = geomSeqs.mapValues(gs => new KDTreeGeometry(gs, builder = SphereBoundsBuilder))
    val geomManagers = geoms.map { case (n, g) => n -> context.spawn(GeometryManager[A](g), "GeometryManager" + n) }

    val buffMap = collection.mutable.Map[Long, collection.mutable.ArrayBuffer[Option[IntersectData]]]() 
  
    message match {
      case CastRay(rec, k, r) => {
        buffMap += (k -> new collection.mutable.ArrayBuffer[Option[IntersectData]])
        geomManagers.foreach(_._2 ! GeometryManager.CastRay(rec, k, r, context.self))
        // context.log.info(s"Cast ray $k to GeometryManagers.")
      }
      case GetBounds(imgDrawer) => {} // Nothing needed for this approach

      case RecID(rec, k, id) => {
        val buffK = buffMap(k)
        buffK += id

        if(buffK.length < numManagers) {
          buffMap -= k
          buffMap += (k -> buffK)
        } else {
          val editedBuff = buffK.filter(_ != None)

          if(editedBuff.isEmpty){
            rec ! intersectResultMaker(k, None)
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

            rec ! intersectResultMaker(k, Some(lowest))
          }
        }
      }
    }
    Behaviors.same
  }
}