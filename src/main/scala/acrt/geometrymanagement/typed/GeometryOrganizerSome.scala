package acrt.geometrymanagement.typed

import acrt.raytracing.typed.PixelHandler
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import acrt.photometry.typed.ImageDrawer.Bounds
import acrt.photometry.typed.PhotonCreator.PhotonCreatorIntersectResult
import swiftvis2.raytrace.{Geometry, IntersectData, KDTreeGeometry, SphereBoundsBuilder}
 
object GeometryOrganizerSome {
  import GeometryOrganizer._

  def apply[A](simpleGeom: Seq[Geometry], intersectResultMaker: (Long, Option[IntersectData]) => A): Behavior[Command[A]] = Behaviors.setup { context =>
    val numTotalManagers = 10
    
    val ymin = simpleGeom.minBy(_.boundingSphere.center.y).boundingSphere.center.y
    val ymax = simpleGeom.maxBy(_.boundingSphere.center.y).boundingSphere.center.y
  
    val geomSeqs = simpleGeom.groupBy(g => ((g.boundingSphere.center.y - ymin) / (ymax-ymin) * numTotalManagers).toInt min (numTotalManagers - 1))
    val geoms = geomSeqs.map { case (n, gs) => n -> new KDTreeGeometry(gs, builder = SphereBoundsBuilder) }
    println("Printing Geom n values from GeomOrganizerSome")
    for(geom <- geoms) println(s"n: ${geom._1}")
    println("Concluding printing Geom n values from GeomOrganizerSome")
    val geomManagers: Map[Int, ActorRef[GeometryManager.CastRay[A]]] = geoms.map { case (n, g) => n -> context.spawn(GeometryManager[A](g), "GeometryManager" + n + System.currentTimeMillis()) }


    val buffMap = collection.mutable.Map[Long, collection.mutable.ArrayBuffer[Option[IntersectData]]]() 
    val numManagersMap = collection.mutable.Map[Long, Int]()
    Behaviors.receiveMessage { message =>
      message match {
        case CastRay(rec, k, r) => {
          //there is only one geom, none of the rays are hitting it
          val intersects = geoms.filter(_._2.boundingSphere.intersectParam(r) != None)
          context.log.info(s"geoms size: ${geoms.size}")
          buffMap += (k -> new collection.mutable.ArrayBuffer[Option[IntersectData]])
          numManagersMap += (k -> intersects.size)
          context.log.info(s"intersects is empty: ${intersects.isEmpty}")
          //intersects is empty every time
          if (intersects.isEmpty) rec ! intersectResultMaker(k, None)
          else for(i <- intersects) {
              geomManagers(i._1) ! GeometryManager.CastRay(rec, k, r, context.self)
          }
            context.log.info(s"Cast ray $k to GeometryManagers.")
        }
        case GetBounds(imgDrawer) => {
          //TODO: FIX
          imgDrawer ! Bounds(0, 100, 0, 100)
        }

        case RecID(rec, k, id) => {
          val buffK = buffMap(k)
          val numManagersK = numManagersMap(k)
          buffK += id
          context.log.info(s"RECID: buffK.length: ${buffK.length}, numManagersK: ${numManagersK}, bool: ${buffK.length < numManagersK}, recIDEmpty: ${id.isEmpty}")
          if(buffK.length < numManagersK) {
            buffMap += (k -> buffK)
          } else {
            val editedBuff = buffK.filter(_ != None)

            if(editedBuff.isEmpty){
              context.log.info(s"Sent empty intersect result $k to ${rec.path.name}")
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
              context.log.info(s"Sent full intersect result $k to ${rec.path.name}")
              rec ! intersectResultMaker(k, Some(lowest))
            }
          }
        }
      }
      Behaviors.same
    }
  }
}