package acrt.geometrymanagement.typed

import acrt.raytracing.typed.PixelHandler
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import acrt.photometry.typed.ImageDrawer.Bounds
import swiftvis2.raytrace.ListScene
import acrt.photometry.typed.PhotonCreator.PhotonCreatorIntersectResult
import swiftvis2.raytrace.{Geometry, IntersectData, KDTreeGeometry, SphereBoundsBuilder, Box}
import swiftvis2.raytrace.BoundingBox.mutualBox
import swiftvis2.raytrace.BoundingBox
import acrt.geometrymanagement.untyped.GeometryCreator 
import acrt.cluster.untyped.frontend.WebCreator
import acrt.photometry.typed.ImageDrawer
 
object GeometryOrganizerSome {
  import GeometryOrganizer._

  def apply[A](numFiles: Int, gc: GeometryCreator, intersectResultMaker: (Long, Option[IntersectData]) => A): Behavior[Command[A]] = Behaviors.setup { context =>
    val numTotalManagers = 10
    val numberList: List[String] = List("5000", "5001", "5002", "5003", "5004", "5005", 
     "5006", "5007", "5008", "5009", "5010", "5011", "5012", "5013", "5014", "5015", 
     "5016", "5017", "5018", "5019", "5020", "5021", "5022", "5023", "5024", "5025", 
     "5026", "5027", "5028", "5029", "6000", "6001", "6002", "6003", "6004", "6005", 
     "6006", "6007", "6008", "6009", "6010", "6011", "6012", "6013", "6014", "6015", 
     "6016", "6017", "6018", "6019", "6020", "6021", "6022", "6023", "6024", "6025",
     "6026", "6027", "6028", "6029")

    val cartAndRadNumbers = {
      var nlist = numberList.take(numFiles)
      while(nlist.length< numFiles) {
        nlist = (nlist ++ nlist).take(numFiles)
      }
      nlist
    }

    val n = math.sqrt(numFiles.toDouble / 10.0).ceil.toInt

    val offsets = for(x <- 0 until 10 * n; y <- 0 until n) yield {
      (x * 2.0e-5 - (10 * n - 1) * 1e-5, y * 2e-4 - (n - 1) * 1e-4)
    }

    def giveOffsets(arr: Seq[String], offsetArray: IndexedSeq[(Double, Double)]): Map[String, (Double, Double)] = {
        arr.map(t => (t, offsetArray(arr.indexOf(t)))).toMap
    }
    //val ymin = simpleGeom.minBy(_.boundingSphere.center.y).boundingSphere.center.y
    //val ymax = simpleGeom.maxBy(_.boundingSphere.center.y).boundingSphere.center.y
    //context.log.info(s"simpleGeom size: ${simpleGeom.size}")
    //val geomSeqs = simpleGeom.groupBy(g => ((g.boundingSphere.center.y - ymin) / (ymax-ymin) * numTotalManagers).toInt min (numTotalManagers - 1))
    //val geoms = geomSeqs.map { case (n, gs) => n -> new KDTreeGeometry(gs, builder = SphereBoundsBuilder) }
    //println("Printing Geom n values from GeomOrganizerSome")
    //for(geom <- geoms) println(s"n: ${geom._1}")
    // println("Concluding printing Geom n values from GeomOrganizerSome")
    // val geomManagers: Map[Int, ActorRef[GeometryManager.CastRay[A]]] = geoms.map { case (n, g) => n -> context.spawn(GeometryManager[A](g), "GeometryManager" + n + System.currentTimeMillis()) }


    val buffMap = collection.mutable.Map[Long, collection.mutable.ArrayBuffer[Option[IntersectData]]]() 
    val numManagersMap = collection.mutable.Map[Long, Int]()

    val wc = new WebCreator

    val offsetsMap = giveOffsets(cartAndRadNumbers, offsets)
    var managerBounds = List[Box]()
    def totalBounds: Box = managerBounds.reduce(BoundingBox.mutualBox(_,_))
    val managers = for(n <- cartAndRadNumbers) yield {
      val data: Geometry = gc(n, offsetsMap(n))
      managerBounds =  data.boundingBox :: managerBounds
      (context.spawn(GeometryManager[A](data), "GeometryManager" + n + System.currentTimeMillis()) -> data)
    }

    Behaviors.receiveMessage { message =>
      message match {
        case CastRay(rec, k, r) => {
          val intersects = managers.filter(_._2.boundingBox.intersectParam(r) != None)
          //context.log.info(s"geoms size: ${geoms.size}")
          //context.log.info("geom: " + geoms.head.toString())
          buffMap += (k -> new collection.mutable.ArrayBuffer[Option[IntersectData]])
          numManagersMap += (k -> intersects.size)
          context.log.info(s"intersects is empty: ${intersects.isEmpty}")
          //intersects is empty every time
          if (intersects.isEmpty) rec ! intersectResultMaker(k, None)
          else for(i <- intersects) {
              i._1 ! GeometryManager.CastRay(rec, k, r, context.self)
          }
            // context.log.info(s"Cast ray $k to GeometryManagers.")
        }
        case GetBounds(imgDrawer) => {
          if(managerBounds.length == numFiles) 
            imgDrawer ! ImageDrawer.Bounds(totalBounds.min.x, totalBounds.max.x, totalBounds.min.y, totalBounds.max.y, totalBounds.min.z, totalBounds.max.z)
          else {
            Thread.sleep(1000)
            context.self ! GetBounds(imgDrawer)
          }
        }

        case RecID(rec, k, id) => {
          val buffK = buffMap(k)
          val numManagersK = numManagersMap(k)
          buffK += id
          // context.log.info(s"RECID: buffK.length: ${buffK.length}, numManagersK: ${numManagersK}, bool: ${buffK.length < numManagersK}, recIDEmpty: ${id.isEmpty}")
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
              // context.log.info(s"Sent full intersect result $k to ${rec.path.name}")
              rec ! intersectResultMaker(k, Some(lowest))
            }
          }
        }
      }
      Behaviors.same
    }
  }
}