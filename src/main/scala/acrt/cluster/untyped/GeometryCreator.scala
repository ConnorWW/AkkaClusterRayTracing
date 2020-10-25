package acrt.cluster.untyped
import swiftvis2.raytrace._
import java.net.URL
import data.CartAndRad
import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.fasterxml.jackson.annotation.JsonSubTypes


/*@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(value = classOf[WebCreator], name = "webcreator"),
    new JsonSubTypes.Type(value = classOf[FileCreator], name = "toomuchramcreator")))*/
sealed trait GeometryCreator extends Serializable  {
    def apply(num: String, offset: Double): Geometry
}

class WebCreator extends GeometryCreator {
    def apply(num: String, offset: Double): Geometry = {
      val carURL = new URL(s"http://www.cs.trinity.edu/~mlewis/Rings/AMNS-Moonlets/Moonlet4/CartAndRad.$num.bin")

      val simpleGeom = CartAndRad.readStream(carURL.openStream).map(p => 
        GeomSphere(Point(offset*2.0e-5-p.x, p.y, p.z), p.rad, _ => new RTColor(1, 1, 1, 1), _ => 0.0))
      
      val geom = new KDTreeGeometry(simpleGeom, builder = SphereBoundsBuilder)
      geom
    }
}

class FileCreator extends GeometryCreator {
    def apply(num: String, offset: Double): Geometry = {
      ???
    }
}