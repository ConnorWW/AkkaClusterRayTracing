package acrt.cluster.untyped.backend.containers

import swiftvis2.raytrace.{Point, Vect, Sphere}
import swiftvis2.raytrace.BoundingSphere
import acrt.cluster.untyped.backend.CborSerializable

//Serializable Container for Sphere trait
case class SphereContainer(center: Point, radius: Double) extends CborSerializable with Sphere {
  //Never used, so stubbed until needed
  def movedBy(v: Vect): Sphere = {
    ???
  }
  def toBoundingSphere: BoundingSphere = {
    new BoundingSphere(center, radius)
  }
}

object SphereContainer {
  def apply(s: Sphere): SphereContainer = {
    new SphereContainer(s.center, s.radius)
  }
}
