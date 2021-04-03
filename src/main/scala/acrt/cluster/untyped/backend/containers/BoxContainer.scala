package acrt.cluster.untyped.backend.containers

import swiftvis2.raytrace.Vect
import swiftvis2.raytrace.Box
import swiftvis2.raytrace.Point
import swiftvis2.raytrace.BoundingBox
import acrt.cluster.untyped.backend.CborSerializable

case class BoxContainer(min: Point, max: Point) extends CborSerializable with Box {
  //Never used, so stubbed until needed
  def movedBy(v: Vect): Box = {
    ???
  }
  def toBoundingBox: BoundingBox = {
    new BoundingBox(min, max)
  }
}
object BoxContainer {
  def apply(b: Box): BoxContainer = {
    new BoxContainer(b.min, b.max)
  }
}
