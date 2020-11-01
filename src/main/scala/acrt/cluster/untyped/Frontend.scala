package acrt.cluster.untyped

import scala.concurrent.duration._
import akka.util.Timeout
import scala.util.Failure
import scala.util.Success
import akka.actor._
import swiftvis2.raytrace._

class Frontend(img: rendersim.RTBufferedImage, numRays: Int, lights: List[PointLight]) extends Actor {
  import Frontend._

  private var backends = IndexedSeq.empty[ActorRef]
  private var jobCounter = 0

  val numFiles = 5

  val organizer = context.actorOf(Props(new GeometryOrganizerSome(numFiles)), "GeometryOrganizer")
  val imageDrawer = context.actorOf(Props(new ImageDrawer(lights, img, numRays, organizer)), "ImageDrawer")

  val cellWidth = 1e-5
  val distanceUp = 1e-5
  val viewSize = 1e-5
  
  val eye = Point(0.0, 0.0, numFiles*1e-5)
  val topLeft = Point(-1e-5, 1e-5, (numFiles-1)*1e-5)
  val right = Vect(2 * 1e-5, 0, 0)
  val down = Vect(0, -2 * 1e-5, 0)

  def receive = {
    case BackendRegistration => {
      organizer ! GeometryOrganizerAll.BackendRegistration(sender)
    }
    case Start =>
      imageDrawer ! ImageDrawer.Start(eye, topLeft, right, down)

    case Terminated(a) =>
      backends = backends.filterNot(_ == a)
  }
}

object Frontend {
  case object Start extends CborSerializable
  case object BackendRegistration extends CborSerializable
}