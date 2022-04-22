package acrt.photometry.typed

import java.awt.image.BufferedImage

import acrt.geometrymanagement.typed.GeometryOrganizerSome
import acrt.geometrymanagement.untyped.PhotometryCreator
import akka.actor.typed.ActorSystem
import swiftvis2.raytrace.{Point, PointLight, RTColor}
import scala.concurrent.ExecutionContext.global
import java.net.URL
import data.CartAndRad
import swiftvis2.raytrace._

import scala.swing.{Alignment, Label, MainFrame, Swing}
import scala.util.Random

object Main extends App {
  val numRays = 1
  val cellWidth = 1e-5
  val distanceUp = 1e-5
  val viewSize = 1e-5
  val numSims = 1
  //val carURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/AMNS-Moonlets/Moonlet4/CartAndRad.6029.bin")
  //val particles = CartAndRad.readStream(carURL.openStream).map(p => GeomSphere(Point(p.x, p.y, p.z), p.rad, _ => new RTColor(1, 1, 1, 1), _ => 0.0))
  //val lights = List(PhotonSource(PointLight(RTColor(1, 1, 1), Point(1, 0, 0.2)), 4000))
  val lights = List(PhotonSource(PointLight(RTColor(1, 1, 1), Point(1, 0, .2)), 100000)/*, PhotonSource(PointLight(RTColor(1, 0, 0), Point(-1e-1, 0, 1e-2)), 40000*/)
                   /*PhotonSource(PointLight(RTColor(1, 0, 0), Point(-10, 5, 10)), 400000)*//*, PhotonSource(PointLight(RTColor(0, 0, 1), Point(-200, 200, 100)), 40)*/
  // val forward = Vect(0, 0, -1)
  // val up = Vect(0, 1, 0)
  // val viewLoc = Point(0.0, 0.0, numSims*1e-5)

  

  val n = math.sqrt(numSims.toDouble / 10.0).ceil.toInt
  // val eye = Point(0.0, 0.0, (10 * n)*1e-5)
  // val topLeft = Point(-1e-5, 1e-5, ((10 * n)-1)*1e-5)
  // val right = Vect(2 * 1e-5, 0, 0)
  // val down = Vect(0, -2 * 1e-5, 0)


  //these values of viewLoc (aka eye), realForward, and realUp give us potentially correct images.
  val viewLoc = Point(0.0, 0.0, (10 * n)*1e-5)
  val realForward = Vect(0, 0, -1)
  val realUp = Vect(0, 1, 0)
  val bimg = new BufferedImage(1000, 1000, BufferedImage.TYPE_INT_ARGB)
  for (i <- 0 until bimg.getWidth(); j <- 0 until bimg.getHeight()) bimg.setRGB(i, j, 0xFF000000)
  val img = new rendersim.RTBufferedImage(bimg)

  val pc = new PhotometryCreator

  val system = ActorSystem
  val start = System.nanoTime()

  //val view = GeometrySetup.standardDownView()
  //val view = GeometrySetup.topView(numSims)
  //val (eye, topLeft, right, down) = view

  //val forward = right.cross(down) //could potentially be incorrectly negative if I got the handed-ness wrong
  //val up = -down
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  //val b = 2
  //val simpleGeom = GeometrySetup.randomGeometryActualArr(new Random, b, -b, b, -b, 1, -b, 0.1, 20)
  //val simpleGeom = GeometrySetup.hugeSphere()
  
  val organizer = system.create(GeometryOrganizerSome[PhotonCreator.PhotonCreatorIntersectResult](numSims, pc, PhotonCreator.PhotonCreatorIntersectResult.apply), "GeomOrganizer")
  //val imageDrawer = system.create(ImageDrawer(lights, eye, forward, up, img), "ImageDrawer")
  val imageDrawer = system.create(ImageDrawer(lights, viewLoc, realForward, realUp, img, start), "ImageDrawer")
  //val imageDrawer = system.create(ImageDrawer(lights, viewLoc, realForward, realUp, img), "ImageDrawer")

  imageDrawer ! ImageDrawer.AcquireBounds(organizer)

  val frame = new MainFrame {
    title = "AkkaPMR Frame"
    contents = new Label("", Swing.Icon(bimg), Alignment.Center)
  }
  frame.visible = true

  //Simple repainting loop
  var repainting = true
  var last = System.nanoTime()
  while (true) {;

    val delay = System.nanoTime() - last
    if (delay >= (.5 * 1e9)) {
      frame.repaint()
      //println("Repainting")
      last = System.nanoTime()
    }
  }
}
