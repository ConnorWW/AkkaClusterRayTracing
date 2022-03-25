package acrt.photometry.typed

import java.awt.image.BufferedImage

import acrt.geometrymanagement.typed.GeometryOrganizerSome
import acrt.geometrymanagement.untyped.PhotometryCreator
import akka.actor.typed.ActorSystem
import swiftvis2.raytrace.{Point, PointLight, RTColor}

import scala.swing.{Alignment, Label, MainFrame, Swing}
import scala.util.Random

object Main extends App {
  val numRays = 1
  val cellWidth = 1e-5
  val distanceUp = 1e-5
  val viewSize = 1e-5
  val numSims = 10

  //val lights = List(PhotonSource(PointLight(RTColor(1, 1, 1), Point(1, 0, 0.2)), 4000))
  val lights = List(//PhotonSource(PointLight(RTColor(1, 1, 1), Point(200, 200, 0)), 4000), PhotonSource(PointLight(RTColor(1, 1, 1), Point(-200, -200, 0)), 4000),
                   PhotonSource(PointLight(RTColor(1, 0, 0), Point(200, -200, 100)), 400000), PhotonSource(PointLight(RTColor(0, 0, 1), Point(-200, 200, 100)), 400000))
  //val forward = Vect(0, 0, -1)
  //val up = Vect(0, 1, 0)
  //val viewLoc = Point(0.0, 0.0, numFiles*1e-5)
  val n = math.sqrt(numSims.toDouble / 10.0).ceil.toInt
  val viewLoc = Point(0.0, 0.0, (10 * n)*1e-5)
  val bimg = new BufferedImage(1000, 1000, BufferedImage.TYPE_INT_ARGB)
  for (i <- 0 until bimg.getWidth(); j <- 0 until bimg.getHeight()) bimg.setRGB(i, j, 0xFF000000)
  val img = new rendersim.RTBufferedImage(bimg)

  val pc = new PhotometryCreator

  val system = ActorSystem


  val view = GeometrySetup.standardDownView()
  val (eye, topLeft, right, down) = view

  val forward = right.cross(down) //could potentially be incorrectly negative if I got the handed-ness wrong
  val up = -down


  val simpleGeom = GeometrySetup.randomGeometryActualArr(new Random, 1, -1, 1, -1, 1, -1, 0.1, 20)


  val organizer = system.create(GeometryOrganizerSome[PhotonCreator.PhotonCreatorIntersectResult](simpleGeom, PhotonCreator.PhotonCreatorIntersectResult.apply), "GeomOrganizer")
  val imageDrawer = system.create(ImageDrawer(lights, eye, forward, up, img), "ImageDrawer")

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
      println("Repainting")
      last = System.nanoTime()
    }
  }
}
