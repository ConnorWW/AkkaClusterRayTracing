package acrt.raytracing

import java.awt.image.BufferedImage
import java.net.URL
import data.CartAndRad
import scala.swing.{MainFrame, Label, Swing, Alignment}
import akka.actor.{ActorSystem, Props}
import swiftvis2.raytrace.{PointLight, GeomSphere, RTColor, Point, Vect}
import acrt.geometrymanagement.{GeometryOrganizerAll, GeometryOrganizerFew, GeometryOrganizerSome}

object Main extends App {
  //Alternate Organizers:
  //val organizer = system.actorOf(Props(new GeometryOrganizerAll(particles)), "GeomOrganizer")
  //val organizer = system.actorOf(Props(new GeometryOrganizerSome(particles)), "GeomOrganizer")

  //Pulls the geometry data from the supplied file within the given directory. Assigns the color of the spheres to black.
  val carURL = new URL("http://www.cs.trinity.edu/~mlewis/Rings/AMNS-Moonlets/Moonlet4/CartAndRad.6029.bin")
  val particles = CartAndRad.readStream(carURL.openStream).map(p => GeomSphere(Point(p.x, p.y, p.z), p.rad, _ => new RTColor(1, 1, 1, 1), _ => 0.0))
  
  //Visualization Params
  val numRays = 1
  val cellWidth = 1e-5
  val distanceUp = 1e-5
  val viewSize = 1e-5
  val numSims = 6
  val firstXOffset = cellWidth * (numSims - 1)
  
  //View Options: Uncomment for different default views
  //Top-Down view
  val eye = Point(0, 0, distanceUp)
  val topLeft = Point(-viewSize, viewSize, distanceUp - viewSize)
  val right = Vect(2 * viewSize, 0, 0)
  val down = Vect(0, -2 * viewSize, 0)
  
  //Across the top view positive Y
  //val eye = Point(0, -firstXOffset-2*cellWidth, distanceUp)
  //val topLeft = Point(-viewSize, -firstXOffset-2*cellWidth+viewSize, distanceUp + viewSize)
  //val right = Vect(2 * viewSize, 0, 0)
  //val down = Vect(0, 0, -2 * viewSize)

  //Across the top view positive X
  //val eye = Point(-firstXOffset-2*cellWidth, 0, distanceUp)
  //val topLeft = Point(-firstXOffset-2*cellWidth+viewSize, viewSize, distanceUp + viewSize)
  //val right = Vect(0, -2 * viewSize, 0)
  //val down = Vect(0, 0, -2 * viewSize)
  
  //Uncomment for multiple simulations
  //val particles = (0 until numSims).flatMap { i =>
  //  (CartAndRad.read(new java.io.File(s"/home/mlewis/Rings/AMNS-Moonlets/Moonlet4c/CartAndRad.720$i.bin"))).map(p => GeomSphere(Point(p.x - firstXOffset + i * 2 * cellWidth, p.y, p.z), p.rad, _ => new RTColor(1, 1, 1, 1), _ => 0.0))
  //}

  println(s"# particles = ${particles.length}")
  
  //Creates a List of PointLights. Does not work for AmbientLights.
  val lights: List[PointLight] = List(PointLight(new RTColor(0.9, 0.9, 0.9, 1), Point(1e-1, 0, 1e-2)), PointLight(new RTColor(0.5, 0.4, 0.1, 1), Point(-1e-1, 0, 1e-2)))
  
  //Creates an RT BufferedImage of the Screen Size
  val bimg = new BufferedImage(1200, 1200, BufferedImage.TYPE_INT_ARGB)
  val img = new rendersim.RTBufferedImage(bimg)
    
  //Creates an actorsystem, an ImageDrawer actor to handle RayTracing, and a GeometryManager actor to handle intersection math, then sends the ImageDrawer the message to start
  val system = ActorSystem("AkkaSystem")  
  val imageDrawer = system.actorOf(Props(new ImageDrawer(lights, img, numRays)), "ImageDrawer")
  val organizer = system.actorOf(Props(new GeometryOrganizerFew(particles)), "GeomOrganizer")
  imageDrawer ! ImageDrawer.Start(eye, topLeft, right, down)
  
  //Creates the Swing frame and places the Buffered Image in it
  val frame = new MainFrame {
    title = "AkkaRT Frame"
    contents = new Label("", Swing.Icon(bimg), Alignment.Center)
  }
  frame.visible = true
  
  //Simple repainting loop
  var repainting = true
  var last = System.nanoTime()
  while (true) {
    val delay = System.nanoTime() - last
    if (delay >= (.5 * 1e9)) {
      frame.repaint()
      last = System.nanoTime()
    }
  }
}
