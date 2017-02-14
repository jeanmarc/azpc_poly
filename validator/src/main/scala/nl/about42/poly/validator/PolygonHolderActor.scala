package nl.about42.poly.validator

import akka.actor.{ Actor, ActorLogging }
import akka.http.scaladsl.model.{ StatusCode, StatusCodes }
import nl.about42.poly.Polygon

import scala.collection.mutable

case class AddPolygon(name: String, polygon: Polygon)
case class RemovePolygon(name: String)
case class GetPolygon(name: String)
case class Result(status: StatusCode, message: String)

class PolygonHolderActor extends Actor with ActorLogging with PolygonValidator {

  val myPolygons = mutable.Map[String, Polygon]()

  def receive = {
    case AddPolygon(name, polygon) =>
      if (validate(polygon)) {
        myPolygons(name) = polygon
        sender ! Result(StatusCodes.OK, s"Polygon $name accepted")
      } else {
        sender ! Result(StatusCodes.BadRequest, s"Polygon $name did not pass validation")
      }
    case RemovePolygon(name) =>
      if (myPolygons.contains(name)) {
        myPolygons.remove(name)
        sender ! Result(StatusCodes.OK, s"Polygon $name removed")
      } else {
        sender ! Result(StatusCodes.BadRequest, s"Polygon $name is not known")
      }
    case GetPolygon(name) =>
      if (myPolygons.contains(name)) {
        sender ! myPolygons(name)
      } else {
        sender ! Result(StatusCodes.BadRequest, s"Polygon $name is not known")
      }
  }

}
