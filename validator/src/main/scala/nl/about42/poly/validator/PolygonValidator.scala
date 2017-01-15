package nl.about42.poly.validator

import nl.about42.poly.Polygon

/**
  * Validates the polygon (it must have coordinates between 1 and size, for both x and y)
  */
trait PolygonValidator {
  def validate( polygon: Polygon): Boolean = {
    polygon.edges match {
      case Nil => false
      case edges => val size = edges.size
        edges.maxBy(_.x).x <= size &&
        edges.maxBy(_.y).y <= size &&
        edges.minBy(_.x).x >= 1 &&
        edges.minBy(_.y).y >= 1
    }
  }

}
