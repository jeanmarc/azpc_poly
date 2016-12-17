package nl.about42.poly.validator

/**
  * Validates the polygon
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
