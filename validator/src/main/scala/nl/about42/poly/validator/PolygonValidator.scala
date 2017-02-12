package nl.about42.poly.validator

import nl.about42.poly.{Vertex, Edge, Polygon}

/**
  * Validates the polygon (it must have coordinates between 1 and size, for both x and y)
  */
trait PolygonValidator {
  def validate( polygon: Polygon): Boolean = {
    allVerticesWithinBounds(polygon) &&
    firstYinBottomHalf(polygon) &&
    firstEdgeSlopePositive(polygon) &&
    checkSlopes(polygon) &&
    noIntersections(polygon)
  }

  def allVerticesWithinBounds( polygon: Polygon): Boolean = {
    polygon.vertices match {
      case Nil => false
      case vertices => val size = vertices.size
        vertices.maxBy(_.x).x <= size &&
          vertices.maxBy(_.y).y <= size &&
          vertices.minBy(_.x).x >= 1 &&
          vertices.minBy(_.y).y >= 1
    }
  }

  def firstYinBottomHalf( polygon: Polygon): Boolean = {
    polygon.vertices.head.y <= 1 + polygon.vertices.size / 2
  }

  def firstEdgeSlopePositive( polygon: Polygon): Boolean = {
    // we know the first edge starts at x == 1, so dx must always be positive
    polygon.edges.head.slope.dy > 0
  }

  def checkSlopes( polygon: Polygon): Boolean = {
    def uniqueSlopes( edges: Seq[Edge]): Boolean = edges match {
      case Nil => true
      case first :: tail => tail.foldLeft(true)((b, e) => b && !first.slope.equals(e.slope)) && uniqueSlopes(tail)
    }
    uniqueSlopes(polygon.edges)
  }

  def noIntersections(polygon: Polygon): Boolean = {
    // verify segments do not intersect
    // check seg1 against seg3..segN-1 (adjoining segments do not intersect by definition)
    // check seg2 against seg4..segN
    // check seg3 against seg5..segN and so forth

    def checkSegment( seg: Edge, edges: Seq[Edge]): Boolean = {
      edges match {
        case Nil => true
        case head :: tail => !intersects(seg, head) && checkSegment(seg, tail)
      }
    }

    def walker( edges: Seq[Edge]): Boolean = {
      edges match {
        case Nil => true
        case e1 :: e2 :: tail => checkSegment(e1, tail) && walker( e2 :: tail)
        case e1 :: tail => true
      }
    }

    checkSegment(polygon.edges.head, polygon.edges.dropRight(1).tail.tail) &&
    walker( polygon.edges.tail)
  }


  private def intersects(e1: Edge, e2: Edge): Boolean = {
    if (
    ((e1.v1.x < e2.v1.x && e1.v1.x < e2.v2.x && e1.v2.x < e2.v1.x && e1.v2.x < e2.v2.x) ||
     (e1.v1.x > e2.v1.x && e1.v1.x > e2.v2.x && e1.v2.x > e2.v1.x && e1.v2.x > e2.v2.x)
    ) &&
    ((e1.v1.y < e2.v1.y && e1.v1.y < e2.v2.y && e1.v2.y < e2.v1.y && e1.v2.y < e2.v2.y) ||
     (e1.v1.y > e2.v1.y && e1.v1.y > e2.v2.y && e1.v2.y > e2.v1.y && e1.v2.y > e2.v2.y)
    )) {
      // segments cannot intersect, because they are completely next to each other, above each other or both
      false
    }
    // do an intersection test
    (ccw(e1.v1, e1.v2, e2.v1) * ccw(e1.v1, e1.v2, e2.v2) <= 0) &&
    (ccw(e2.v1, e2.v2, e1.v1) * ccw(e2.v1, e2.v2, e1.v2) <= 0)
  }

  private def ccw( v1: Vertex, v2: Vertex, v3: Vertex): Int = {
    (v2.x - v1.x) * (v3.y - v1.y) - (v3.x - v1.x) * (v2.y - v1.y)
  }
}
