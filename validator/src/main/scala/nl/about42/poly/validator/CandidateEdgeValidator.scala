package nl.about42.poly.validator

import nl.about42.poly.{ Edge, Path, Vertex }

/**
 * Created by Jean-Marc van Leerdam on 2017-02-13
 */
trait CandidateEdgeValidator {
  def validate(edge: Edge, currentPath: Path) = {
    uniqueSlopes(edge, currentPath.edges) &&
    noIntersections(edge, currentPath.edges.dropRight(1))
  }

  private def noIntersections(seg: Edge, edges: Seq[Edge]) = {
    def checkSegment(seg: Edge, edges: Seq[Edge]): Boolean = {
      edges match {
        case Nil => true
        case head :: tail => !intersects(seg, head) && checkSegment(seg, tail)
      }
    }

    checkSegment(seg, edges)
  }

  private def uniqueSlopes(edge: Edge, edges: Seq[Edge]) = {
    edges.foldRight(true)((eX, result) => result && !edge.slope.equals(eX.slope))
  }

  private def intersects(e1: Edge, e2: Edge): Boolean = {
    if (((e1.v1.x < e2.v1.x && e1.v1.x < e2.v2.x && e1.v2.x < e2.v1.x && e1.v2.x < e2.v2.x) ||
      (e1.v1.x > e2.v1.x && e1.v1.x > e2.v2.x && e1.v2.x > e2.v1.x && e1.v2.x > e2.v2.x)) &&
      ((e1.v1.y < e2.v1.y && e1.v1.y < e2.v2.y && e1.v2.y < e2.v1.y && e1.v2.y < e2.v2.y) ||
        (e1.v1.y > e2.v1.y && e1.v1.y > e2.v2.y && e1.v2.y > e2.v1.y && e1.v2.y > e2.v2.y))) {
      // segments cannot intersect, because they are completely next to each other, above each other or both
      false
    }
    // do an intersection test
    (ccw(e1.v1, e1.v2, e2.v1) * ccw(e1.v1, e1.v2, e2.v2) <= 0) &&
      (ccw(e2.v1, e2.v2, e1.v1) * ccw(e2.v1, e2.v2, e1.v2) <= 0)
  }

  private def ccw(v1: Vertex, v2: Vertex, v3: Vertex): Int = {
    (v2.x - v1.x) * (v3.y - v1.y) - (v3.x - v1.x) * (v2.y - v1.y)
  }

}
