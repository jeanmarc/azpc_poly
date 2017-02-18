package nl.about42

package object poly {
  case class Vertex(x: Int, y: Int) {
    override def toString: String = {
      s"($x,$y)"
    }
  }

  case class Slope(dx: Int, dy: Int) {
    def equals(s: Slope): Boolean = {
      dx * s.dy == dy * s.dx
    }
  }

  case class Edge(v1: Vertex, v2: Vertex) {
    val slope = calculateSlope(v1, v2)

    /**
     * Returns normalized slope of the edge
     */
    private def calculateSlope(v1: Vertex, v2: Vertex): Slope = {
      val dx = v2.x - v1.x
      val dy = v2.y - v1.y
      Slope(dx, dy)
    }
  }

  // a closed path of vertices
  case class Polygon(vertices: Seq[Vertex]) {
    private val v2 = vertices.tail :+ vertices.head
    val edges = vertices.zip(v2).map(e => new Edge(e._1, e._2))

    def codeString: String = {
      vertices.mkString(",")
    }

    /*
      Area of polygon is given by formula:
      area = 0.5 * abs(  (x1*y2 - y1*x2) + (x2*y3 - y2*x3) ... + (xN*y1 - yN*x1)  )
     */
    def area: Double = {
      0.5 * Math.abs(vertices.zip(vertices.tail :+ vertices.head).foldLeft(0.0)((sum, pair) => sum + pair._1.x * pair._2.y - pair._1.y * pair._2.x))
    }
  }

  /*
     Path is an unclosed sequence of vertices
   */
  case class Path(val vertices: Seq[Vertex]) {
    val edges = vertices.size match {
      case 0 => Seq.empty
      case 1 => Seq.empty
      case _ => vertices.dropRight(1).zip(vertices.tail).map(e => new Edge(e._1, e._2))
    }

    def codeString: String = {
      vertices.mkString(",")
    }

    def asPolygon: Polygon = new Polygon(vertices)
  }

  class Solution(val minArea: Double, val minPolygon: Polygon, val maxArea: Double, val maxPolygon: Polygon)

  class LevelState(val dx: Int, val dy: Int)

  class SolverState( val currentSolution: Solution, val level: Int, val levelState: Array[LevelState], val timeSpent: Long)

  object SolverState {
    def fromFile(filename: String): SolverState = {
      StateBuilder.fromFile(filename)
    }
  }
}
