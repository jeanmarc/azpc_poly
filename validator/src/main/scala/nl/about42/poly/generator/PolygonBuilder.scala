package nl.about42.poly.generator

import nl.about42.poly.{Edge, Path, Polygon, Vertex}
import nl.about42.poly.validator.CandidateEdgeValidator

/**
  * Generator that will build all possible polygons an a grid of size N, following a set of rules
  */
class PolygonBuilder(size: Int) extends Iterable[PolygonState] with CandidateEdgeValidator{

  private val initialState = new PolygonState(new Path(Seq.empty), (1 to size).toList, (1 to size).toList)
  private val levelState = Array.fill[(Int, Int)](size)((0,0))

  override def iterator: Iterator[PolygonState] = new Iterator[PolygonState] {
    def hasNext = true
    def next = ???
  }

  def findNextPolygon( levelState: Array[LevelState], polygonState: PolygonState): Option[(Polygon, Array[LevelState])] = {
    if (polygonState.remainingX.size == 0) {
      // check if closing the path succeeds without intersections and with a correct slope
      if (validate(new Edge(polygonState.currentPath.vertices.last, polygonState.currentPath.vertices.head),
          new Path(polygonState.currentPath.vertices.tail))) {
        Some (new Polygon( polygonState.currentPath.vertices), levelState)
      } else {
        None
      }
    } else {
      // look forward through the options for a possible edge and then recurse into the next level
      val level = size - polygonState.remainingX.size
      var startx = levelState(level).dx
      var starty = levelState(level).dy
      while (startx < size && starty < size) {
        var attempt = polygonState.addVertex(new Vertex(polygonState.remainingX(startx), polygonState.remainingY(starty)))
        attempt match {
          case None =>
            starty += 1
            if (starty == size){
              starty = 0
              startx += 1
              if (startx == size) {
                return None
              }
            }
          case Some(newState) =>
            levelState(level) = new LevelState(startx, starty)
            val result = findNextPolygon(levelState, newState)
            result match {
              case None =>
                // continue looking
                starty += 1
                if (starty == size){
                  starty = 0
                  startx += 1
                  if (startx == size) {
                    return None
                  }
                }
              case Some((pol, state)) =>
                return Some((pol, state))
            }
        }
      }
    }
    return None
  }

}

class LevelState(val dx: Int, val dy: Int)



class PolygonState( val currentPath: Path, val remainingX: Seq[Int], val remainingY: Seq[Int]) extends CandidateEdgeValidator {
  def addVertex(vertex: Vertex): Option[PolygonState] = {
    if (!remainingX.contains(vertex.x) || !remainingY.contains(vertex.y)) {
      None
    } else {
      currentPath.vertices.size match {
        case 0 => if (vertex.x != 1 || vertex.y > 1 + remainingY.size / 2) {
                    None
                  } else {
                    Some(new PolygonState(new Path(Seq(vertex)), remainingX.filter(_ != vertex.x), remainingY.filter(_ != vertex.y)))
                  }
        case 1 => if (vertex.y < currentPath.vertices.head.y) {
                    None
                  } else {
                    Some(new PolygonState(new Path(currentPath.vertices :+ vertex), remainingX.filter(_ != vertex.x), remainingY.filter(_ != vertex.y)))
                  }
        case _ => if (validate(new Edge(currentPath.vertices.last, vertex), currentPath)) {
                    Some(new PolygonState(new Path(currentPath.vertices :+ vertex), remainingX.filter(_ != vertex.x), remainingY.filter(_ != vertex.y)))
                  } else {
                    None
                  }
      }
    }
  }

  def showState: String = {
    s"path: ${currentPath.codeString}, remX: $remainingX, remY: $remainingY"
  }
}
