package nl.about42.poly.generator

import nl.about42.poly.{Edge, Path, Polygon, Vertex}
import nl.about42.poly.validator.CandidateEdgeValidator

/**
  * Generator that will build all possible polygons an a grid of size N, following a set of rules
  */
class PolygonBuilder(size: Int) extends CandidateEdgeValidator{

  private val initialState = new PolygonState(new Path(Seq.empty), (1 to size).toList, (1 to size).toList)
/*
  override def iterator: Iterator[Polygon] = new Iterator[Polygon] {
    var levelState = Array.fill[LevelState](size)(new LevelState(0,0))
    var nextPol:Polygon = new Polygon(Seq(new Vertex(1,1), new Vertex(1,2)))
    var beenFetched = true
    def hasNext: Boolean = {
      if (!beenFetched)
        return true

      beenFetched = false
      val res = findNextPolygon(levelState, initialState)
      res match {
        case None => false
        case Some((pol, state)) =>
          levelState = state
          nextPol = pol
          true
      }
    }
    def next = {
      nextPol
    }

  }
*/
  def findNextPolygon( levelState: Array[LevelState], polygonState: PolygonState): Option[(Polygon, Array[LevelState])] = {
    if (polygonState.remainingX.size == 0) {
      // check if closing the path succeeds without intersections and with a correct slope
      if (validate(new Edge(polygonState.currentPath.vertices.last, polygonState.currentPath.vertices.head),
          new Path(polygonState.currentPath.vertices.tail))) {
        // advance highest level to the next state (otherwise same polygon will be found again)
        advanceState(size - 1, levelState)
        return Some (new Polygon( polygonState.currentPath.vertices), levelState)
      } else {
        return None
      }
    } else {
      // look forward through the options for a possible edge and then recurse into the next level
      val limit = polygonState.remainingX.size
      val level = size - polygonState.remainingX.size
      var x = levelState(level).dx
      var y = levelState(level).dy
      if (x >= limit && y >= limit) {
        return None
      }
      while (x < limit && y < limit) {
        var attempt = polygonState.addVertex(new Vertex(polygonState.remainingX(x), polygonState.remainingY(y)))
        attempt match {
          case Some(newState) =>
            val result = findNextPolygon(levelState, newState)
            result match {
              case Some((pol, state)) =>
                return Some((pol, state))
              case _ => // no action
            }
          case _ => // no action
        }
        advanceState(level, levelState)
        x = levelState(level).dx
        y = levelState(level).dy
        if (x == 0 && y == 0) {
          // level carry occurred, no more matches possible
          return None
        }
      }
    }
    return None
  }

  def advanceState( level: Int, state: Array[LevelState]): Unit = {
    val maxLevels = state.size
    val levelSize = maxLevels - level

    val x = state(level).dx
    val y = state(level).dy
    if (y >= levelSize - 1) {
      if (x >= levelSize - 1) {
        if (level > 0) {
          // this level is full, add one at a lower level
          advanceState(level - 1, state)
        } else {
          // cannot advance anymore, set overflow
          state(0) = new LevelState(maxLevels, maxLevels)
        }
      } else {
        state(level) = new LevelState(x + 1, 0)
        (level + 1 to maxLevels - 1).foreach(i => state(i) = new LevelState(0, 0))
      }
    } else {
      state(level) = new LevelState(x, y + 1)
      (level + 1 to maxLevels - 1).foreach(i => state(i) = new LevelState(0, 0))
    }
  }
}

class LevelState(val dx: Int, val dy: Int)


class PolygonState( val currentPath: Path, val remainingX: Seq[Int], val remainingY: Seq[Int]) extends CandidateEdgeValidator {
  def addVertex(vertex: Vertex): Option[PolygonState] = {
    System.out.println(s"$remainingX - $remainingY - addvertex $vertex to ${currentPath.vertices}")
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
