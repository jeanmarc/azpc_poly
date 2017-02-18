package nl.about42.poly.generator

import nl.about42.poly.reporter.{DataStore, StateReporter}
import nl.about42.poly._
import nl.about42.poly.validator.CandidateEdgeValidator

/**
 * Generator that will build all possible polygons an a grid of size N, following a set of rules
 */
class PolygonBuilder(size: Int, dataStore: DataStore = new DataStore) extends CandidateEdgeValidator {
  val stateReporter = new StateReporter(dataStore)
  val entry = System.currentTimeMillis()
  val step = 2000
  var tick = entry + step

  private val initialState = new PolygonState(new Path(Seq.empty), (1 to size).toList, (1 to size).toList)
  var levelState = Array.fill[LevelState](size)(new LevelState(0, 0))
  levelState(size - 1) = new LevelState(0, -1)

  var minCandidate: Polygon = new Polygon(List(Vertex(1, 1), Vertex(size, size)))
  var maxCandidate: Polygon = new Polygon(List(Vertex(1, 1), Vertex(size, size)))
  var minArea: Double = 10e200
  var maxArea: Double = 0

  var currentSolution = new Solution(minArea, minCandidate, maxArea, maxCandidate)
  var done = false
  var start = System.currentTimeMillis()

  def abort = {
    done = true
  }

  def solve: Solution = {
    start = System.currentTimeMillis()
    while (!done) {
      val result = findNextPolygon(levelState, initialState)
      result match {
        case Some((pol, state)) => {
          levelState = state
          val area = pol.area
          if (area > maxArea) {
            maxCandidate = pol
            maxArea = area
            System.out.println(s"new max: ${maxCandidate.codeString} -       $area")
            currentSolution = new Solution(minArea, minCandidate, maxArea, maxCandidate)
          }
          if (area < minArea) {
            minCandidate = pol
            minArea = area
            System.out.println(s"new min: ${minCandidate.codeString} - $area")
            currentSolution = new Solution(minArea, minCandidate, maxArea, maxCandidate)
          }
        }
        case _ => done = true
      }
    }

    report(size, 0, 0, Array.empty, new Path(Seq.empty))
    new Solution(minArea, minCandidate, maxArea, maxCandidate)
  }

  def solvePartial( ) = ???

  def findNextPolygon(levelState: Array[LevelState], polygonState: PolygonState): Option[(Polygon, Array[LevelState])] = {
    if (polygonState.remainingX.size == 0) {
      // check if closing the path succeeds without intersections and with a correct slope
      if (validate(
        new Edge(polygonState.currentPath.vertices.last, polygonState.currentPath.vertices.head),
        new Path(polygonState.currentPath.vertices.tail)
      )) {
        return Some(new Polygon(polygonState.currentPath.vertices), levelState)
      } else {
        return None
      }
    } else {
      // look forward through the options for a possible edge and then recurse into the next level
      val limit = polygonState.remainingX.size
      val level = size - polygonState.remainingX.size

      // if we are at the 0 level, take a top-level step to move forward beyond the previous solution
      if (level == 0) {
        advanceState(size - 1, levelState)
      }

      var x = levelState(level).dx
      var y = levelState(level).dy
      if (x >= limit && y >= limit) {
        // overflow detected, no more polygons possible
        return None
      }
      while (x < limit && y < limit) {
        if (System.currentTimeMillis() > tick){
          report(size, level, tick, levelState, polygonState.currentPath)
          tick = System.currentTimeMillis() + step
        }
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
        if (x >= size && y >= size) {
          // level overflow occurred, no more matches possible
          return None
        }
      }
    }
    return None
  }

  def advanceState(level: Int, state: Array[LevelState]): Unit = {
    val maxLevels = state.size
    val levelSize = maxLevels - level

    val x = state(level).dx
    val y = state(level).dy
    if (y >= levelSize - 1) {
      if (x >= levelSize - 1) {
        // this level is full, set overflow
        state(level) = new LevelState(maxLevels, maxLevels)
      } else {
        state(level) = new LevelState(x + 1, 0)
        (level + 1 to maxLevels - 1).foreach(i => state(i) = new LevelState(0, 0))
      }
    } else {
      state(level) = new LevelState(x, y + 1)
      (level + 1 to maxLevels - 1).foreach(i => state(i) = new LevelState(0, 0))
    }
  }

  def report(size: Int, level: Int, tick: Long, levelState: Array[LevelState], path: Path) = {
    stateReporter.report(size, level, tick, start, System.currentTimeMillis(), levelState, path, currentSolution)
  }

}

class LevelState(val dx: Int, val dy: Int)

class PolygonState(val currentPath: Path, val remainingX: Seq[Int], val remainingY: Seq[Int]) extends CandidateEdgeValidator {
  def addVertex(vertex: Vertex): Option[PolygonState] = {
    //System.out.println(s"$remainingX - $remainingY - addvertex $vertex to ${currentPath.vertices}")
    if (!remainingX.contains(vertex.x) || !remainingY.contains(vertex.y)) {
      // the x and y values still have to be available
      None
    } else {
      currentPath.vertices.size match {
        case 0 => if (vertex.x != 1 || vertex.y > 1 + remainingY.size / 2) {
            // the first point must have x == 1 and y in the bottom half (incl mid value)
            None
          } else {
            Some(new PolygonState(new Path(Seq(vertex)), remainingX.filter(_ != vertex.x), remainingY.filter(_ != vertex.y)))
          }
        case 1 => if (vertex.y < currentPath.vertices.head.y) {
            // the second point must have a positive slope
            None
          } else {
            Some(new PolygonState(new Path(currentPath.vertices :+ vertex), remainingX.filter(_ != vertex.x), remainingY.filter(_ != vertex.y)))
          }
        case _ => if (validate(new Edge(currentPath.vertices.last, vertex), currentPath)) {
            val remX = remainingX.filter(_ != vertex.x)
            val remY = remainingY.filter(_ != vertex.y)
            val newPath = new Path(currentPath.vertices :+ vertex)
            if (remX.size == 4){
              if (closingPossible(newPath, remX, remY)) {
                Some(new PolygonState(newPath, remX, remY))
              } else {
                None
              }
            } else {
              Some(new PolygonState(newPath, remX, remY))
            }
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
