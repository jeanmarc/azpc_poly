package nl.about42.poly.reporter

import nl.about42.poly.{LevelState, Path, Solution}

/**
  * Create parsable output to monitor current state of polygon generation
  */
class StateReporter(datastore: DataStore) {

  def report( size: Int, level: Int, tick: Long, start: Long, finish: Long, levelState: Array[LevelState], currentPath: Path, currentSolution: Solution) = {
    val result =
      s"""
         |{"size": $size,
         | "tick": $tick,
         | "start": $start,
         | "finish": $finish,
         | "duration": ${finish - start},
         | "level": $level,
         | "state": "${dumpState(levelState)}",
         | "currentPath": "${currentPath.codeString}",
         | "minArea": ${currentSolution.minArea},
         | "minSolution": "${currentSolution.minPolygon.codeString}",
         | "maxArea": ${currentSolution.maxArea},
         | "maxSolution": "${currentSolution.maxPolygon.codeString}"
         |}
       """.stripMargin
    datastore.save(s"status_$size.json", result)
  }

  private def dumpState(state: Array[LevelState]): String = {
    state.map(s => s"(${s.dx},${s.dy})").mkString(",")
  }

}
