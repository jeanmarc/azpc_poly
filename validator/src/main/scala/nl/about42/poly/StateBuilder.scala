package nl.about42.poly

import scala.io.Source
import scala.util.parsing.json.JSON

/**
  * Created by Jean-Marc van Leerdam on 2017-02-18
  */
object StateBuilder {
  def fromFile( name: String): SolverState = {
    val dataA =
      """
        |{"size": 11,
        | "tick": 1487439292519,
        | "start": 1487434645797,
        | "finish": 1487439292520,
        | "duration": 4646723,
        | "level": 5,
        | "state": "(0,0),(0,0),(6,4),(5,1),(6,0),(5,2),(3,2),(2,2),(0,2),(1,1),(11,11)",
        | "currentPath": "(1,1),(2,2),(9,7),(8,4),(11,3)",
        | "minArea": 6.0,
        | "minSolution": "(1,1),(2,2),(5,7),(3,3),(8,11),(4,4),(10,9),(6,6),(11,10),(7,5),(9,8)",
        | "maxArea": 62.0,
        | "maxSolution": "(1,1),(2,2),(3,9),(6,7),(8,4),(7,6),(9,5),(5,8),(4,11),(10,10),(11,3)"
        |}
        |
      """.stripMargin // read file

    val data = Source.fromFile(name).mkString

    val stateInfo: Map[String, Any] = JSON.parseFull(data) match {
      case Some(m: Map[String, Any]) => m
    }
    val minSolutionString = stateInfo("minSolution") match {
      case s: String => s
      case _ => "not found"
    }
    val minArea = stateInfo("minArea") match {
      case a: Double => a
      case _ => 0.0
    }

    val minSolution = makePolygon(minSolutionString)
    ???
  }

  def makePolygon(input: String): Polygon = {
    ???
  }
}
