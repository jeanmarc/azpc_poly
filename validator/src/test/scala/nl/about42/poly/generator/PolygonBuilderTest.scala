package nl.about42.poly.generator

import nl.about42.poly.reporter.ConsoleStore
import nl.about42.poly.{Path, Polygon, Vertex}

/**
 * Testing the builder
 */
class PolygonBuilderTest extends org.scalatest.FunSuite {

  test("State advancement should work") {
    val pgBuilder = new PolygonBuilder(5)
    var levelState = Array.fill[LevelState](5)(new LevelState(0, 0))
    levelState(4) = new LevelState(0, -1)

    pgBuilder.advanceState(4, levelState)
    dumpState(levelState)
    pgBuilder.advanceState(3, levelState)
    dumpState(levelState)
    pgBuilder.advanceState(3, levelState)
    dumpState(levelState)
    (0 to 10).foreach(i => {
      pgBuilder.advanceState(2, levelState)
      dumpState(levelState)
    })
  }

  test("Some polygons will be generated") {
    val gridSize = 11
    val pgBuilder = new PolygonBuilder(gridSize, new ConsoleStore)

    val solution = pgBuilder.solve

    System.out.println(s"final min: ${solution.minPolygon.codeString} - ${solution.minArea}")
    System.out.println(s"final max: ${solution.maxPolygon.codeString} - ${solution.maxArea}")

  }

  private def dumpState(state: Array[LevelState]) = {
    state.foreach(s => System.out.print(s"(${s.dx},${s.dy}),"))
    System.out.println()

  }
}
