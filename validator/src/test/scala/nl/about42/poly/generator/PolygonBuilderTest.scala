package nl.about42.poly.generator

import nl.about42.poly.reporter.{ConsoleStore, FileStore}
import nl.about42.poly.{LevelState, Path, Polygon, Vertex}

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
    //val gridSizes = List(5, 7, 11, 17, 23, 29, 37, 47, 59, 71, 83, 97, 113, 131, 149, 167, 191, 223, 257, 293, 331, 373, 419, 467, 521)

    val gridSizes = List(11)
    gridSizes.foreach(s => generate(s))
  }

  def generate(size: Int) = {
    val pgBuilder = new PolygonBuilder(size, new FileStore("frontend/src/main/resources/www/results"))

    val solution = pgBuilder.solve

    System.out.println(s"final min: ${solution.minPolygon.codeString} - ${solution.minArea}")
    System.out.println(s"final max: ${solution.maxPolygon.codeString} - ${solution.maxArea}")

  }

  private def dumpState(state: Array[LevelState]) = {
    state.foreach(s => System.out.print(s"(${s.dx},${s.dy}),"))
    System.out.println()

  }
}
