package nl.about42.poly.generator

import nl.about42.poly.{Path, Polygon, Vertex}

/**
  * Testing the builder
  */
class PolygonBuilderTest extends org.scalatest.FunSuite {

  test("State advancement should work") {
    val pgBuilder = new PolygonBuilder(5)
    var levelState = Array.fill[LevelState](5)(new LevelState(0,0))

    pgBuilder.advanceState(3, levelState)
    dumpState(levelState)
    pgBuilder.advanceState(3, levelState)
    dumpState(levelState)
    (0 to 10).foreach (i => {
    pgBuilder.advanceState(2, levelState)
    dumpState(levelState)
    })
  }

  test("Some polygons will be generated") {
    val pgBuilder = new PolygonBuilder(5)

    val initialState = new PolygonState(new Path(Seq.empty), (1 to 5).toList, (1 to 5).toList)
    var levelState = Array.fill[LevelState](5)(new LevelState(0,0))

    var minCandidate: Polygon = new Polygon(List(Vertex(1,1), Vertex(2,2)))
    var maxCandidate: Polygon = new Polygon(List(Vertex(1,1), Vertex(2,2)))
    var minArea: Double = 10e200
    var maxArea: Double = 0

    var done = false

    while(!done) {
      dumpState(levelState)
      val result = pgBuilder.findNextPolygon(levelState, initialState)
      dumpState(levelState)
      result match {
        case Some((pol, state)) => {
          levelState = state
          val area = pol.area
          if (area > maxArea) {
            maxCandidate = pol
            maxArea = area
            System.out.println(s"new max: ${maxCandidate.codeString}")
          }
          if (area < minArea) {
            minCandidate = pol
            minArea = area
            System.out.println(s"new min: ${minCandidate.codeString}")
          }
        }
        case _ => done = true
      }
    }

  }

  private def dumpState(state: Array[LevelState]) = {
    state.foreach( s => System.out.print(s"(${s.dx},${s.dy}),"))
    System.out.println()

  }
}
