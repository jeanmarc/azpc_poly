package nl.about42.poly.generator

import nl.about42.poly.{Polygon, Vertice}

/**
  * Class that will generate a stream of list of vertices according to the rules for vertices
  */
class VerticesGenerator(size: Int) {
  val base = (1 to size).toList.permutations

  def getPolygons(): Iterator[Polygon] = {
    for {
      yValues <- base
      xIndices <- (2 to size).toList.permutations
    } yield (makePolygon( yValues, 1 :: xIndices))
  }

  private def makePolygon( yValues: List[Int], xIndices: List[Int]): Polygon = {
    val vertices = xIndices.map(x => Vertice(x, yValues(x-1)))
    Polygon(vertices)
  }

}
