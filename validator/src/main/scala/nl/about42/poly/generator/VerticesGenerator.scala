package nl.about42.poly.generator

import nl.about42.poly.{ Edge, Polygon, Vertex }

/**
 * Class that will generate a stream of list of vertices according to the rules for vertices
 */
class VerticesGenerator(size: Int) {

  def getPolygons(): Iterator[Polygon] = {
    for {
      yValues <- (1 to size).toList.permutations
      xIndices <- (2 to size).toList.permutations
    } yield (makePolygon(yValues, 1 :: xIndices))
  }

  private def makePolygon(yValues: List[Int], xIndices: List[Int]): Polygon = {
    val vertices = xIndices.map(x => Vertex(x, yValues(x - 1)))
    Polygon(vertices)
  }

}
