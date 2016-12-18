
case class Vertice( x: Int, y: Int)
case class Polygon(edges: Seq[Vertice])

val size = 3
val base1 = (1 to size).toList.permutations
val base = (1 to size).toList.permutations
val xValues = (2 to size).toList.permutations

val candidates = for {
  b1 <- base1
  x <- (2 to size).toList.permutations
} yield (b1, x)

def makePolygon( yValues: List[Int], xIndices: List[Int]): Polygon = {
  val vertices = xIndices.map(x => Vertice(x, yValues(x-1)))
  Polygon(vertices)
}

val test = candidates.map(x => makePolygon(x._1, 1 :: x._2))

test.foreach(x => System.out.println(x.edges))


def getPolygons(): Iterator[Polygon] = {
  for {
    yValues <- base
    xIndices <- (2 to size).toList.permutations
  } yield (makePolygon( yValues, 1 :: xIndices))
}

val t2 = getPolygons()

t2.foreach(x => System.out.println(x.edges))
