package nl.about42

package object poly {
  case class Vertice( x: Int, y: Int)
  case class Polygon(edges: Seq[Vertice])

}
