
import nl.about42.poly._
import nl.about42.poly.generator.VerticesGenerator
import nl.about42.poly.validator.PolygonValidator

import scala.collection.mutable

val v1 = new nl.about42.poly.Vertex(1, 1)
val v2 = new nl.about42.poly.Vertex(5, 2)
val v3 = Vertex(9,3)

val e = new nl.about42.poly.Edge(v1, v2)

e.slope

val e2 = new Edge(v2, v1)
val e3 = new Edge(v3, v1)

e.slope.equals(e2.slope)
e.slope.equals(e3.slope)
e2.slope equals e3.slope

e3.slope

val cand = (0 to 3).toList.permutations

while (cand.hasNext)
  System.out.println(cand.next)

//The values of n are 5, 7, 11, 17, 23, 29, 37, 47, 59, 71, 83, 97, 113, 131, 149, 167, 191, 223, 257, 293, 331, 373, 419, 467, 521.


val pg = new VerticesGenerator(11).getPolygons()

class test extends PolygonValidator {

}


var minCandidate: Polygon = new Polygon(List(Vertex(1,1), Vertex(2,2)))
var maxCandidate: Polygon = new Polygon(List(Vertex(1,1), Vertex(2,2)))
var minArea: Double = 10e200
var maxArea: Double = 0

val tester = new test

while (pg.hasNext){
  val p = pg.next()
  if (tester.validate(p)){
    val area = p.area
    if (area > maxArea) {
      maxCandidate = p
      maxArea = area
    }
    if (area < minArea){
      minCandidate = p
      minArea = area
    }
  }
}

minCandidate.codeString
minArea

maxCandidate.codeString
maxArea

