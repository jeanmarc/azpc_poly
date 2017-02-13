
import nl.about42.poly._
import nl.about42.poly.generator.{PolygonBuilder, PolygonState, VerticesGenerator}
import nl.about42.poly.validator.PolygonValidator


//The values of n are 5, 7, 11, 17, 23, 29, 37, 47, 59, 71, 83, 97, 113, 131, 149, 167, 191, 223, 257, 293, 331, 373, 419, 467, 521.


val pgBuilder = new PolygonBuilder(5)



//val iter = pgBuilder.iterator
//iter.hasNext

/*
val pg = new VerticesGenerator(7).getPolygons()

class test extends PolygonValidator {

}


var minCandidate: Polygon = new Polygon(List(Vertex(1,1), Vertex(2,2)))
var maxCandidate: Polygon = new Polygon(List(Vertex(1,1), Vertex(2,2)))
var minArea: Double = 10e200
var maxArea: Double = 0

val tester = new test

while (pg.hasNext){
  val p = pg.next()
  System.out.print(".")
  if (tester.validate(p)){
    System.out.print("*")
    val area = p.area
    if (area > maxArea) {
      maxCandidate = p
      maxArea = area
      System.out.println(s"\nnew max: ${maxCandidate.codeString}")
    }
    if (area < minArea){
      minCandidate = p
      minArea = area
      System.out.println(s"\nnew min: ${minCandidate.codeString}")
    }
  }
}

minCandidate.codeString
minArea

maxCandidate.codeString
maxArea

*/