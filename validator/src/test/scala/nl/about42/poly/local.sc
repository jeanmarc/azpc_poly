
import nl.about42.poly._
import nl.about42.poly.generator.{PolygonState, VerticesGenerator}
import nl.about42.poly.validator.PolygonValidator


//The values of n are 5, 7, 11, 17, 23, 29, 37, 47, 59, 71, 83, 97, 113, 131, 149, 167, 191, 223, 257, 293, 331, 373, 419, 467, 521.


val p1 = new Path(Seq.empty)

val p2 = new Path(Seq(new Vertex(1, 1)))

val p3 = new Path(Seq(new Vertex(1, 1), new Vertex(3, 2)))


p1.edges
p2.edges
p3.edges


val polgen = new PolygonState(new Path(Seq.empty), (1 to 5).toList, (1 to 5).toList)

polgen.showState

val pg1 = polgen.addVertex(new Vertex(1,1))

pg1 match {
  case None => "illegal add"
  case Some(pg) => pg.showState
}


val pg2 = pg1.get.addVertex(new Vertex(5,4))

pg2 match {
  case None => "illegal add"
  case Some(pg) => pg.showState
}

val pg3 = pg2.get.addVertex(new Vertex(4,2))

pg3 match {
  case None => "illegal add"
  case Some(pg) => pg.showState
}


val pg4 = pg3.get.addVertex(new Vertex(2,5))

pg4 match {
  case None => "illegal add"
  case Some(pg) => pg.showState
}


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