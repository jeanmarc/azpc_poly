
import nl.about42.poly._
import nl.about42.poly.generator.{PolygonBuilder, PolygonState, VerticesGenerator}
import nl.about42.poly.validator.PolygonValidator


//The values of n are 5, 7, 11, 17, 23, 29, 37, 47, 59, 71, 83, 97, 113, 131, 149, 167, 191, 223, 257, 293, 331, 373, 419, 467, 521.


val remX = Seq(1, 1, 1)
val remY = Seq(2, 4, 6)

val res = for {
  x <- remX
  y <- remY
} yield (x, y)

res.foldRight(true)((x, b) => if (x._1 == 1) b else false)
