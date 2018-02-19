package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val t = singletonSet(3)
  val s = singletonSet(2)
  println(printSet(union(t, s)))
  println(printSet(diff(t, s)))
  println(printSet(intersect(t, s)))
}
