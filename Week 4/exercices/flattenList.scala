def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => Nil
  case x::t => (x match {
    case list: List[Any] => flatten(list)
    case y => List(y)
  }):::flatten(t)
}