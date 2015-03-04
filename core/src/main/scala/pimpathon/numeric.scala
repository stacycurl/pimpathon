package pimpathon


object numeric {
  implicit class NumericOps[A](val na: Numeric[A]) extends AnyVal {
    def xmap[B](aToB: A ⇒ B, bToA: B ⇒ A): Numeric[B] = na match {
      case xna: XMappedNumeric[_, _] ⇒ xna.xmap(aToB, bToA)
      case other                     ⇒ new XMappedNumeric[A, B](other, aToB, bToA)
    }
  }

  class XMappedNumeric[A, B](na: Numeric[A], aToB: A ⇒ B, bToA: B ⇒ A) extends Numeric[B] {
    def xmap[C](bToC: B ⇒ C, cToB: C ⇒ B): Numeric[C] =
      new XMappedNumeric[A, C](na, aToB andThen bToC, cToB andThen bToA)

    def compare(l: B, r: B): Int = na.compare(bToA(l), bToA(r))

    def fromInt(i: Int): B = aToB(na.fromInt(i))

    def toDouble(b: B): Double = na.toDouble(bToA(b))
    def toFloat(b: B): Float   = na.toFloat(bToA(b))
    def toInt(b: B): Int       = na.toInt(bToA(b))
    def toLong(b: B): Long     = na.toLong(bToA(b))

    def negate(b: B): B = aToB(na.negate(bToA(b)))

    def minus(l: B, r: B): B = aToB(na.minus(bToA(l), bToA(r)))
    def plus(l: B, r: B): B  = aToB(na.plus(bToA(l),  bToA(r)))
    def times(l: B, r: B): B = aToB(na.times(bToA(l), bToA(r)))
  }
}