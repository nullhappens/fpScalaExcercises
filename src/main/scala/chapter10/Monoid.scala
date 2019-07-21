package chapter10

trait Monoid[A] {

  def combine(a: A, b: A): A

  def identity: A

}

object Monoid {

  // 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def combine(a: Int, b: Int): Int = a + b

    override def identity: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def combine(a: Int, b: Int): Int = a * b

    override def identity: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(a: Boolean, b: Boolean): Boolean = a || b

    override def identity: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(a: Boolean, b: Boolean): Boolean = a && b

    override def identity: Boolean = true
  }

  // 10.2
  def optionMonoid[A](implicit ev: Monoid[A]): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      override def combine(a: Option[A], b: Option[A]): Option[A] =
        for {
          va <- a
          vb <- b
        } yield ev.combine(va, vb)

      override def identity: Option[A] = None
    }

  // 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def combine(a: A => A, b: A => A): A => A = a compose b

    override def identity: A => A = (a: A) => a
  }

  // 10.4
  // Write tests using the hideous test crap we wrote in chapter8 (pass)

  // 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.identity)((b, a) => m.combine(b, f(a)))

  // 10.6 (pass)

  // 10.7
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.isEmpty)
      m.identity
    else if (as.length == 1)
      f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
}
