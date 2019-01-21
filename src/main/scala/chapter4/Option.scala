package chapter4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None    => None
  }

  // can also be written as map(f) getOrElse None
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None    => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None    => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _    => this
  }

  // can also be written as flatMap(a => if(f(a)) Some(a) else None)
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) this else None
    case None    => None
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.nonEmpty)
        Some(xs.foldRight(0: Double)((x, acc) => x + acc) / xs.length)
      else None

    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }
  }

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(av => b.map(bv => f(av, bv)))

  // Written with for comprehensions
  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  // 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil    => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def sequence1[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((a, z) => map2(a, z)(_ :: _))

  // 4.5
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as match {
      case Nil    => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(a => a)

}
