package chapter5

sealed trait Stream[+A] {

  // 5.1
  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // 5.2
  def take(n: Int): Stream[A] =
    if (n > 0) this match {
      case Cons(h, _) if n == 1 =>
        Stream.cons(h(), Stream.empty) // we can say Stream.empty
      case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
      case _          => Stream.empty
    } else Stream() // or Stream()

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def loop(i: Int, s: Stream[A]): Stream[A] =
      if (i <= 0) s
      else
        s match {
          case Cons(_, t) => loop(i - 1, t())
          case Empty      => Stream.empty
        }
    loop(n, this)
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      if (p(h())) Stream.cons(h(), t() takeWhile p) else Stream.empty
    case Empty => Stream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Terminates early if p(a) is true

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, acc) =>
      if (p(a)) Stream.cons(a, acc) else Stream.empty)

  // 5.6
  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case _          => None
  }

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, acc) => Stream.cons(f(a), acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, acc) =>
      if (f(a)) Stream.cons(a, acc.filter(f)) else acc.filter(f))

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, acc) => f(a).append(acc))

  // 5.15
  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s     => Some((s, s drop 1))
    } append Stream.empty

  def hasSubSequence[B >: A](s: Stream[B]): Boolean =
    tails exists (s2 => Stream.startsWith(s, s2))

  // 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // "Smart" constructors
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 5.10
  def fibs(): Stream[Int] = {
    def loop(f0: Int, f1: Int): Stream[Int] =
      cons(f0, loop(f1, f0 + f1))
    loop(0, 1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None         => empty
    }

  // 5.12
  def fibsViaUnfold(): Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def onesViaUnfold(): Stream[Int] = unfold(1)(_ => Some((1, 1)))

  // 5.13
  def mapViaUnfold[A, B](s: Stream[A])(f: A => B): Stream[B] =
    unfold(s) {
      case Cons(hd, tl) => Some((f(hd()), tl()))
      case Empty        => None
    }

  def takeViaUnfold[A](s: Stream[A])(n: Int): Stream[A] =
    unfold((s, n)) {
      case (Cons(h, _), 1)          => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _                        => None
    }

  def takeWhileViaUnfold[A](s: Stream[A])(f: A => Boolean): Stream[A] =
    unfold(s) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _                    => None
    }

  def zipWith[A, B, C](s1: Stream[A], s2: Stream[B])(
      f: (A, B) => C): Stream[C] =
    unfold((s1, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[A, B](s1: Stream[A], s2: Stream[B]): Stream[(A, B)] =
    zipWith(s1, s2)((_, _))

  def zipAll[A, B](s1: Stream[A],
                   s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s1, s2)((_, _))

  def zipWithAll[A, B, C](s1: Stream[A], s2: Stream[B])(
      f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((s1, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) =>
        Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) =>
        Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  // 5.14
  def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean =
    Stream.zipAll(s1, s2).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }
}
