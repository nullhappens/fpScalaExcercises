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
    foldRight(Stream.empty)((a, acc) =>
      if (p(a)) Stream.cons(a(), acc) else Stream.empty)

  // 5.6
  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case _ => None
  }

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, acc) => Stream.cons(f(a), acc.map(f)))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, acc) => if(f(a)) Stream.cons(a, acc.filter(f)) else acc.filter(f))

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => Stream.cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, acc) => f(a).append(acc))

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
}
