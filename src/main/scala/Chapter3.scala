//sealed trait means that all implementations of this trait (type/class/scalazfunzstuffzz) are included in this file.
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, t) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  // excercise 3.2
  def tail[A](x: List[A]): List[A] = x match {
    case Nil        => Nil
    case Cons(_, t) => t
  }

  // excercise 3.3
  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil        => Cons(x, Nil)
    case Cons(_, t) => Cons(x, t)
  }

  // excercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (n <= 0)
        Cons(h, t)
      else
        drop(t, n - 1)
  }

  // excercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h))
        dropWhile(t, f)
      else
        Cons(h, t)
  }

  // excercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil                   => Nil
    case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
    case Cons(h, t)            => Cons(h, init(t))
  }

  // excercise 3.7
  def product3(as: List[Int]): Int = foldRight(as, 1)(_ * _)

  // excercse 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0) { (_, b) =>
    as match {
      case Nil        => b
      case Cons(_, t) => 1 + length(t)
    }
  }

  // excercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // excercise 3.11
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product4(l: List[Int]): Int = foldLeft(l, 0)(_ * _)

  def length2(l: List[Int]): Int = foldLeft(l, 0) { (b, _) =>
    l match {
      case Nil           => b
      case Cons(_, tail) => 1 + length2(tail)
    }
  }

  // excercise 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc, h) => Cons(h, acc))

  // excercise 3.13
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  // excercise 3.14
  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))

  // excercise 3.15
  def flatten[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])((h, acc) => append(h, acc))

  // excercise 3.16
  def increment(as: List[Int]): List[Int] = as match {
    case Nil              => Nil
    case Cons(head, tail) => Cons(head + 1, increment(tail))
  }

  def incrementViaFoldRight(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((a, acc) => Cons(a + 1, acc))

  // 3.17
  def convertToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, acc) => Cons(a.toString, acc))

  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A]) { (a, acc) =>
      if (f(a)) Cons(a, acc)
      else acc
    }

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

}
