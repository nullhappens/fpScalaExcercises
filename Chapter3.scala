package fpinscala.datastructures

//sealed trait means that all implementations of this trait (type/class/scalazfunzstuffzz) are included in this file.
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head:A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x,xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, t) => 0.0
		case Cons(x, xs) => x * product(xs)
	}

	def apply[A](as:A*): List[A] =
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

	//excercise 3.2
	def tail[A](x: List[A]) : List[A] = x match {
		case Nil => Nil
		case Cons(h, t) => t
	}

	//excercise 3.3
	def setHead[A](l: List[A], x:A): List[A] = l match {
		case Nil => Cons(x, Nil)
		case Cons(h, t) => Cons(x, t)
	}

	//excercise 3.4
	def drop[A](l: List[A], n: Int): List[A] = l match {
		case Nil => Nil
		case Cons(h, t) =>
			if (n <= 0)
				Cons(h, t)
			else
				drop(t, n-1)
	}

	//excercise 3.5
	def dropWhile[A] (l:List[A], f:A => Boolean): List[A] = l match {
		case Nil => Nil
		case Cons(h, t) =>
			if (f(h))
				dropWhile(t,f)
			else
				t
	}
}