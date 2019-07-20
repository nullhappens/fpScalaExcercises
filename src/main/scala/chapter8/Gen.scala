package chapter8

import chapter5.Stream
import chapter6.{RNG, State}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(a => f(a)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => Gen.listOfN(s, this))

  // 8.10
  def unsized: SGen[A] = SGen(_ => this)

  def map2[B, C](g: Gen[B], f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] =
    this map2 (g, (a: A, b: B) => (a, b))

  // 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.nonNegativeLessThan(1)).map(_ % 2 == 0))

  def listOfN[A](size: Int, gen: Gen[A]) =
    Gen(State.sequence(List.fill(size)(gen.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception ${e.getMessage}\n" +
      s"stack trace: \n ${e.getStackTrace.mkString("\n")}"
}
