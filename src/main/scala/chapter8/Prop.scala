package chapter8

import chapter6.RNG.Rand
import chapter6.{RNG, State}
import chapter8.Prop.{FailedCase, SuccessCount}

trait Prop {
  def check(): Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}

case class Gen[A](sample: State[RNG, A])
object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.nonNegativeLessThan(1)).map(_ % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

}
