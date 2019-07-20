package chapter8

import chapter5.Stream
import chapter6.{RNG, SimpleRNG}
import chapter8.Prop.{MaxSize, TestCases}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x: Falsified    => x
    }
  }

  def tag(msg: String): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(e, c) => Falsified(s"$msg \n $e", c)
      case x               => x
    }
  }

  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x                 => x
    }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type FailedCase = String
  type MaxSize = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    Stream
      .zip(Gen.randomStream(as)(rng), Stream.from(0))
      .take(n)
      .map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(Gen.buildMsg(a, e), i)
          }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Passed => println(s" + OK, Passed $testCases tests.")
      case Falsified(failure, successes) =>
        println(s" ! Falisifed after $successes passed tests:\n $failure")
      case Proved => println(s" + OK, proved property")
    }
}
