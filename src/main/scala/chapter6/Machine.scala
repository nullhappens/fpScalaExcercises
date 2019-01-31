package chapter6


// TODO: Understand this exercise one day
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyMachine {
  def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin)
    }

  // exactly the same as update
  def update2(i: Input)(m: Machine): Machine = (i, m) match {
    case (_, Machine(_, 0, _)) => m
    case (Coin, Machine(false, _, _)) => m
    case (Turn, Machine(true, _, _)) => m
    case (Coin, Machine(true, candy, coin)) =>
      Machine(locked = false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) =>
      Machine(locked = true, candy - 1, coin)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs map (State.modify[Machine] _ compose update2))
    s <- State.get
  } yield (s.coins, s.candies)
}


object MachineMain extends App {
  val m = Machine(locked = true, 5, 10)
  val inputs: List[Input] = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
  val ((coins, candies), _) = CandyMachine.simulateMachine(inputs).run(m)
  assert(coins == 14)
  assert(candies == 1)
}
