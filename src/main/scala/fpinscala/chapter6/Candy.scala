package fpinscala.chapter6

// Exercise 6.11
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update: Input => Machine => Machine =
    (i: Input) => (s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    // Transform the inputs into a list of "modify" actions
    // to sequentially "update" a "Machine" and sequence it
    _ <- State.sequence(inputs map (State.modify[Machine] _ compose update))
    // Extract the final state
    s <- State.get
  } yield (s.coins, s.candies)  // Map final state S (=> Machine)
                                // to desired A (=> Int, Int) in State[S, A]

  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] =
    State.sequence(inputs map (State.modify[Machine] _ compose update)).flatMap(_ =>
      State.get.map(s =>
        (s.coins, s.candies)))
}
