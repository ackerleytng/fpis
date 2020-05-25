import java.awt.print.Book
// RNG

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

// State

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, nS) = run(s)
      f(a) run nS
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](s: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => s.flatMap(b => State.unit(f(a, b))))
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequenceViaFoldRight[S, A](ss: List[State[S, A]])
      : State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](List()))(
      (s, acc) => s.map2(acc)(_ :: _)
    )

  def sequence[S, A](ss: List[State[S, A]])
      : State[S, List[A]] = {
    def go(ss: List[State[S, A]], s: S, acc: List[A])
        : (List[A], S) = ss match {
      case Nil => (acc.reverse, s)
      case h :: t => h.run(s) match {
	case (a, s) => go(t, s, a :: acc)
      }
    }

    State(s => go(ss, s, List()))
  }

  def sequenceViaFoldLeft[S, A](ss: List[State[S, A]])
      : State[S, List[A]] =
    ss.reverse.foldLeft(unit[S, List[A]](List()))(
      (acc, s) => s.map2(acc)(_ :: _)
    )

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}

// Type alias

type Rand[A] = State[RNG, A]

// Some states

val int: Rand[Int] = State((rng: RNG) => rng.nextInt)

val nonNegativeInt: Rand[Int] =
  int.map(n => if (n < 0) -(n + 1) else n)

val double: Rand[Double] =
  nonNegativeInt.map(_ / (Int.MaxValue.toDouble + 1))

def ints(count: Int): Rand[List[Int]] =
  State.sequence(List.fill(count)(int))

def nonNegativeLessThan(n: Int): Rand[Int] = {
  nonNegativeInt.flatMap { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      State.unit(mod)
    else
      nonNegativeLessThan(i)
  }
}

// Exercising the above

val rng = SimpleRNG(42)
println(rng)
val (n1, rng2) = rng.nextInt

println(State.unit(5).map(_ + 1) run SimpleRNG(30))
println(int.map(_ + 1) run SimpleRNG(42))
println(int run rng2)
println(nonNegativeInt run rng2)
println(double run rng2)
println((double.map2(int)((_, _))) run rng)
println(State.sequence(List(int, double)) run rng)

println(ints(5) run rng)

val ns: Rand[List[Int]] = for {
  x <- nonNegativeLessThan(5)
  y <- nonNegativeLessThan(100)
  xs <- ints(x)
} yield xs.map(_ % y)

println(ns run rng)

// Candy dispenser

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def simulateMachine(inputs: List[Input])
      : State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence(
        inputs.map(i => State.modify(
          (s: Machine) => (i, s) match {
            case (_, Machine(_, 0, _)) => s
            case (Coin, Machine(false, _, _)) => s
            case (Turn, Machine(true, _, _)) => s
            case (Coin, Machine(true, candies, coins)) =>
              Machine(false, candies, coins + 1)
            case (Turn, Machine(false, candies, coins)) =>
              Machine(true, candies - 1, coins)
          })))
      s <- State.get
    } yield (s.candies, s.coins)
}

println(Candy.simulateMachine(List(Coin, Turn, Coin, Turn)).run(
  Machine(true, 5, 0)
))
println(Candy.simulateMachine(
  List.fill(3)(List(Coin, Coin, Turn)).flatten
).run(
  Machine(true, 5, 0)
))
