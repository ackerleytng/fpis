import scala.collection.mutable
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

def randomPair(rng: RNG): ((Int, Int), RNG) = {
  val (i1, rng2) = rng.nextInt
  val (i2, rng3) = rng2.nextInt
  ((i1, i2), rng3)
}

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (n, nextRng) = rng.nextInt
  val actual = if (n < 0) -(n + 1) else n
  (actual, nextRng)
}

def double(rng: RNG): (Double, RNG) = {
  val (n, r) = nonNegativeInt(rng)
  (n / (Int.MaxValue.toDouble + 1), r)
}

def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val (i, nextRng) = rng.nextInt
  val (d, r) = double(nextRng)
  ((i, d), r)
}

def doubleInt(rng: RNG): ((Double, Int), RNG) = {
  val ((i, d), r) = intDouble(rng)
  ((d, i), r)
}

def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  val (i0, r0) = double(rng)
  val (i1, r1) = double(r0)
  val (i2, r2) = double(r1)

  ((i0, i1, i2), r2)
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  val out = new mutable.ListBuffer[Int]()

  @annotation.tailrec
  def go(count: Int, rng: RNG): RNG = {
    if (count == 0) rng
    else {
      val (i, r0) = rng.nextInt
      out.append(i)
      go(count - 1, r0)
    }
  }

  val r = go(count, rng)
  (out.toList, r)
}


val rng = SimpleRNG(42)
println(rng)
val (n1, rng2) = rng.nextInt
println((n1, rng2))
val (n2, rng3) = rng2.nextInt
println((n2, rng3))
println(randomPair(rng2))
println(nonNegativeInt(rng2))
println(double(rng2))
println(intDouble(rng2))
println(ints(3)(rng2))


type Rand[+A] = RNG => (A, RNG)

val int: Rand[Int] = _.nextInt

def unit[A](a: A): Rand[A] =
  rng => (a, rng)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

def nonNegativeEven: Rand[Int] =
  map(nonNegativeInt)(i => i - i % 2)

val doubleViaMap = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, r0) = ra(rng)
    val (b, r1) = rb(r0)
    (f(a, b), r1)
  }

println(nonNegativeEven(rng))
println(doubleViaMap(rng2))

println(map2(int, double)((_, _))(rng))

def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
  map2(ra, rb)((_, _))

val randIntDouble: Rand[(Int, Double)] =
  both(int, double)

val randDoubleInt: Rand[(Double, Int)] =
  both(double, int)

println(randIntDouble(rng))
println(randDoubleInt(rng))

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

println(sequence(List(int, double(_)))(rng))

def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
  val (i, rng2) = nonNegativeInt(rng)
  val mod = i % n
  if (i + (n - 1) - mod >= 0)
    (mod, rng2)
  else nonNegativeLessThan(n)(rng)
}

println(nonNegativeLessThan(10)(rng))

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (a, nRng) = f(rng)
    g(a)(nRng)
  }

def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] = {
  flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      unit(mod)
    else
      nonNegativeLessThanViaFlatMap(i)
  }
}

println(nonNegativeLessThanViaFlatMap(10)(rng))

def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => unit(f(a)))

println(mapViaFlatMap(int)(_ * 1)(rng))
println(mapViaFlatMap(int)(_ * 2)(rng))

def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

println(map2ViaFlatMap(int, double)((_, _))(rng))

// Fixed
def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

println(rollDie(SimpleRNG(5))._1)
