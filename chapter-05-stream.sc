import java.awt.print.Book
import scala.collection.mutable
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, List()).reverse
  }

  def toListFast: List[A] = {
    val buf = new mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => buf.toList
      case Cons(h, t) =>
        buf += h()
        go(t())
    }
    go(this)
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def take(n: Int): Stream[A] = (this, n) match {
    case (Empty, _) => Empty
    case (_, 0) => Empty
    case (Cons(h, t), n) => Cons(h, () => t().take(n - 1))
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  @annotation.tailrec
  final def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((e, acc) =>
      if (p(e)) Stream.cons(e, acc)
      else Stream.empty)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOptionWithFoldRight: Option[A] =
    foldRight(None: Option[A])((e, _) => Some(e))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((e, acc) => Stream.cons(f(e), acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((e, acc) =>
      if (p(e)) Stream.cons(e, acc)
      else acc)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((e, acc) => Stream.cons(e, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((e, acc) => f(e) append acc)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (s, 0) => None
      case (Cons(h, t), n) => Some((h(), (t(), n - 1)))
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if (f(h())) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, bs)) {
      case (Cons(a, at), Cons(b, bt)) => Some((f(a(), b()), (at(), bt())))
      case _ => None
    }

  def zip[B](bs: Stream[B]): Stream[(A, B)] =
    zipWith(bs)((_, _))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Empty, Cons(h2, t2)) => Some((
        (None, Some(h2())),
        (Empty, t2())
      ))
      case (Cons(h1, t1), Empty) => Some((
        (Some(h1()), None),
        (t1(), Empty)
      ))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((
        (Some(h1()), Some(h2())),
        (t1(), t2())
      ))
    }

  def startsWith[A](s: Stream[A]): Boolean = (this, s) match {
    case (Cons(l, lt), Cons(r, rt)) if (l() == r()) => lt() startsWith rt()
    case (_, Empty) => true
    case _ => false
  }

  def startsWith2[A](s: Stream[A]): Boolean =
    zipAll(s) takeWhile(!_._2.isEmpty) forAll {
      case (a, b) => a == b
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(Stream.empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z))((e, acc) => Stream.cons(f(e, acc.headOption.get), acc))

  def tails2: Stream[Stream[A]] =
    scanRight(Stream.empty: Stream[A])(Stream.cons(_, _))
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      Stream.cons(a, go(b, a + b))
    go(0, 1)
  }

  val fibs2: Stream[Int] =
    unfold((0, 1))({ case (a: Int, b: Int) => Some((a, (b, b + a))) })

  def from2(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constant3[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  val ones2 = constant3(1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Empty
    }

  // unfold is an example of a corecursive function

  // recursive => consume data, corecursive => produce data. recursive
  // functions terminate by recursing on smaller inputs, corecursive functions
  // need not terminate as long as they remain productive

  // productive = can always evaluate more of the results in a finite amount of
  // time. unfold is productive as long as f terminates, since we just need to
  // run the function f one more time to generate the next element of the
  // Stream

  // corecursion == guarded recursion
  // productivity == cotermination

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

println(Stream(1, 2, 3).toList)
println(Stream(1, 2, 3, 4, 5).drop(2).toList)
println(Stream(1, 2, 3, 4, 5).take(2).toList)
println(Stream(1, 2, 3, 4, 5).take(0).toList)
println(Stream(1, 2, 3).toListFast)
println(Stream(1, 2, 3, 4, 5).drop(2).toListFast)
println(Stream(1, 2, 3, 4, 5).take(2).toListFast)
println(Stream(1, 2, 3, 4, 5).takeWhile(_ % 2 == 1).toListFast)

println(Stream(1, 3, 5).forAll(_ % 2 == 1))
println(Stream(1, 3, 6, 5).forAll(_ % 2 == 1))

println(Stream(1, 2, 3, 4, 5).takeWhileWithFoldRight(_ % 2 == 1).toListFast)

println(Stream(1).headOptionWithFoldRight)
println(Stream(1, 2, 3).headOptionWithFoldRight)
println(Stream().headOptionWithFoldRight)

println(Stream(1, 2, 3).map(_ * 2).toListFast)
println(Stream(1, 2, 3, 4, 5).filter(_ % 2 == 1).toListFast)
println(Stream(1, 2, 3).append(Stream(4, 5, 6)).toListFast)
println(Stream(1, 2, 3).flatMap(e => Stream(e, e, e)).toListFast)

println(Stream(1, 2, 3, 4, 5).find(_ % 2 == 0))

val ones: Stream[Int] = Stream.cons(1, ones)

println(ones.take(5).toListFast)
println(ones.exists(_ % 2 != 0))
println(ones.map(_ + 1).exists(_ % 2 == 0))
println(ones.takeWhile(_ == 1))
println(ones.forAll(_ != 1))

println(Stream.constant(1).forAll(_ != 1))
println(Stream.constant2(1).take(8).toListFast)
println(Stream.from(1).take(8).toListFast)
println(Stream.fibs.take(8).toListFast)

def nextState(s: Int): Option[(Int, Int)] =
  if (s >= 5) None
  else Some((s, s + 1))

println(nextState(1))
println(Stream.unfold(1)(nextState).take(8).toListFast)
println(Stream.fibs2.take(10).toListFast)
println(Stream.from2(5).take(10).toListFast)
println(Stream.constant3(5).take(10).toListFast)
println(Stream.ones2.take(10).toListFast)
println(Stream.ones2.mapViaUnfold(_ * 2).take(4).toListFast)
println(Stream.ones2.mapViaUnfold(_ * 2).takeViaUnfold(4).toListFast)
println(Stream.from(1).takeWhileViaUnfold(_ == 1).toList)

println(Stream.ones2.zipWith(Stream.constant3(2))(_ + _).take(5).toList)
println(Stream.ones2.take(5).zipAll(Stream.constant3(2).take(8)).toList)

println(Stream.from(1).take(5) startsWith Stream.from(1).take(5))
println(Stream.from(1) startsWith Stream.from(1).take(5))
println(Stream.from(2) startsWith Stream.from(1))
println(Stream.from(1).take(5) startsWith Stream.from(1))

println(Stream.from(1).take(5) startsWith2 Stream.from(1).take(5))
println(Stream.from(1) startsWith2 Stream.from(1).take(5))
println(Stream.from(2) startsWith2 Stream.from(1))
println(Stream.from(1).take(5) startsWith2 Stream.from(1))

println(Stream.from(1).take(5).tails.map(_.toList).toList)
println(Stream.from(1).take(5) hasSubsequence Stream.from(2).take(3))

println(Stream.from(1).take(3).scanRight(0)(_ + _).toList)
println(Stream.from(1).take(3).tails2.map(_.toList).toList)
