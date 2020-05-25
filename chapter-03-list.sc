sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((e, acc) => Cons(f(e), acc))

  // Use List[A]() to help in type inference
  def reverse2[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc, e) => Cons(e, acc))

  // Or be explicit about types
  def reverse3[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc, e) => Cons(e, acc))

  def reverse[A](as: List[A]): List[A] = {
    @annotation.tailrec
    def go(as: List[A], acc: List[A]): List[A] = as match {
      case Nil => acc
      case Cons(x, xs) => go(xs, Cons(x, acc))
    }

    go(as, Nil)
  }

  def sum3(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def product3(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse2(as), z)((a, b) => f(b, a))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](ns: List[A]) =
    foldRight(ns, 0)((_, i) => i + 1)

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new Exception("Can't get tail of Nil")
    case Cons(x, xs) => xs
  }

  def setHead[A](xs: List[A], h: A): List[A] = xs match {
    case Nil => throw new Exception("Can't setHead of Nil")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](xs: List[A], n: Int): List[A] = (xs, n) match {
    case (Nil, _) => Nil
    case (l, x) if x <= 0 => l
    case (Cons(_, xs), n) => drop(xs, n - 1)
  }

  def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => xs
  }

  def append[A](as: List[A], oas: List[A]): List[A] =
    foldRight(as, oas)(Cons(_, _))

  def concatenate[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])((e, acc) => append(e, acc))

  def increment(is: List[Int]): List[Int] =
    map2(is)(a => a + 1)

  def toString[A](is: List[A]): List[String] =
    map2(is)(_.toString)

  def map2[A, B](is: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t)
    }
    go(is)
    List(buf.toList: _*)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) if (f(h)) => buf += h; go(t)
      case Cons(h, t) => go(t)
    }
    go(as)
    List(buf.toList: _*)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concatenate(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else List())

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    val buf = new collection.mutable.ListBuffer[C]
    def go(as: List[A], bs: List[B]): Unit = (as, bs) match {
      case (_, Nil) => ()
      case (Nil, _) => ()
      case (Cons(a, as), Cons(b, bs)) => buf += f(a, b); go(as, bs)
    }
    go(as, bs)
    List(buf.toList: _*)
  }

  def zipSum(as: List[Int], bs: List[Int]): List[Int] =
    zipWith(as, bs)(_ + _)

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(ph, pt)) if h == ph => startsWith(t, pt)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(a, Cons(b, Nil)) => Cons(a, Nil)
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

println(List.dropWhile(List(1,2,3,4,5))(_ < 3))
println(List.init(List(1,2,3,4,5)))
println(List.init(List(1, 2)))
println(List.init(List(1)))
println(List.init(Nil))
println(List.sum2(List(1,2,3,4,5)))
println(List.product2(List(1,2,3,4,5)))
println(List.sum3(List(1,2,3,4,5)))
println(List.product3(List(1,2,3,4,5)))
println(List.length(List(1,2,3,4,5)))
println(List.length(List()))
println(List.reverse(List()))
println(List.reverse(List(1, 2, 3)))
println(List.reverse2(List(1, 2, 3)))
println(List.append(List(1, 2, 3), List(4, 5, 6)))
println(List.concatenate(List(List(1, 2, 3), List(4, 5, 6), List(7, 8))))
println(List.increment(List(1, 2, 3)))
println(List.toString(List(1.0, 2.0, 3.0)))
println(List.filter(List(1, 2, 3, 4, 5))(a => a % 2 == 0))
println(List.flatMap(List(1, 2, 3))(a => List(a, a)))
println(List.filterViaFlatMap(List(1, 2, 3, 4, 5))(a => a % 2 == 0))
println(List.zipSum(List(1, 2, 3, 4, 5), List(1, 2, 3)))

println(List.hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2, 3)))
println(List.hasSubsequence(List(1, 2, 3, 4, 5), List(3)))
println(List.hasSubsequence(List(1, 2, 2, 3, 4), List(2, 3)))
println(List.hasSubsequence(List(1, 2, 2, 2, 4), List(2, 2, 4)))
