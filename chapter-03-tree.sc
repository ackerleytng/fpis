sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => max(l) max max(r)
  }

  def depth(t: Tree[Int]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(agg: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => agg(fold(l)(f)(agg), fold(r)(f)(agg))
  }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l, r) => l + r + 1)

  def max2(t: Tree[Int]): Int =
    fold(t)(identity)((l, r) => l max r)

  def depth2(t: Tree[Int]): Int =
    fold(t)(_ => 1)((l, r) => (l max r) + 1)

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
}

println(Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
println(Tree.max(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
println(Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
println(Tree.map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ + 1))

println(Tree.size2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
println(Tree.max2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
println(Tree.depth2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
println(Tree.map2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ + 1))
