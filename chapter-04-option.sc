case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

println(Some(1).map(_ + 5))
println((None: Option[Int]).map(_ + 5))
println(Some(1).getOrElse(2))
println((None: Option[Int]).getOrElse(2))
println(Some(1) orElse Some(2))
println((None: Option[Int]) orElse Some(2))
println(Some(1) flatMap(x => Some(x + 5)))
println((None: Option[Int]) flatMap(x => Some(x + 5)))
println(Some(1) filter(_ % 2 == 0))
println(Some(2) filter(_ % 2 == 0))
println((None: Option[Int]) filter(_ % 2 == 0))


def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None }

def mean(xs: Seq[Double]): Option[Double] =
  Try(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

println(mean(List(1.0, 2.0, 3.0)))
println(variance(List(1.0, 2.0, 3.0)))

def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a flatMap(aa => b map(bb => f(aa, bb)))

def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a.foldRight(Some(List()): Option[List[A]])((e, acc) => e match {
    case Some(v) => acc.map(v :: _)
    case None => None
  })

def sequence2[A](a: List[Option[A]]): Option[List[A]] =
  a.foldRight(Some(List()): Option[List[A]])((e, acc) => map2(e, acc)(_ :: _))

def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldRight(Some(List()): Option[List[B]])((e, acc) => acc flatMap (aa => f(e) map (_ :: aa)))

def traverse2[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldRight(Some(List()): Option[List[B]])((e, acc) => map2(f(e), acc)(_ :: _))

def sequence3[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  traverse2(a)(identity)

println(sequence(List(Some(1), Some(2))))
println(sequence(List(Some(1), None, Some(2))))
println(sequence2(List(Some(1), Some(2))))
println(sequence2(List(Some(1), None, Some(2))))
