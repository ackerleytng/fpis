import java.util.concurrent.Executors
import java.util.concurrent.Callable
import java.util.concurrent.TimeUnit
import java.util.concurrent.Future
import java.util.concurrent.ExecutorService

type Par[A] = ExecutorService => Future[A]

object Par {
  def unit[A](a: A): Par[A] =
    (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  private case class Map2Future[A, B, C](
    a: Future[A],
    b: Future[B],
    f: (A, B) => C
  ) extends Future[C] {

    @volatile var cache: Option[C] = None

    def isDone = cache.isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.MILLISECONDS.convert(timeout, units))

    private def compute(timeoutMs: Long): C = cache match {
      case Some(c) => c
      case None => {
        val start = System.currentTimeMillis
        val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
        val stop = System.currentTimeMillis
        val at = stop - start

        val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS)

        val ret = f(ar, br)
        cache = Some(ret)
        ret
      }
    }
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))(
      (pa, acc) => map2(pa, acc)(_ :: _)
    )

  def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }

  private def _sequenceBalanced[A](ps: IndexedSeq[Par[A]])
      : Par[IndexedSeq[A]] = fork {
    if (ps.isEmpty) unit(Vector())
    else if (ps.length == 1) map(ps.head)(Vector(_))
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(_sequenceBalanced(l), _sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequenceBalanced[A](ps: List[Par[A]]): Par[List[A]] =
    map(_sequenceBalanced(ps.toIndexedSeq))(_.toList)

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = {
    val pbs: List[Par[B]] = as.map(asyncF(f))
    sequence(pbs)
  }

  def filter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val l: List[Par[List[A]]] = as.map(
      a => lazyUnit(if (f(a)) List(a) else List())
    )
    map(sequence(l))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)
}

/*

 7.7

 Initial law:
 map(y)(id) == y

 Substitute y == id(y) on right side:
 map(y)(id) == id(y)

 Substitute f for id function (1):
 map(y)(f) == f(y)

 Apply (1) on itself (swap f and g):
 map(map(y)(g))(f) == f(g(y))

 Substitute f(g(_)) with (f compose g)(_):
 map(map(y)(g))(f) == (f compose g)(y)

 Apply (1) on the left side:
 map(map(y)(g))(f) == map(y)(f compose g)

 */
