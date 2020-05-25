import java.util.concurrent.Callable
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.ExecutorService


sealed trait Future[A] {
  def apply(cb: A => Unit): Unit
}

type Par[A] = ExecutorService => Future[A]

def run[A](es: ExecutorService)(p: Par[A]): A = {
  val ref = new AtomicReference[A]
  val latch = new CountDownLatch(1)
  p(es) { a => ref.set(a); latch.countDown }
  latch.await
  ref.get
}

def unit[A](a: A): Par[A] =
  es => new Future[A] {
    def apply(cb: A => Unit): Unit = cb(a)
  }

def fork[A](a: => Par[A]): Par[A] =
  es => new Future[A] {
    def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
  }

def eval(es: ExecutorService)(r: => Unit): Unit =
  es.submit(new Callable[Unit] { def call = r })
