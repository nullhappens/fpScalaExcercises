package chapter7

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def map2[A, B, C](a: Par[A])(b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val aa = a(es)
      val bb = b(es)
      UnitFuture(f(aa.get(), bb.get())) // Danger: does not respect timeouts
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa)(unit(()))((a, _) => f(a))

  def fork[A](a: Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call: A = a(es).get() // deadlocks for fixed size thread pools
      })

  // 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => fork(unit(f(a)))

  // 7.5
  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h)(t)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val list: List[Par[List[A]]] = as.map(asyncF { a =>
      if (f(a)) List(a) else List.empty
    })
    map(sequence(list))(_.flatten)
  }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

}

abstract class Par[A] {
  def unit(a: A): Par[A]

  def run: A

  def map2[B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

  def fork(a: => Par[A]): Par[A]

  def lazyUnit(a: => A): Par[A] = fork(unit(a))
}
