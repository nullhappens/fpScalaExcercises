package chapter8

case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] =
    SGen { forSize(_) map f }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      forSize(n) flatMap { f(_).forSize(n) }
    }
    SGen(g2)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))
}

object SGen {
  // 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(Gen.listOfN(_, g))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => Prop.forAll(g(i))(f))
      val prop: Prop =
        props
          .map(p =>
            Prop { (max, _, rng) =>
              p.run(max, casesPerSize, rng)
          })
          .toList
          .reduce(_ && _)
      prop.run(max, n, rng)
  }
}
