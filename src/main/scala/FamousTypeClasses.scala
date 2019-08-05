object FamousTypeClasses {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Monoid[A] {
    def add(a: A, b: A): A // asociativa add(add(a, b), c) === add(a, add(b, c))
    def zero: A // neutro: add(x, zero) === add(zero, x) === x
  }

  trait Foldable[F[_]] {
    def foldl[A, B](fa: F[A])(z: B, f: (B, A) => B): B
  }

  trait Applicative[F[_]] {
    def pure[A](a: A): F[A]
    def ap[A, B](ff: F[A=>B])(fa: F[A]): F[B]
  }

  implicit val optionApplicative = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = fa match {
      case None => None
      case Some(a) => ff.map(f => f(a))
    }
  }

  implicit val listApplicative = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      for {
        f <- ff
        a <- fa
      } yield f(a)
  }
  implicit val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val listFoldable = new Foldable[List] {
    override def foldl[A, B](fa: List[A])(z: B, f: (B, A) => B): B = fa.foldLeft(z)(f)
  }

  implicit val intMonoid = new Monoid[Int] {
    override def add(a: Int, b: Int): Int = a + b

    override def zero: Int = 0
  }

  def sum[F[_], A](fa: F[A])(implicit foldable: Foldable[F], monoid: Monoid[A]): A = {
    foldable.foldl[A, A](fa)(monoid.zero, monoid.add)
  }
}
