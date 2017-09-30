package liberrs1.conv

import scala.language.higherKinds

object Syntax {

  implicit class PossibleErrSyntax[F[_], A](val fa: F[A]) extends AnyVal {
    def errorTo[I](implicit c: ValueConverter[F, I]): Either[I, A] = c.convert(fa)
  }

  implicit class NoErrSyntax[A](val a: A) extends AnyVal {
    def noErrorTo[I]: Either[I, A] = Right(a)
  }
}
