package liberrs.v3.conv

import cats.Monad

import scala.language.higherKinds

object Syntax {

  implicit class PossibleErrSyntax[F[_], A](val fa: F[A]) {
    def convertTo[G[_]](implicit vc: ValueConverter[F, G]): G[A] = vc.convert(fa)
  }

  implicit class NoErrSyntax[A](val a: A) extends AnyVal {
    def liftTo[G[_]: Monad]: G[A] = Monad[G].pure(a)
  }
}
