package liberrs.v1.conv

import liberrs.v1.adt.ErrOr

import scala.language.higherKinds

/**
  * Created by deak on 9/28/17.
  */
object Syntax {

  // provides syntax: `.asErrOr`.  The converter is provided at the
  // conversion call sites.
  implicit class PossibleErrSyntax[F[_], A](val fa: F[A]) extends AnyVal {
    def asErrOr(implicit c: ValueConverter[F]): ErrOr[A] = c.convert(fa)
  }

  // provides syntax: `.liftToErrOr` for any type.  No converter is necessary
  // in this case.
  implicit class NoErrSyntax[A](val a: A) extends AnyVal {
    def liftToErrOr: ErrOr[A] = Right(a)
  }
}
