package liberrs.v1.conv.either

import liberrs.v1.adt.{ErrOr, ToErr}
import liberrs.v1.conv.ValueConverter

// Value converter for Either values with a varying error type.
// To get one of these, a converter a specific for error type is needed.
private[conv] class EitherValueConverter[E](implicit val toRq: ToErr[E])
  extends ValueConverter[Either[E, ?]] {
  def convert[A](either: Either[E, A]): ErrOr[A] = either match {
    case Left(f)  => Left(toRq.convertError(f))
    case Right(s) => Right(s)
  }
}
