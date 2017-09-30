package liberrs.v1.conv.option

import liberrs.v1.adt.{Err, ErrOr, UnspecifiedError}
import liberrs.v1.conv.ValueConverter

// Value converter for Option values
private[conv] object OptionValueConverter extends ValueConverter[Option] {
  def convert[A](optA: Option[A]): ErrOr[A] = optA match {
    case None    => Left[Err, A](UnspecifiedError)
    case Some(s) => Right(s)
  }
}
