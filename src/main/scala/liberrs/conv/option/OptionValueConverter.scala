package liberrs.conv.option

import liberrs.adt.{Err, ErrOr, UnspecifiedError}
import liberrs.conv.ValueConverter

// Value converter for Option values
private[conv] object OptionValueConverter extends ValueConverter[Option] {
  def convert[A](optA: Option[A]): ErrOr[A] = optA match {
    case None    => Left[Err, A](UnspecifiedError)
    case Some(s) => Right(s)
  }
}
