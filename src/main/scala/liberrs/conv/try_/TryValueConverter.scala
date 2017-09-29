package liberrs.conv.try_

import liberrs.adt.{ErrOr, WrappedThrowable}
import liberrs.conv.ValueConverter

import scala.util.{Failure, Success, Try}

// Value converter for Try values
private[conv] object TryValueConverter extends ValueConverter[Try] {
  def convert[A](tryA: Try[A]): ErrOr[A] = tryA match {
    case Failure(f) => Left(WrappedThrowable(f))
    case Success(s) => Right(s)
  }
}
