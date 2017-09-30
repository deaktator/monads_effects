package liberrs.v1.conv.try_

import liberrs.v1.adt.{ErrOr, WrappedThrowable}
import liberrs.v1.conv.ValueConverter

import scala.util.{Failure, Success, Try}

// Value converter for Try values
private[conv] object TryValueConverter extends ValueConverter[Try] {
  def convert[A](tryA: Try[A]): ErrOr[A] = tryA match {
    case Failure(f) => Left(WrappedThrowable(f))
    case Success(s) => Right(s)
  }
}
