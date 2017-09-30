package liberrs.v1.conv

import liberrs.v1.adt.{ErrOr, ToErr}
import liberrs.v1.conv.either.EitherValueConverter
import liberrs.v1.conv.option.OptionValueConverter
import liberrs.v1.conv.try_.TryValueConverter

import scala.language.higherKinds
import scala.util.Try

/**
  * Created by deak on 9/28/17.
  */
trait ValueConverter[F[_]] {
  def convert[A](fa: F[A]): ErrOr[A]
}

object ValueConverter {
  implicit def optionValueConverter: ValueConverter[Option] = OptionValueConverter
  implicit def tryValueConverter: ValueConverter[Try] = TryValueConverter
  implicit def eitherValueConverter[E: ToErr]: ValueConverter[Either[E, ?]] =
    new EitherValueConverter[E]
}
