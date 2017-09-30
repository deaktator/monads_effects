package liberrs1.conv

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

/**
  * Created by deak on 9/28/17.
  */
sealed trait ValueConverter[F[_], I] {
  def convert[A](fa: F[A]): Either[I, A]
}

object ValueConverter {
  implicit def optionValueConverter[I](implicit ec: ErrorConverter[None.type, I]): ValueConverter[Option, I] =
    new OptionValueConverter[I]

  implicit def tryValueConverter[I](implicit ec: ErrorConverter[Throwable, I]): ValueConverter[Try, I] =
    new TryValueConverter[I]

  implicit def eitherValueConverter[E, I](implicit ec: ErrorConverter[E, I]): ValueConverter[Either[E, ?], I] =
    new EitherValueConverter[E, I]

  // Value converter for Either values with a varying error type.
  // To get one of these, a converter a specific for error type is needed.
  private[this] final class EitherValueConverter[E, I](implicit ec: ErrorConverter[E, I])
    extends ValueConverter[Either[E, ?], I] {
    def convert[A](either: Either[E, A]): Either[I, A] = either match {
      case Left(f)  => Left(ec.convertError(f))
      case Right(s) => Right(s)
    }
  }

  // Value converter for Option values
  private[this] final class OptionValueConverter[I](implicit ec: ErrorConverter[None.type, I])
    extends ValueConverter[Option, I] {
    def convert[A](optA: Option[A]): Either[I, A] = optA match {
      case None    => Left(ec.convertError(None))
      case Some(s) => Right(s)
    }
  }

  // Value converter for Try values
  private[this] final class TryValueConverter[I](implicit ec: ErrorConverter[Throwable, I])
  extends ValueConverter[Try, I] {
    def convert[A](tryA: Try[A]): Either[I, A] = tryA match {
      case Failure(f) => Left(ec.convertError(f))
      case Success(s) => Right(s)
    }
  }
}
