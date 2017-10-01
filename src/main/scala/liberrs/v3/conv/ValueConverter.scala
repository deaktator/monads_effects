package liberrs.v3.conv

import cats.MonadError

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

trait ValueConverter[F[_], G[_]] {
  def convert[A](fa: F[A]): G[A]
}

object ValueConverter {

  implicit def identityValueConverter[F[_]]: ValueConverter[F, F] =
    new ValueConverter[F, F] {
      override def convert[A](fa: F[A]): F[A] = fa
    }

  implicit def optionValueConverter[F[_], I](implicit
      me: MonadError[F, I],
      ec: ErrorConverter[None.type, I]): ValueConverter[Option, F] =
    new OptionValueConverter[F, I]

  implicit def tryValueConverter[F[_], I](implicit
      me: MonadError[F, I],
      ec: ErrorConverter[Throwable, I]): ValueConverter[Try, F] =
    new TryValueConverter[F, I]

  implicit def eitherValueConverter[F[_], E, I](implicit
      me: MonadError[F, I],
      ec: ErrorConverter[E, I]): ValueConverter[Either[E, ?], F] =
    new EitherValueConverter[F, E, I]


  private[this] final class TryValueConverter[F[_], I](implicit
      me: MonadError[F, I],
      ec: ErrorConverter[Throwable, I])
  extends ValueConverter[Try, F] {
    override def convert[A](tryA: Try[A]): F[A] = tryA match {
      case Failure(e) => me.raiseError(ec.convertError(e))
      case Success(s) => me.pure(s)
    }
  }

  private[this] final class EitherValueConverter[F[_], E, I](implicit
      me: MonadError[F, I],
      ec: ErrorConverter[E, I])
    extends ValueConverter[Either[E, ?], F] {
    override def convert[A](either: Either[E, A]): F[A] = either match {
      case Left(e)  => me.raiseError(ec.convertError(e))
      case Right(s) => me.pure(s)
    }
  }

  private[this] final class OptionValueConverter[F[_], I](implicit
      me: MonadError[F, I],
      ec: ErrorConverter[None.type, I])
  extends ValueConverter[Option, F] {
    override def convert[A](option: Option[A]): F[A] = option match {
      case e: None.type => me.raiseError(ec.convertError(e))
      case Some(s) => me.pure(s)
    }
  }
}