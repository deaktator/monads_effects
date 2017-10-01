package liberrs.v3.conv

trait ErrorConverter[E, I] {
  def convertError(e: E): I
}

object ErrorConverter {
  implicit def toUnitErrorConverter[E]: ErrorConverter[E, Unit] = new ToUnitErrorConverter[E]

  implicit def toThrowableErrorConverter[E]: ErrorConverter[E, Throwable] = new ToThrowableErrorConverter[E]

  implicit def identityErrorConverter[E]: ErrorConverter[E, E] = new IdentityErrorConverter[E]

  private[this] final class ToUnitErrorConverter[E] extends ErrorConverter[E, Unit] {
    override def convertError(e: E): Unit = ()
  }

  private[this] final class IdentityErrorConverter[E] extends ErrorConverter[E, E] {
    override def convertError(e: E): E = e
  }

  private[this] final class ToThrowableErrorConverter[E] extends ErrorConverter[E, Throwable] {
    override def convertError(e: E): Throwable = e match {
      case t: Throwable => t
      case notThrowable => ThrowableError(notThrowable)
    }
  }

  case class ThrowableError[E] private(cause: E) extends Throwable
}