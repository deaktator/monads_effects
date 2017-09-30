package liberrs.v2.adt

import liberrs.v2.conv.ErrorConverter
import liberrs.v2.lib.{DecodingFailure, ParsingFailure}

sealed trait Err

object Err {
  implicit object ParsingFailureToErr extends ErrorConverter[ParsingFailure, Err] {
    def convertError(parsingFailure: ParsingFailure): Err = CirceParsingFailure(parsingFailure)
  }

  implicit object DecodingFailureToErr extends ErrorConverter[DecodingFailure, Err] {
    def convertError(decodingFailure: DecodingFailure): Err = CirceDecodingFailure(decodingFailure)
  }

  implicit object ThrowableToErr extends ErrorConverter[Throwable, Err] {
    override def convertError(throwable: Throwable): Err = WrappedThrowable(throwable)
  }

  implicit object NoneToErr extends ErrorConverter[None.type, Err] {
    override def convertError(none: None.type): Err = UnspecifiedError
  }
}

case object UnspecifiedError extends Err
final case class CirceDecodingFailure(ex: DecodingFailure) extends Err
final case class CirceParsingFailure(ex: ParsingFailure) extends Err
final case class WrappedThrowable(ex: Throwable) extends Err

