package liberrs.v1.adt

import liberrs.v1.lib.{DecodingFailure, ParsingFailure}

sealed trait Err
case object UnspecifiedError extends Err
final case class CirceDecodingFailure(ex: DecodingFailure) extends Err
final case class CirceParsingFailure(ex: ParsingFailure) extends Err
final case class WrappedThrowable(ex: Throwable) extends Err
