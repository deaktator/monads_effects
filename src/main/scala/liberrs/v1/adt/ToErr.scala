package liberrs.v1.adt

import liberrs.v1.lib.{DecodingFailure, ParsingFailure}

// Error converters from external errors to errors in the Error ADT.
// This will be useful in the RQErrorOrConverter for Eithers.
sealed trait ToErr[E] {
  def convertError(e: E): Err
}

object ToErr {
  // implementations of error converters

  implicit object ParsingFailureToErr extends ToErr[ParsingFailure] {
    def convertError(e: ParsingFailure): Err = CirceParsingFailure(e)
  }

  implicit object DecodingFailureToErr extends ToErr[DecodingFailure] {
    def convertError(e: DecodingFailure): Err = CirceDecodingFailure(e)
  }
}
