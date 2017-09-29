package liberrs

import liberrs.adt.ErrOr
import liberrs.lib.{DecodingFailure, ParsingFailure}

import scala.util.{Failure, Success, Try}
import liberrs.conv.Syntax._

/**
  * Created by deak on 9/28/17.
  */
object Examples {

  private[this] val ps: Either[ParsingFailure, Int] = Right(1)
  private[this] val pf: Either[ParsingFailure, Int] = Left(new ParsingFailure)

  private[this] val ds: Either[DecodingFailure, Float] = Right(1f)
  private[this] val df: Either[DecodingFailure, Float] = Left(new DecodingFailure)

  private[this] val ts: Try[Double] = Success(1d)
  private[this] val tf: Try[Double] = Failure(new Exception)

  private[this] val os = Option(1d)
  private[this] val of = Option.empty[Double]

  private[this] def someFunction(x: Int, y: Float, z: Double) = x + y + z


  // Necessary to attach the map / flatMap methods needed to use the for
  // comprehension syntax.
  import cats.syntax.either.catsSyntaxEither


  // --------------------------------------------------------------------------
  //  Success case
  // --------------------------------------------------------------------------
  def ex1: ErrOr[Double] = for {
    p <- ps.asErrOr
    d <- ds.asErrOr
    t <- ts.asErrOr
  } yield someFunction(p, d, t)

  // --------------------------------------------------------------------------
  //  Parsing Failure
  // --------------------------------------------------------------------------
  def ex2: ErrOr[Double] = for {
    p <- pf.asErrOr
    d <- ds.asErrOr
    t <- ts.asErrOr
  } yield someFunction(p, d, t)

  // --------------------------------------------------------------------------
  //  Decoding Failure
  // --------------------------------------------------------------------------
  def ex3: ErrOr[Double] = for {
    p <- ps.asErrOr
    d <- df.asErrOr
    t <- ts.asErrOr
  } yield someFunction(p, d, t)

  // --------------------------------------------------------------------------
  //  Other Failure
  // --------------------------------------------------------------------------
  def ex4: ErrOr[Double] = for {
    p <- ps.asErrOr
    d <- ds.asErrOr
    t <- tf.asErrOr
  } yield someFunction(p, d, t)

  // --------------------------------------------------------------------------
  //  Success with Option
  // --------------------------------------------------------------------------
  def ex5: ErrOr[Double] = for {
    p <- ps.asErrOr
    d <- ds.asErrOr
    o <- os.asErrOr
  } yield someFunction(p, d, o)

  // --------------------------------------------------------------------------
  //  Failure with Option
  // --------------------------------------------------------------------------
  def ex6: ErrOr[Double] = for {
    p <- ps.asErrOr
    d <- ds.asErrOr
    o <- of.asErrOr
  } yield someFunction(p, d, o)

  // --------------------------------------------------------------------------
  //  Lifting a raw value to ErrOr.
  // --------------------------------------------------------------------------
  def ex7: ErrOr[Double] = for {
    p <- ps.asErrOr
    d <- ds.asErrOr
    c <- 1d.liftToErrOr
  } yield someFunction(p, d, c)
}
