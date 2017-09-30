package liberrs.v1

import liberrs.v1.adt.ErrOr
import liberrs.v1.conv.Syntax._
import liberrs.v1.lib.{DecodingFailure, ParsingFailure}

import scala.util.{Failure, Success, Try}

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
  //  Lifting a raw value to ErrOr.  This shouldn't really be necessary as
  //  the value could be just bound to a variable but if you want to do it,
  //  you could.  This should really only be necessary if you wanted to put
  //  it as the first element of the for comprehension.
  // --------------------------------------------------------------------------
  def ex7: ErrOr[Double] = for {
    c <- 1.liftToErrOr        // p = 1 doesn't compile
    d <- ds.asErrOr
    o <- os.asErrOr
  } yield someFunction(c, d, o)

  // --------------------------------------------------------------------------
  //  Using "raw" values not inside an ErrOr.  Fine after the first line of
  //  the for comprehension.
  // --------------------------------------------------------------------------
  def ex8: ErrOr[Double] = for {
    p <- ps.asErrOr
    c  = 1f
    o <- os.asErrOr
  } yield someFunction(p, c, o)

  def ex9: ErrOr[Double] = for {
    p <- ps.asErrOr
    d <- ds.asErrOr
    c  = 1d
  } yield someFunction(p, d, c)
}
