package liberrs1

import liberrs1.adt.Err
import liberrs1.conv.Syntax._
import liberrs1.conv.ValueConverter
import liberrs1.lib.{DecodingFailure, ParsingFailure}

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

  private[this] def someFunction(x: Int, y: Float, z: Double): Double = x + y + z


  // Used attach the map / flatMap syntax (for the right side of the Either) for
  // use in the for comprehension.  This could easily be written ourselves as:
  //
  //   implicit class EitherSyntax[L, R](val e: Either[L, R]) extends AnyVal {
  //     def map[A](f: R => A): Either[L, A] = e.right.map(f)
  //     def flatMap[A](f: R => Either[L, A]): Either[L, A] = e.right.flatMap(f)
  //   }
  //
  import cats.syntax.either.catsSyntaxEither


  // All examples output type: Either[Err, Double].

  // --------------------------------------------------------------------------
  //  Success case
  // --------------------------------------------------------------------------
  def ex1 = for {
    p <- ps.errorTo[Err]
    d <- ds.errorTo[Err]
    t <- ts.errorTo[Err]
  } yield someFunction(p, d, t)

  // --------------------------------------------------------------------------
  //  Success case, written generically for any error type.
  // --------------------------------------------------------------------------

  /**
    * ex1 can be rewritten generically by making a simple modification to the
    * function body.  The concrete error type is replaced with a type parameter.
    * Then the function needs to be parametrized by this error type.  Finally,
    * ValueConverters for the types encountered in the for comprehension, need to
    * be included as implicit function parameters.  These are `t`, `d`, `p`.
    *
    * '''NOTE''': This function is included only so `ex2` can be run without
    * needing additional imports aside from `import Example._`.
    *
    * @param t a value converter for `Try`
    * @param p a value converter for `Either` where the error type on the left is
    *          `ParsingFailure`
    * @param d a value converter for `Either` where the error type on the left is
    *          `DecodingFailure`
    * @tparam I the internal error type.
    * @return An `Either` with the desired internal error type on the left.
    */
  private[this] def ex2Helper[I](implicit
      t: ValueConverter[Try, I],
      p: ValueConverter[Either[ParsingFailure, ?], I],
      d: ValueConverter[Either[DecodingFailure, ?], I]): Either[I, Double] =

    for {
      p <- ps.errorTo[I]
      d <- ds.errorTo[I]
      t <- ts.errorTo[I]
    } yield someFunction(p, d, t)

  def ex2 = ex2Helper[Err]


  // --------------------------------------------------------------------------
  //  Parsing Failure
  // --------------------------------------------------------------------------
  def ex3 = for {
    p <- pf.errorTo[Err]
    d <- ds.errorTo[Err]
    t <- ts.errorTo[Err]
  } yield someFunction(p, d, t)

  // --------------------------------------------------------------------------
  //  Decoding Failure
  // --------------------------------------------------------------------------
  def ex4 = for {
    p <- ps.errorTo[Err]
    d <- df.errorTo[Err]
    t <- ts.errorTo[Err]
  } yield someFunction(p, d, t)

  // --------------------------------------------------------------------------
  //  Other Failure
  // --------------------------------------------------------------------------
  def ex5 = for {
    p <- ps.errorTo[Err]
    d <- ds.errorTo[Err]
    t <- tf.errorTo[Err]
  } yield someFunction(p, d, t)

  // --------------------------------------------------------------------------
  //  Success with Option
  // --------------------------------------------------------------------------
  def ex6 = for {
    p <- ps.errorTo[Err]
    d <- ds.errorTo[Err]
    o <- os.errorTo[Err]
  } yield someFunction(p, d, o)

  // --------------------------------------------------------------------------
  //  Failure with Option
  // --------------------------------------------------------------------------
  def ex7 = for {
    p <- ps.errorTo[Err]
    d <- ds.errorTo[Err]
    o <- of.errorTo[Err]
  } yield someFunction(p, d, o)

  // --------------------------------------------------------------------------
  //  Lifting a raw value to Either[Err, ?].  This shouldn't really be necessary
  //  as the value could be just bound to a variable but if you want to do it,
  //  you could.  This should really only be necessary if you wanted to put
  //  it as the first element of the for comprehension.
  // --------------------------------------------------------------------------
  def ex8 = for {
    c <- 1.noErrorTo[Err] // c = 1 doesn't compile
    d <- ds.errorTo[Err]
    o <- os.errorTo[Err]
  } yield someFunction(c, d, o)

  // --------------------------------------------------------------------------
  //  Using "raw" values not inside an Either[Err, ?].  Fine after the first
  //  line of the for comprehension.
  // --------------------------------------------------------------------------
  def ex9 = for {
    p <- ps.errorTo[Err]
    c  = 1f
    o <- os.errorTo[Err]
  } yield someFunction(p, c, o)

  def ex10 = for {
    p <- ps.errorTo[Err]
    d <- ds.errorTo[Err]
    c  = 1d
  } yield someFunction(p, d, c)
}
