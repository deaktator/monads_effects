package liberrs1

// To run some of the examples:
//
//   import liberrs1.Examples._
//   run(ex1)
//   run(ex2)
//   ...


// The only import needed use the syntax is liberrs1.conv.Syntax._
// The internal base error type should also be included.

import liberrs1.adt.Err
import liberrs1.conv.Syntax._
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
  private[this] val tf: Try[Double] = Failure(new Exception("Some random error"))

  private[this] val os = Option(1d)
  private[this] val of = Option.empty[Double]

  private[this] def someFunction(x: Int, y: Float, z: Double): Double = x + y + z


  // Used attach the map / flatMap syntax (for the right side of the Either) for
  // use in the for comprehension.  This could easily be written ourselves.  But
  // why reinvent the wheel?
  //
  //   implicit class EitherSyntax[L, R](val e: Either[L, R]) extends AnyVal {
  //     def map[A](f: R => A): Either[L, A] = e.right.map(f)
  //     def flatMap[A](f: R => Either[L, A]): Either[L, A] = e.right.flatMap(f)
  //   }
  //
  import cats.syntax.either.catsSyntaxEither


  // ------------------------------------------------------------------------
  //  Error handling can be decoupled from the rest of processing logic and
  //  side effects can be handled in greater isolation.
  // ------------------------------------------------------------------------

  // Error handling can easily be decomposed.

  private[this] def handleParsingFailure(p: ParsingFailure): Unit =
    println("A parsing failure occurred")
  private[this] def handleDecodingFailure(p: DecodingFailure): Unit =
    println("A decoding failure occurred")
  private[this] def handleUnspecifiedError(): Unit =
    println("Something went wrong")
  private[this] def handleThrown(t: Throwable): Unit =
    println(s"Something was thrown: '${t.getMessage}'.")
  private[this] def handleFailure(err: Err): Unit = {
    import adt._
    err match {
      case UnspecifiedError        => handleUnspecifiedError()
      case CirceDecodingFailure(f) => handleDecodingFailure(f)
      case CirceParsingFailure(f)  => handleParsingFailure(f)
      case WrappedThrowable(t)     => handleThrown(t)
    }
  }

  // Mimics main.
  def run(prog: => Either[Err, Double]): Unit =
    prog.fold(handleFailure, success => println(s"succeeded with value: $success"))

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


  def ex2 = {
    import liberrs1.conv.ValueConverter

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
    def ex2Helper[I](implicit
        t: ValueConverter[Try, I],
        p: ValueConverter[Either[ParsingFailure, ?], I],
        d: ValueConverter[Either[DecodingFailure, ?], I]): Either[I, Double] = {

      for {
        p <- ps.errorTo[I]
        d <- ds.errorTo[I]
        t <- ts.errorTo[I]
      } yield someFunction(p, d, t)
    }

    // Calling just requires supplying the type.
    ex2Helper[Err]
  }


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
