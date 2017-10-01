package liberrs.v3

import liberrs.v3.lib.{DecodingFailure, ParsingFailure}
import liberrs.v3.conv.Syntax._
import liberrs.v3.adt.Err

import cats.Monad
import cats.instances.option.catsStdInstancesForOption
import cats.instances.either.catsStdInstancesForEither
import cats.instances.try_.catsStdInstancesForTry

// For ex2:
import liberrs.v3.conv.ValueConverter
import cats.syntax.functor.toFunctorOps
import cats.syntax.flatMap.toFlatMapOps


import scala.language.higherKinds
import scala.util.{Failure, Success, Try}


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

  type ErrOr[+A] = Either[Err, A]

  // --------------------------------------------------------------------------
  //  Success case
  // --------------------------------------------------------------------------

  def ex1o = for {
    p <- ps.convertTo[Option]
    d <- ds.convertTo[Option]
    t <- ts.convertTo[Option]
  } yield someFunction(p, d, t)

  def ex1e = for {
    p <- ps.convertTo[Either[Err, ?]]
    d <- ds.convertTo[Either[Err, ?]]
    t <- ts.convertTo[Either[Err, ?]]
  } yield someFunction(p, d, t)

  def ex1eo = for {
    p <- ps.convertTo[ErrOr]
    d <- ds.convertTo[ErrOr]
    t <- ts.convertTo[ErrOr]
  } yield someFunction(p, d, t)

  def ex1t = for {
    p <- ps.convertTo[Try]
    d <- ds.convertTo[Try]
    t <- ts.convertTo[Try]
  } yield someFunction(p, d, t)

  // --------------------------------------------------------------------------
  //  Success case, written generically for any error type.
  // --------------------------------------------------------------------------

  /**
    * ex1 can be rewritten generically by making a simple modification to the
    * function body.  The concrete output types is replaced with a type parameter.
    * Then the function needs to be parametrized by this type.  Finally,
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
    *
    *
    *
    * @param t
    * @param p
    * @param d
    * @tparam F
    * @return
    */
  private[this]def ex2Helper[F[_]](implicit
      fm: Monad[F],
      t: ValueConverter[Try, F],
      p: ValueConverter[Either[ParsingFailure, ?], F],
      d: ValueConverter[Either[DecodingFailure, ?], F]): F[Double] = {
    for {
      p <- ps.convertTo[F]
      d <- ds.convertTo[F]
      t <- ts.convertTo[F]
    } yield someFunction(p, d, t)
  }

  def ex2o  = ex2Helper[Option]
  def ex2e  = ex2Helper[Either[Err, ?]]
  def ex2eo = ex2Helper[ErrOr]
  def ex2t  = ex2Helper[Try]

  // --------------------------------------------------------------------------
  //  Parsing Failure
  // --------------------------------------------------------------------------

  def ex3o = for {
    p <- pf.convertTo[Option]
    d <- ds.convertTo[Option]
    t <- ts.convertTo[Option]
  } yield someFunction(p, d, t)

  def ex3e = for {
    p <- pf.convertTo[Either[Err, ?]]
    d <- ds.convertTo[Either[Err, ?]]
    t <- ts.convertTo[Either[Err, ?]]
  } yield someFunction(p, d, t)

  def ex3eo = for {
    p <- pf.convertTo[ErrOr]
    d <- ds.convertTo[ErrOr]
    t <- ts.convertTo[ErrOr]
  } yield someFunction(p, d, t)

  def ex3t = for {
    p <- pf.convertTo[Try]
    d <- ds.convertTo[Try]
    t <- ts.convertTo[Try]
  } yield someFunction(p, d, t)



  def ex4o = for {
    p <- 1.liftTo[Option]
    d <- ds.convertTo[Option]
    t <- ts.convertTo[Option]
  } yield someFunction(p, d, t)

  def ex4e = for {
    p <- 1.liftTo[Either[Err, ?]]
    d <- ds.convertTo[Either[Err, ?]]
    t <- ts.convertTo[Either[Err, ?]]
  } yield someFunction(p, d, t)

  def ex4eo = for {
    p <- 1.liftTo[ErrOr]
    d <- ds.convertTo[ErrOr]
    t <- ts.convertTo[ErrOr]
  } yield someFunction(p, d, t)

  def ex4t = for {
    p <- 1.liftTo[Try]
    d <- ds.convertTo[Try]
    t <- ts.convertTo[Try]
  } yield someFunction(p, d, t)
}
