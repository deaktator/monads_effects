// Do `sbt console` in a shell
// in console, do `import monads._`

import scala.language.higherKinds

/**
  * This is a short explanation of modeling (computational) effects other
  * than failure and short circuiting via the Option monad.  It came about
  * after a conversation with someone (''X'') frustrated by the lack of
  * diagostic information in some codebases.
  *
  - ''X'':  I don’t get why we use options...
  - ''X'':  I really don’t see any benefit
  - ''X'':  and it’s making things unnecessarily harder
  *
  - ''ME'':  Options are usually stupid.
  *
  - ''X'':  slowing things down, obscuring things.
  - ''X'':  I think they are almost always stupid, at this point.
  - ''X'':  I wasted two weeks chasing a bug that was purely a data issue
  - ''X'':  and the options quietly sweep all those issues under the rug
  - ''X'':  I don’t get it
  - ''X'':  It seems so wrong to me to develop in this manner
  *
  - ''ME'': I'm going to teach you some s*** tomorrow because you are smart and can handle it.
  - ''ME'': Then hopefully we can spread this like wild fire and we'll all be happy.
  *
  */
package object monads {

  // We use Options to model the computational effect of failure and short
  // circuiting in programs.  This is convenient because we don't have to
  // program, necessarily explicitly, in terms of the effect but can use the
  // fact that Option has a monad instance to our advantage while programming.
  // This frees the programmer from having to check error conditions throughout
  // his programs.

  // So, what do we mean by monad?  Something like the following trait below
  // where pure and flatMap (return and >>= in Haskell) are abstract and
  // must be defined by the Monad type class instance.  Monads must also adhere
  // to 3 laws to be a monad: left identity, right identity, associativity.
  // For more information, see https://wiki.haskell.org/Monad_laws

  trait Monad[M[_]] {
    // Monad instances need to define these 2 abstract functions:

    // Given a pure a:A, lift it in to a monadic context.
    def pure[A](a: A): M[A]

    // Given a value (in a monadic context, M) and a function that takes
    // a  pure value and returns a new value in a monadic context, produce
    // a value contained in the monadic context.
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // Functor functions:
    // Functors are strictly less powerful than monads so concrete
    // implementations can be derived.
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(f andThen pure)
    // ...

    // Applicative functor functions:
    // Applicative functors are strictly less powerful than monads so
    // concrete implementations can be derived.
    def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))
    def ap[A, B](f: M[A => B])(ma: M[A]): M[B] =
      map2(f, ma)((fn, a) => fn(a))
    // ...
  }

  object Monad {
    /**
      * Convenience method so `Monad[M]` can be called.
      * @param monad
      * @return `monad`
      */
    def apply[M[_]](implicit monad: Monad[M]): Monad[M] = monad
  }

  // Provided to allow for comprehension syntax.
  // Attaches `map` and `flatMap` extension methods to `ma`.
  implicit class MonadSyntax[M[_]: Monad, A](val ma: M[A]) {
    def map[B](f: A => B): M[B] = Monad[M].map(ma)(f)
    def flatMap[B](f: A => M[B]): M[B] = Monad[M].flatMap(ma)(f)
  }

  // Create an Opt algebraic data type (ADT) that works like Option.
  // This way, we can create the monad instance for Opt to see how it works.

  sealed trait Opt[+A]
  object Opt {
    def apply[A](a: A): Opt[A] = OptSome(a)
    def none[A]: Opt[A] = OptNone
  }
  case class OptSome[+A](value: A) extends Opt[A]
  case object OptNone extends Opt[Nothing]

  implicit case object OptMonad extends Monad[Opt] {
    def pure[A](a: A): Opt[A] = OptSome(a)
    def flatMap[A, B](ma: Opt[A])(f: A => Opt[B]): Opt[B] = ma match {
      case OptSome(a) => f(a)      // If a value exists, just apply f to a.
      case OptNone    => OptNone   // Otherwise, propagate non-existence.
    }
  }

  // Doing the same thing in all cases:               |
  // add the arguments a and b.                       |
  //                                                  v

  def exAdd1: Opt[Double] =
    for (a <- Opt(1); b <- Opt(2d))           yield a + b

  def exAdd2: Opt[Double] =
    for (a  <- Opt(1); b <- Opt.none[Double]) yield a + b

  def exAdd3: Opt[Double] =
    for (a  <- Opt.none[Double]; b <- Opt(1)) yield a + b

  // We don't have any error information in Opt. Just success and failure since
  // Opt just models failure and short circuiting.

  // But it's possible to easily model errors too.  In this case, error
  // information is retained as failures occur, but the failure is still
  // propagated throughout the rest of the computation.  To do this, a new
  // monad is needed.  Enter the (right-biased) Either monad.  We call this
  // right-biased because the values passed to subsequent computations will
  // exist on the right side of the Either (if the value is present).  The
  // left side of the Either is reserved for errors.  This can still short
  // circuit the remainder of a computation, but now diagnostic error
  // information is available.

  // Either has two type parameters but the Monad trait has only unbound
  // type parameter.  So, to create an Either monad, the left type parameter
  // must be fixed while the right type parameter is allowed to remain unbound.
  // So `L` is fixed and the `Either[L, ?]` syntax means construct a new type
  // with only one unbound parameter where the `?` is.  This `?` syntax comes
  // from the kind projector compiler plugin available at:
  //   https://github.com/non/kind-projector
  // Notice this monad definition is very similar to the definition of the Opt
  // monad.
  class EitherMonad[L] extends Monad[Either[L, ?]] {
    // For pure, just place the value a Right.  This has to be a Right
    // because the `?` is on the right type parameter of Either which means
    // it's the only side whose types can vary.
    def pure[A](a: A): Either[L, A] = Right(a)
    def flatMap[A, B](ma: Either[L, A])(f: A => Either[L, B]): Either[L, B] =
      ma match {
        case Right(a) => f(a)      // If a value exists, just apply f to a.
        case Left(lt) => Left(lt)  // Otherwise, propagate the error.
      }
  }

  // A function that can produce a EitherMonad instance for any `L`.
  implicit def stringErrOrMonad[L]: Monad[Either[L, ?]] = new EitherMonad[L]

  // Create a type alias for the type of errors we want to track, in this case
  // String.  This aids the type inferencer in the compiler.
  type StringErrOr[+A] = Either[String, A]

  // Let's look at a new example.  Say we want to compute the reciprocal of
  // an integer and turn it into a double.  This is possible, but we want to
  // produce an error for the reciprocal of 0 because of division be zero.

  def reciprocal(value: Int): StringErrOr[Double] = value match {
    case 0 => Left("Can't determine reciprocal of 0.")
    case _ => Right(1d / value)
  }

  // Compute the reciprocal and then the reciprocal of the reciprocal
  // valid input value, computation succeeds
  def exReciprocal1: StringErrOr[Double] =
    for {
      value <- Right(5)             : StringErrOr[Int]
      rec   <- reciprocal(value)
      id     = 1d / rec
    } yield id

  // Compute the reciprocal and then the reciprocal of the reciprocal
  // invalid input value, computation doesn't happen
  def exReciprocal2: StringErrOr[Double] =
    for {
      value <- Left("bad input")    : StringErrOr[Int]
      rec   <- reciprocal(value)
      id     = 1d / rec
    } yield id

  // Compute the reciprocal and then the reciprocal of the reciprocal
  // valid input value, computation fails mid way
  def exReciprocal3: StringErrOr[Double] =
    for {
      value <- Right(0)             : StringErrOr[Int]
      rec   <- reciprocal(value)
      id     = 1d / rec
    } yield id


  // And again, let's come back to added two numbers like with Opt instances.

  def exAdd4: StringErrOr[Double] =
    for {
      a <- Right(1)               : StringErrOr[Int]
      b <- Right(2d)              : StringErrOr[Double]
    } yield a + b

  // Or use the monads pure method.
  def exAdd5: StringErrOr[Double] =
    for {
      a <- Monad[StringErrOr].pure(1)
      b <- Monad[StringErrOr].pure(2d)
    } yield a + b

  def exAdd6: StringErrOr[Double] =
    for {
      a <- Right(1)               : StringErrOr[Int]
      b <- Left("err")            : StringErrOr[Double]
    } yield a + b

  // Now, Opts and StringError follow the exact same pattern for adding two
  // numbers.  This can be codified in the following function: addTwo.
  // It doesn't matter how we model failure, adding two numbers SHOULD NOT
  // have to care about errors.  This is the power that generic programming
  // provides: it let's the context dictate how effects and the handling of
  // effects are modelled.  The only constraint is that the container has a
  // monad instance.

  def addTwo[M[_]: Monad](ma: M[Int], mb: M[Double]): M[Double] =
    // or we could just the applicative function method:
    // Monad[M].add2(ma, mb)((a, b) => a + b)
    for (a <- ma; b <- mb) yield a + b

  // monads.Opt[Double] = OptNone
  def exAdd7: Opt[Double] =
    addTwo(Opt(1), Opt.none[Double])

  // Provide Monad type because compiler needs a little help.
  // monads.StringErrOr[Double] = Left(No value present)
  def exAdd8: StringErrOr[Double] =
    addTwo[StringErrOr](Right(1), Left("No value present"))

  // There are way more types of effects than modelling failure.  We can
  // model state, logging, failure, etc.  We can even stack these effects
  // using some more advanced techniques.
}
