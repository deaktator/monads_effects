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

  private val BAD_INPUT = "bad input"

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


  // Let's come back to added two numbers like with Opt instances.

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
      b <- Left(BAD_INPUT)        : StringErrOr[Double]
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
      value <- Left(BAD_INPUT)      : StringErrOr[Int]
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


  // Notice the problem that reciprocal returns a specific type of monadic
  // value: StringErrOr[Double].  What if we want to generalize this like
  // with the addTwo function divided above.  How could that be done?
  //
  // We want something like a monad that can also encode errors.
  //
  // It would be nice if we could construct and emit an error generically.
  // We CAN do this and libraries have support for this.  For instance, the
  // typelevel cats library (https://typelevel.org/cats/) has support for this
  // in a type class called MonadError.  Let's use this.  We'll create an
  // extra type class called InjectError.  This is because the MonadError type
  // class has the following structure:
  //
  //   MonadError[F[_], E]
  //
  // So it has the typical type constructor F like Monad does, but it also has
  // an error type, E.  But not all MonadErrors have the same E type.  So, we
  // create the InjectError type class that can convert from a known error type
  // EI to one that is consumable by MonadError for the type of Monad we want.
  // This looks like the following:

  trait InjectError[EI, E] {
    def convertError(err: EI): E
  }

  // MonadError[Either[E, ?], E] is parameteric in the error type, so
  // EitherInjectError takes an E as a type parameter.  The convertError
  // function is just the identity function.

  class EitherInjectError[E] extends InjectError[E, E] {
    def convertError(e: E): E = e
  }

  // The MonadError for Option is MonadError[Option, Unit], meaning it only
  // accepts errors of type Unit.  This makes since because Option doesn't
  // maintain any information about errors (hence Option[A] is isomorphic to
  // Either[Unit, A]). Therefore, while OptionInjectError takes a type
  // parameter, E, the convertError function ignores its input and always
  // returns Unit.

  class OptionInjectError[E] extends InjectError[E, Unit] {
    def convertError(e: E): Unit = ()
  }

  // Some functions to implicitly create these InjectError instances.

  implicit def eitherInjectError[E]: InjectError[E, E] = new EitherInjectError[E]
  implicit def optionInjectError[E]: InjectError[E, Unit] = new OptionInjectError[E]

  // Here we're going to start using the typelevel cats definitions for
  // MonadError and ApplicativeError.  Note that above we said an Applicative
  // can be derived from a Monad and that a Monad is strictly more powerful
  // than an Applicative.  The same is true for MonadErrors and
  // ApplicativeErrors.

  import cats.{ApplicativeError, MonadError}

  // Here we take an Either and an implicit ApplicativeError and InjectError
  // And turn the Either into an M for which the ApplicativeError is defined.
  // This is pretty easy.  If the value, e, is a Right, we assume it is a "good"
  // value and convert it to the ApplicativeError M using the pure method.  If
  // the value is a Left, we assume it is an error and raise the error into the
  // ApplicativeError using raiseError.  Remember that different
  // ApplicativeErrors have different error types.  This is why the InjectError
  // type class was created.  If an InjectError is in scope, it can convert the
  // Left(lft) into something the ApplicativeError's raiseError method can
  // consume.

  def eitherTo[M[_], L, R, E](e: Either[L, R])(implicit
                              me: ApplicativeError[M, E],
                              ie: InjectError[L, E]): M[R] = {
    e match {
      case Right(rt) => me.pure(rt)
      case Left(lft) => me.raiseError(ie.convertError(lft))
    }
  }

  // Now, we can rewrite the reciprocal function to be generic such that it
  // can take a value inside any type constructor, M, as long as the implicit
  // MonadError and InjectError are found for that type M.  This allows us to
  // rewrite this function generically and make use of the original reciprocal
  // function defined above.
  //
  // Note that we need a MonadError rather than just an ApplicativeError because
  // the flatMap in Monad is needed and the raiseError in ApplicativeError is
  // needed downstream in the eitherTo method.
  def reciprocal1[M[_], E](value: M[Int])(implicit
                           me: MonadError[M, E],
                           ie: InjectError[String, E]): M[Double] = {

    // Note that since MonadError is an ApplicativeError it gets passed
    // implicitly.  Use the flatMap from MonadError.
    me.flatMap(value){ v => eitherTo(reciprocal(v)) }
  }

  // We do the map at the end to do the reciprocal of the reciprocal like in
  // the examples above.

  def reciprocalReciprocal[M[_], E](value: M[Int])(implicit
                                    me: MonadError[M, E],
                                    ie: InjectError[String, E]): M[Double] = {
    me.map(reciprocal1(value))(r => 1 / r)
  }


  // So now we can test these things generic implementations with some different
  // monads: the Option monad and the right biased Either monad.

  // Let's look at calculating reciprocals of Either values.

  def exReciprocal4: Either[String, Double] = {
    // Since we are using cats for the MonadError, we can specify just the
    // basic Either type.  We must provide the type ascription making this
    // Right an Either because the MonadError is defined on Either, not Right
    // and Left.
    val value: Either[String, Int] = Right(5)  // Good value.

    // We import the either instances here but could have done it above.
    // Since we are going to show examples with the Option MonadError later,
    // we want to show these in isolation.
    import cats.instances.either._
    reciprocalReciprocal(value)
  }

  def exReciprocal5: Either[String, Double] = {
    import cats.instances.either._
    val value: Either[String, Int] = Right(0)  // Good value. No reciprocal.
    reciprocalReciprocal(value)
  }

  def exReciprocal6: Either[String, Double] = {
    import cats.instances.either._
    val value: Either[String, Int] = Left(BAD_INPUT)  // Bad value.
    reciprocalReciprocal(value)
  }

  // Similarly, we can calculate reciprocals of Option values without changing
  // our reciprocal code.

  def exReciprocal7: Option[Double] = {
    import cats.instances.option._
    val value = Option(5)                       // Good value.
    reciprocalReciprocal(value)
  }

  def exReciprocal8: Option[Double] = {
    import cats.instances.option._
    val value = Option(0)                       // Good value. No reciprocal.
    reciprocalReciprocal(value)
  }

  def exReciprocal9: Option[Double] = {
    import cats.instances.option._
    val value = Option.empty[Int]               // Bad value.
    reciprocalReciprocal(value)
  }

  // So we've shown some ways that generic programming can help to capture
  // different effects like failures and errors.  We can modify our programs
  // to be more generic, thereby allowing us switch easily between some of the
  // effects we want to capture.

  // There are way more types of effects than modelling failure.  We can
  // model state, logging, failure, etc.  We can even stack these effects
  // using some more advanced techniques like Free, Eff and Monad Transformers.
}
