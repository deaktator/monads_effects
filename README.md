# monads_effects

This simple [Scala](http://www.scala-lang.org) project that was created after
a conversation with someone (*X*) frustrated by the way `Option`s throw away
information and make it hard to reason about program failures.

The goal is to illustrate the use of `Option` to model the computational effect
of failure and short circuiting.  It also shows a simple alternative that
encodes additional information (the right-biased Either monad).

Hopefully the reader will load the code into a REPL and play with it while
reading through the code.  The code can be read in order from top to bottom and
contains a liberal amount of comments, since it should be used as a learning
resource.

The project gives a very brief, inadequate definition of monads.  Then an
anemic `Opt` [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) (similar
to Scala's `Option`) is defined. Next a monad instance for `Opt` is derived.
Then it is used in some examples involving *for comprehensions*.  Next a monad
for `Either` is created and used in some examples.  Finally, some of the code
is refactored to derived the essence of the computations.  This allows the
inputs and outputs to not depend on the container type as long as the proper
[context bound](http://docs.scala-lang.org/tutorials/FAQ/context-bounds.html)
exists.

Hopefully this project will show the reader that `Option` is just one effect and
that many others are possible and that using monads to model effects can pay off.
This opens up the possibility of understanding a lot of really important ideas
like [monad transformers](https://en.wikipedia.org/wiki/Monad_transformer)
(*or see cats* [OptionT](https://typelevel.org/cats/datatypes/optiont.html)
*example*), the [Free monad](https://typelevel.org/cats/datatypes/freemonad.html),
extensible effects using the [Eff monad](http://atnos-org.github.io/eff/).

## Running

Assuming SBT is installed, run the following in a shell:

```bash
git clone https://github.com/deaktator/monads_effects.git
cd monads_effects
sbt console
```

Inside the SBT console run:

```
import monads._
```

## Conversation that sparked this project

> *X*:  I don’t get why we use options... <br/>
> *X*:  I really don’t see any benefit <br/>
> *X*:  and it’s making things unnecessarily harder

> *ME*:  Options are usually stupid.

> *X*:  slowing things down, obscuring things. <br/>
> *X*:  I think they are almost always stupid, at this point. <br/>
> *X*:  I wasted two weeks chasing a bug that was purely a data issue <br/>
> *X*:  and the options quietly sweep all those issues under the rug <br/>
> *X*:  I don’t get it <br/>
> *X*:  It seems so wrong to me to develop in this manner

> *ME*: I'm going to teach you some s*** tomorrow because you are smart and can handle it. <br/>
> *ME*: Then hopefully we can spread this like wild fire and we'll all be happy.
