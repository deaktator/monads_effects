package liberrs

/**
  * Created by deak on 9/28/17.
  */
package object adt {
  type ErrOr[+A] = Either[Err, A]
}