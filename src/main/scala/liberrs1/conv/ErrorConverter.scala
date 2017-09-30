package liberrs1.conv

// Error converters from external errors to errors internal the Error ADT.
trait ErrorConverter[E, I] {
  def convertError(e: E): I
}
