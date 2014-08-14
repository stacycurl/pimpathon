package pimpathon


object option {
  implicit def optionOps[A](option: Option[A]): OptionOps[A] = new OptionOps[A](option)

  class OptionOps[A](option: Option[A]) {
    def getOrThrow(message: String): A = getOrThrow(new NoSuchElementException(message))
    def getOrThrow(exception: Exception): A = option.getOrElse(throw exception)
  }
}
