package pimpathon


object option {
  implicit def optionPimps[A](option: Option[A]): OptionPimps[A] = new OptionPimps[A](option)

  class OptionPimps[A](option: Option[A]) {
    def tapNone[Discarded](none: ⇒ Discarded): Option[A] = tap(none, _ ⇒ {})
    def tapSome[Discarded](some: A ⇒ Discarded): Option[A] = tap({}, some)
    def tap[Discarded](none: ⇒ Discarded, some: A ⇒ Discarded): Option[A] = {option.map(some).getOrElse(none); option}
    def getOrThrow(message: String): A = getOrThrow(new NoSuchElementException(message))
    def getOrThrow(exception: ⇒ Exception): A = option.getOrElse(throw exception)
    def invert(a: A): Option[A] = option.map(_ ⇒ None: Option[A]).getOrElse(Some(a))
  }
}