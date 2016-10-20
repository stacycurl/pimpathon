import java.io.{FileOutputStream, File}
import java.nio.charset.Charset
import sbt.{Info, Pure, Task}
import scala.xml.{NodeSeq, Elem}
import scala.collection.immutable.{Map ⇒ ▶:}


object Documentation {
  def generate(version: String): Task[File] =
    Task[File](Info[File](), Pure[File](() ⇒ generateDoc(version), inline = false))

  private def generateDoc(version: String): File = {
    println("Generating documentation")

    val file: File = new File("docs/index.html")
    val fos  = new FileOutputStream(file, false)

    fos.write(template(version, Map("A" → List(
      Partial("calc", "(A ⇒ B) ⇒ B", "Applies a function",
        "12.calc(_ + 3)" → "15", " 2.calc(_ * 3)" → "6"
      ),
      Partial("|>", "(A ⇒ B) ⇒ B", "Applies a function",
        "12 |> (_ + 3)" → "15", " 2 |> (_ * 3)" → "6"
      ),
      Partial("calcIf", "Predicate[A] ⇒ (A ⇒ B) ⇒ Option[B]", "Applies a function if a predicate holds",
        "1.calcIf(_ > 1)(_ * 10)" → "None", "2.calcIf(_ > 1)(_ * 10)" → "Some(20)"
      ),
      Partial("calcUnless", "Predicate[A] ⇒ (A ⇒ B) ⇒ Option[B]", "Applies a function if a predicate does not hold",
        "1.calcUnless(_ > 1)(_ * 10)" → "Some(10)", "2.calcUnless(_ > 1)(_ * 10)" → "None"
      ),
      Partial("calcPF", "PartialFunction[A, B] ⇒ Option[B]", "Applies a partial function",
        "1 calcPF { case 2 ⇒ 20 }" → "None", "2 calcPF { case 2 ⇒ 20 }" → "Some(20)"
      ),
      Partial("transform", "PartialFunction[A, A] ⇒ A", "Applies a partial function",
        "1 transform { case 2 ⇒ 20 }" → "1", "2 transform { case 2 ⇒ 20 }" → "20"
      ),
      Partial("tapIf", "Predicate[A] ⇒ (A ⇒ Discarded)*) ⇒ A", "Performs some actions if a predicate holds",
        "1.tapIf(_ > 1)(print)" → "Does nothing", "2.tapIf(_ > 1)(print)" → "prints \"2\""
      ),
      Partial("tapUnless", "Predicate[A] ⇒ (A ⇒ Discarded)*) ⇒ A", "Performs some actions if a predicate does not hold",
        "1.tapUnless(_ > 1)(print)" → "prints \"1\"", "2.tapUnless(_ > 1)(print)" → "Does nothing"
      ),
      Partial("tapPF", "PartialFunction[A, Discarded] ⇒ A", "Performs an action",
        "1 tapPF { case 2 ⇒ println(\"two !\") }" → "Does nothing",
        "2 tapPF { case 2 ⇒ println(\"two !\") }" → "Prints \"two !\""
      ),
      Partial("attempt", "(A ⇒ B) ⇒ Try[B]", "Applies a function that can fail",
        "1.attempt(_ ⇒ sys.error(\"boom !\"))" → "Failure(Exception(\"boom !\"))", "1.attempt(_ * 2)" → "Success(2)"
      ),
      Partial("partialMatch", "PartialFunction[A, B] ⇒ Option[B]", "Applies a partial function",
        "1 partialMatch { case 2 ⇒ \"two\" }" → "None", "2 partialMatch { case 2 ⇒ \"two\" }" → "Some(\"two\")"
      ),
      Partial("lpair", "(A ⇒ B) ⇒ (B, A)", "Applies a function & retain original value",
        "1.lpair(_ * 2)" → "(2, 1)"
      ),
      Partial("rpair", "(A ⇒ B) ⇒ (A, B)", "Retain original value & applies a function",
        "1.rpair(_ * 2)" → "(1, 2)"
      ),
      Partial("filterSelf", "Predicate[A] ⇒ Option[A]", "Retain original value if a predicate holds",
        "1.filterSelf(_ > 1)" → "None", "2.filterSelf(_ > 1)" → "Some(2)"
      ),
      Partial("ifSelf", "Predicate[A] ⇒ Option[A]", "Retain original value if a predicate holds",
        "1.ifSelf(_ > 1)" → "None", "2.ifSelf(_ > 1)" → "Some(2)"
      ),
      Partial("filterNotSelf", "Predicate[A] ⇒ Option[A]", "Retain original value if a predicate does not hold",
        "1.filterNotSelf(_ > 1)" → "Some(1)", "2.filterNotSelf(_ > 1)" → "None"
      ),
      Partial("unlessSelf", "Predicate[A] ⇒ Option[A]", "Retain original value if a predicate does not hold",
        "1.unlessSelf(_ > 1)" → "Some(1)", "2.unlessSelf(_ > 1)" → "None"
      ),
      Partial("isOneOf", "A* ⇒ Boolean", "Test if value is contained in a sequence",
        "1.isOneOf(1, 2, 3)" → "true", "4.isOneOf(1, 2, 3)" → "false"
      ),
      Partial("isNotOneOf", "A* ⇒ Boolean", "Test if value is missing from a sequence",
        "1.isNotOneOf(1, 2, 3)" → "false", "4.isNotOneOf(1, 2, 3)" → "true"
      ),
      Partial("containedIn", "Set[A] ⇒ Boolean", "Test if value is contained in a set",
        "1.containedIn(Set(1, 2, 3()" → "true", "4.containedIn(Set(1, 2, 3))" → "false"
      ),
      Partial("notContainedIn", "Set[A] ⇒ Boolean", "Test if value is missing from a set",
        "1.notContainedIn(Set(1, 2, 3))" → "false", "4.notContainedIn(Set(1, 2, 3))" → "true"
      ),
      Partial("withFinally", "(A ⇒ Unit) ⇒ (A ⇒ B) ⇒ B", "Applies a function and perform a cleanup action",
        "1.withFinally(println)(_ * 2)" → "2 (and prints 1)"
      ),
      Partial("tryFinally", "(A ⇒ B) ⇒ (A ⇒ Unit) ⇒ B", "Applies a function and perform a cleanup action",
        "1.tryFinally(_ * 2)(println)" → "2 (and prints 1)"
      ),
      Partial("cond", "Predicate[A] ⇒ ((A ⇒ B), (A ⇒ B)) ⇒ B", "Applies a choice of functions depending on whether a predicate holds",
        "1.cond(_ > 1)(\"true: \" + _)(\"false: \" + _)" → "false: 1",
        "2.cond(_ > 1)(\"true: \" + _)(\"false: \" + _)" → "true: 2"
      ),
      Partial("addTo", "Growable[A] ⇒ A", "Adds value into a growable",
        "1.addTo(ints)" → "ints += 1"
      ),
      Partial("removeFrom", "Shrinkable[A] ⇒ A", "Removed value from a shrinkable",
        "1.removeFrom(ints)" → "ints -= 1"
      ),
      Partial("unfold", "(A ⇒ Option[(B, A)]) ⇒ Stream[B]", "Builds a stream by repeatedly applying a function",
        "64.unfold(i ⇒ if (i > 1) Some((i, i/2)) else None)" → "Stream(64, 32, 16, 8, 4, 2)"
      ),
      Partial("update", "(A ⇒ Discarded)* ⇒ A", "Performs some actions",
        "1.tap(println)" → "Prints 1"
      ),
      Partial("withSideEffect", "(A ⇒ Discarded)* ⇒ A", "Performs some actions",
        "1.withSideEffect(println)" → "Prints 1"
      ),
      Partial("tap", "(A ⇒ Discarded)* ⇒ A", "Performs some actions",
        "1.update(println)" → "Prints 1"
      ),
      Partial("bounded", "(A, A) ⇒ (implicit Numeric[A]) ⇒ A", "Constrain value to be between two others",
        "0.bounded(1, 3)" → "1", "2.bounded(1, 3)" → "2", "5.bounded(1, 3)" → "3"
      ),
      Partial("ensure", "( ⇒ E) ⇒ Predicate[A] ⇒ Validation[E, A]", "Validates predicate",
        "1.ensure(\"<= 1\")(_ > 1)" → "Failure(\"<= 1\")", "2.ensure(\"<= 1\")(_ > 1)" → "Success(2)"
      ),
      Partial("ensureNel", "( ⇒ E) ⇒ Predicate[A] ⇒ ValidationNel[E, A]", "",
        "1.ensureNel(\"<= 1\")(_ > 1)" → "Failure(NonEmptyList(\"<= 1\"))", "2.ensureNel(\"<= 1\")(_ > 1)" → "Success(2)"
      )
      // passes, fails
    ), "(A, B)" → List(
      Partial("addTo", "(Growable[A], Growable[B]) ⇒ (A, B)", "Adds values into growables",
        "(1, \"foo\").addTo(ints, strings)" → "ints += 1; strings += \"foo\""
      ),
      Partial("removeFrom", "(Shrinkable[A], Shrinkable[B]) ⇒ (A, B)", "Removes values from a shrinkables",
        "(1, \"foo\").removeFrom(ints, strings)" → "ints -= 1; strings -= \"foo\""
      ),
      Partial("tap", "(A ⇒ B ⇒ Discarded) ⇒ (A, B)", "Performs an action",
        "(1, \"foo\").tap(i ⇒ s ⇒ println(s\"int: $i, string: $s\")" → "Prints \"int: 1, string: foo\""
      ),
      Partial("calc", "((A, B) ⇒ C) ⇒ C", "Applies a function",
        "(1, \"foo\").calc { case (i, s) ⇒ s\"int: $i, string: $s\" }" → "\"int: 1, string: foo\""
      ),
      Partial("calcC", "(A ⇒ B ⇒ C) ⇒ C", "Applies a curried function",
        "(1, \"foo\").calc(i ⇒ s ⇒ s\"int: $i, string: $s\")" → "\"int: 1, string: foo\""
      ),
      Partial("to", "(implicit A ⇒ C, B ⇒ C) ⇒ (C, C)", "Converts all elements to a common type",
        "(123, 456.0).to[String]" → "(\"123\", \"456.0\")"
      ),
      Partial("tmap", "(A ⇒ C, B ⇒ D) ⇒ (C, D)", "Applies functions to each element",
        "(1, \"foo\").tmap(_ * 2, _.reverse)" → "(2, \"oof\")"
      )
    ), "Boolean" → List(
      Partial("asInt", "Int", "Converts to int",
        "false.asInt" → "0", "true.asInt" → "1"
      ),
      Partial("either.or", "R ⇒ L ⇒ Either[L, R]", "Construct a Right if true, Left if false",
        "false.either(1).or(\"foo\")" → "Left(\"foo\")", "true.either(1).or(\"foo\")" → "Right(1)"
      ),
      Partial("option", "A ⇒ Option[A]", "Construct a Some if true, None if false",
        "false.option(1)" → "None", "true.option(1)" → "Some(1)"
      ),
      Partial("cond", "(⇒ A, ⇒ A) ⇒ A", "Returns first value if true, second if false",
        "false.cond(123, 456)" → "456", "true.cond(123, 456)" → "123"
      ),
      Partial("implies", "Boolean ⇒ Boolean", "Logical implication",
        "false implies false" → "true", "false implies true" → "true",
        "true implies false" → "false", "true implies true" → "true"
      ),
      Partial("nor", "Boolean ⇒ Boolean", "Logical NOR",
        "false nor false" → "true", "false nor true" → "false",
        "true nor false" → "false", "true nor true" → "false"
      ),
      Partial("nand", "Boolean ⇒ Boolean", "Logical NAND",
        "false nand false" → "true", "false nand true" → "true",
        "true nand false" → "true", "true nand true" → "false"
      )
    ), "Option[A]" → List(
      Partial("getOrThrow", "String ⇒ A", "Return value or throw exception with message",
        "None.getOrThrow(\"No potatoes!\")" → "throws NoSuchElementException(\"No potatoes\")",
        "Some(\"Potatoes\").getOrThrow(\"No potatoes\")" → "\"Potatoes\""
      ),
      Partial("getOrThrow", "(⇒ Exception) ⇒ A", "Return value or throw exception",
        "None.getOrThrow(new PotatoException(\"No potatoes!\"))" → "throws PotatoException(\"No potatoes\")",
        "Some(\"Potatoes\").getOrThrow(new PotatoException(\"No potatoes\"))" → "\"Potatoes\""
      ),
      Partial("invert", "A ⇒ Option[A]", "Convert Some to None and vice versa",
        "None.invert(456)" → "Some(456)", "Some(123).invert(456)" → "None"
      ),
      Partial("toTry", "Try[A]", "Convert Some to Success and None to Failure",
        "None.toTry" → "Failure(new NoSuchElementException)",
        "Some(123).toTry" → "Success(123)"
      ),
      Partial("tap", "(⇒ Discarded, A ⇒ Discarded) ⇒ Option[A]", "Perform one action or another",
        "None.tap(println(\"none\"), i ⇒ println(s\"some: $i\"))" → "prints \"none\"",
        "Some(123).tap(println(\"none\"), i ⇒ println(s\"some: $i\"))" → "prints \"some: 123\""
      ),
      Partial("tapNone", "(⇒ Discarded) ⇒ Option[A]", "Perform action if None",
        "None.tapNone(println(\"none\"))" → "prints \"none\"",
        "Some(123).tapNone(println(\"none\"))" → "Does nothing"
      ),
      Partial("tapSome", "(A ⇒ Discarded) ⇒ Option[A]", "Perform action if Some",
        "None.tapSome(i ⇒ println(s\"some: $i\"))" → "Does nothing",
        "Some(123).tapSome(i ⇒ println(s\"some: $i\"))" → "prints \"some: 123\""
      ),
      Partial("amass", "PartialFunction[A, Option[B]] ⇒ Option[B]", "Applies partial function if Some",
        "     None amass { case 123 ⇒ Some(456) }" → "None",
        "Some(321) amass { case 123 ⇒ Some(456) }" → "None",
        "Some(123) amass { case 123 ⇒ Some(456) }" → "Some(456)"
      ),
      Partial("toSuccessNel", "(⇒ E) ⇒ ValidationNel[E, A]", "Converts Some to Success & None to FailureNel",
        "Some(1).toSuccessNel(\"fail\")" → "Success(1)",
        "None.toSuccessNel(\"fail\")" → "Failure(NonEmptyList(\"fail\"))"
      )
    ), "Option[E]" → List(
      Partial("toFailureNel", "(⇒ A) ⇒ ValidationNel[E, A]", "Converts Some to FailureNel & None to Success",
        "Some(1).toFailureNel(\"succeed\")" → "Failure(NonEmptyList(1))",
        "None.toFailureNel(\"succeed\")" → "Success(\"succeed\")"
      )
    ), "Either[L, R]" → List(
      Partial("tap", "(L ⇒ Discarded, R ⇒ Discarded) ⇒ Either[L, R]", "Perform one action or another",
        "    Left(1).tap(l ⇒ print(\"left: \" + l), r ⇒ print(\"right: \" + r))" → "Prints \"left: 1\"",
        "Right(true).tap(l ⇒ print(\"left: \" + l), r ⇒ print(\"right: \" + r))" → "Prints \"right: true\""
      ),
      Partial("tapLeft", "(L ⇒ Discarded) ⇒ Either[L, R]", "Perform action if Left",
        "    Left(1).tapLeft(l ⇒ print(\"left: \" + l))" → "Prints \"left: 1\"",
        "Right(true).tapLeft(l ⇒ print(\"left: \" + l))" → "Does nothing"
      ),
      Partial("tapRight", "(R ⇒ Discarded) ⇒ Either[L, R]", "Perform action if Right",
        "    Left(1).tap(r ⇒ print(\"right: \" + r))" → "Does nothing",
        "Right(true).tap(r ⇒ print(\"right: \" + r))" → "Prints \"right: true\""
      ),
      Partial("addTo", "(Growable[L], Growable[R]) ⇒ Either[L, R]", "Adds values into growables",
        "     Left(1).addTo(ints, strings)" → "ints += 1",
        "Right(\"foo\").addTo(ints, strings)" → "strings += \"foo\""
      ),
      Partial("removeFrom", "(Shrinkable[L], Shrinkable[R]) ⇒ Either[L, R]", "Removes values from a shrinkables",
        "     Left(1).removeFrom(ints, strings)" → "ints -= 1",
        "Right(\"foo\").removeFrom(ints, strings)" → "strings -= \"foo\""
      )
    ), "Try[A]" → List(
      Partial("fold", "(Throwable ⇒ B, A ⇒ B) ⇒ B", "Convert to B",
        "Failure(Throwable(\"boom\")).fold(_.getMessage, \"message: \" + _)" → "\"boom\"",
        "Success(123).fold(_.getMessage, \"message: \" + _)" → "\"message: 123\""
      ),
      Partial("getMessage", "Option[String]", "None if Success, Some if Failure",
        "Failure(Throwable(\"boom\")).getMessage" → "Some(\"boom\")", "Success(123).getMessage" → "None"
      ),
      Partial("toEither", "Either[Throwable, A]", "Convert to Either",
        "Failure(Throwable(\"boom\")).toEither" → "Left(Throwable(\"boom\"))",
        "Success(123).toEither" → "Right(123)"
      ),
      Partial("toDisjunction", "Throwable \\/ A", "Convert to Disjunction",
        "Failure(Throwable(\"boom\")).toDisjunction" → "-\\/(Throwable(\"boom\"))",
        "Success(123).toDisjunction" → "\\/-(123)"
      )
    ), "FilterMonadic[(K, V)]" → List(
      Partial("toMultiMap", "MultiMap[List, K, V]", "Convert to MultiMap",
        "List((1, 11), (2, 22), (1, 111)).toMultiMap[Set]" → "Map(1 → Set(11, 111), 2 → Set(22))"
      )
    ), "List[A]" → List(
      Partial("batchBy", "(A ⇒ B) → List[List[A]]", "Split into batches whenever result of function changes",
        "List(1, 2, -1, -2, 3, -3, -4).batchBy(_ >= 0)" → "List(List(1, 2), List(-1, -2), List(3), List(-3, -4))"
      ),
      Partial("countBy", "MultiMap[List, Int, A]", "Group by number of occurrences of function result",
        "List(1, 2, -1, -2, 3, -3, -4).countBy(_ >= 0)" → "Map(3 → List(1, 2, 3), 4 → List(-1, -2, -3, -4))"
      ),
      Partial("distinctBy", "(A ⇒ B) ⇒ List[A]", "Retain only the first occurrence of an element whose function result is repeated",
        "List(\"foo\", \"food\", \"oof\", \"foods\").distinctBy(_.length))" → "List(\"food\", \"foods\")"
      ),
      Partial("duplicatesBy", "(A ⇒ B) ⇒ List[A]", "Retain only those elements whose function result is repeated",
        "List(\"foo\", \"food\", \"oof\", \"foods\", \"doof\").duplicatesBy(_.length))" → "List(\"foo\", \"food\", \"oof\", \"doof\")"
      ),
      Partial("duplicates", "List[A]", "Retain only those elements which are repeated",
        "List(1, 2, 3, 1, 4, 3).duplicates" → "LIst(1, 3)"
      ),
      Partial("const", "B ⇒ List[A]", "Replace all elements with a constant value",
        "List(1, 2, 3).const(\"foo\")" → "List(\"foo\", \"foo\", \"foo\")"
      ),
      Partial("lpair", "A => B => List[(B, A)]", "Pair up each element with the result of f",
        "List(1, 2, 3).lpair(_ * 2)" → "List((2,1), (4,2), (6,3)))"
      ),
      Partial("rpair", "A => B => List[(A, B)]", "Pair up each element with the result of f",
        "List(1, 2, 3).lpair(_ * 2)" → "List((1,2), (2,4), (3,6)))"
      ),
      Partial("countWithSize", "Predicate[A] ⇒ Option[(Int, Int)]", "Count number of times a predicate passes, along with the list size",
        "Nil.countWithSize(_ >= 0)" → "None",
        "List(1, 2, -2, 3, -4, 5).countWithSize(_ >= 0)" → "Some((4, 6))"
      ),
      Partial("sizeGT", "Int ⇒ Boolean", "Determine if the list size exceeds some value",
        "List(1, 2, 3).sizeGT(2)" → "true", "List(1, 2, 3).sizeGT(3)" → "false"
      ),
      Partial("fraction", "Predicate[A] ⇒ Double", "Determine what fraction of the list passes some predicate",
        "Nil.fraction(_ >= 0)" → "Double.NaN",
        "List(1, 2, -2, 3, -4, 5).fracton(_ >= 0)" → "Some(0.667)"
      ),
      Partial("emptyTo", "List[A] ⇒ List[A]", "Replace empty list with supplied value",
        "List(1, 2).emptyTo(List(3, 4))" → "List(1, 2)",
        "Nil.emptyTo(List(3, 4))" → "List(3, 4)"
      ),
      Partial("headTail", "(A, List[A])", "The head & tail if the list is non empty, throws otherwise",
        "List(1, 2, 3).headTail" → "(1, List(2, 3))",
        "Nil.headTail" → "throws \"headTail of empty list\""
      ),
      Partial("headTailOption", "Option[(A, List[A])]", "The head & tail if the list is non empty, None otherwise",
        "List(1, 2, 3).headTailOPtion" → "Some((1, List(2, 3)))", "Nil.headTailOption" → "None"
      ),
      Partial("initOption", "Option[List[A]]", "The init if the list is non empty, None otherwise",
        "List(1, 2, 3).initOption" → "Some(List(1, 2))", "Nil.initOption" → "None"
      ),
      Partial("tailOption", "Option[List[A]]", "The tail if the list is non empty, None otherwise",
        "List(1, 2, 3).tailOption" → "Some(List(2, 3))", "Nil.tailOption" → "None"
      ),
      Partial("initLast", "(List[A], A)", "The init & last if the list is non empty, throws otherwise",
        "List(1, 2, 3).initLast" → "(List(1, 2), 3)",
        "Nil.initLast" → "throws \"initLast of empty list\""
      ),
      Partial("initLastOption", "Option[(List[A], A)]", "The init & last if the list is non empty, None otherwise",
        "List(1, 2, 3).initLastOption" → "Some((List(1, 2), 3))",
        "Nil.initLastOption" → "None"
      ),
      Partial("prefixPadTo", "(Int, A) ⇒ List[A]",
        "Prefix lists smaller than provide size with repetitions of the provided element",
          "List(1, 2).prefixPadTo(1, 99)" → "List(1, 2)",
          "List(1, 2).prefixPadTo(4, 99)" → "List(99, 99, 1, 2)"
      ),
      Partial("sharedPrefix", "List[A] ⇒ (implicit A ⇒ A ⇒ Boolean) ⇒ (List[A], List[A], List[A])",
        "Split list into parts shared with the beginning of another, along with the remainder of each",
        "List(1, 2, 3, 4).sharedPrefix(List(1, 2, 4, 3))" → "(List(1, 2), List(3, 4), List(4, 3))"
      ),
      Partial("amass", "PartialFunction[A, List[B]] ⇒ List[B]", "filter andThen flatMap",
        "List(1, 2, 3, 4).amass { case i if i % 2 == 0 ⇒ List(i, -i) }" → "List(2, -2, 4, -4)"
      ),
      Partial("calcIfNonEmpty", "(List[A] ⇒ B) ⇒ Option[B]", "Calculate result if non empty, None otherwise",
        "Nil.calcIfNonEmpty(_.length)" → "None", "List(1, 2, 3).calcifNonEmpty(_.length)" → "Some(3)"
      ),
      Partial("mapIfNonEmpty", "(A ⇒ B) ⇒ Option[List[B]]", "Map if non empty, None otherwise",
        "Nil.mapIfNonEmpty(_ * 10)" → "None", "List(1, 2, 3).mapIfNonEmpty(_ * 10)" → "Some(List(10, 20, 30))"
      ),
      Partial("onlyDisjunction", "List[A] \\/ A", "\\/- if list has 1 element, -\\/ otherwise",
        "Nil.onlyDisjunction" → "-\\/(Nil)", "List(1).onlyDisjunction" → "\\/-(1)", "List(1, 2).onlyDisjunction" → "-\\/(List(1, 2))"
      ),
      Partial("uncons", "(⇒ B, List[A] ⇒ B) ⇒ B", "Convert to B depending on whether the list is empty or not",
        "          Nil.uncons(\"[]\", l ⇒ s\"[${l.length} elements]\"" → "\"[]\"",
        "List(1, 2, 3).uncons(\"[]\", l ⇒ s\"[${l.length} elements]\"" → "\"[3 elements]\""
      ),
      Partial("unconsC", "(⇒ B, A ⇒ List[A] ⇒ B) ⇒ B", "Convert to B with head & tail if non empty, default otherwise",
        "          Nil.unconsC(\"[]\", h ⇒ t ⇒ s\"[$h .. ${t.length} more]\"" → "\"[]\"",
        "List(1, 2, 3).unconsC(\"[]\", h ⇒ t ⇒ s\"[$h .. ${t.length} more]\"" → "\"[1, 2 more]\""
      ),
      Partial("unsnocC", "(⇒ B, List[A] ⇒ A ⇒ B) ⇒ B", "Convert to B with init & last if non empty, default otherwise",
        "          Nil.unsnocC(\"[]\", i ⇒ l ⇒ s\"[${i.length} previous, $l]\"" → "\"[]\"",
        "List(1, 2, 3).unsnocC(\"[]\", i ⇒ l ⇒ s\"[${i.length} previous, $l]\"" → "\"[2 previous, 3]\""
      ),
      Partial("tapNonEmpty", "List[A] ⇒ Discarded", "Perform an action if the list is non empty",
        "    Nil.tapNonEmpty(l ⇒ print(\"non-empty: \" + l))" → "Does nothing",
        "List(1).tapNonEmpty(l ⇒ print(\"non-empty: \" + l))" → "Prints \"non-empty: List(1)\""
      ),
      Partial("tapEmpty", "(⇒ Discarded) ⇒ List[A]", "Perform an action if the list is empty",
        "    Nil.tapEmpty(print(\"empty\"))" → "Prints \"empty\"",
        "List(1).tapEmpty(print(\"empty\"))" → "Does nothing"
      ),
      Partial("tap", "(⇒ Discarded, List[A] ⇒ Discarded) ⇒ List[A]",
        "Perform an action depending on whether the list is empty or not",
        "    Nil.tap(print(\"empty\"), l ⇒ print(\"non-empty: \" + l))" → "Prints \"empty\"",
        "List(1).tap(print(\"empty\"), l ⇒ print(\"non-empty: \" + l))" → "Prints \"non-empty: List(1)\""
      ),
      Partial("zipWith", "List[B] ⇒ ((A, B) ⇒ C) ⇒ List[C]", "Combine with another list element-wise",
        "List(1, 2).zipWith(List('z', 'x')) { case (i, c) ⇒ s\"i: $i, c: $c\"}" → "List(\"i: 1, c: z\", \"i: 2, c: x\")",
        "   List(1).zipWith(List('z', 'x')) { case (i, c) ⇒ s\"i: $i, c: $c\"}" → "List(\"i: 1, c: z\")",
        "List(1, 2).zipWith(List('z'))      { case (i, c) ⇒ s\"i: $i, c: $c\"}" → "List(\"i: 1, c: z\")"
      ),
      Partial("zipToMap", "List[V] ⇒ Map[A, V]", "Combine with another list element-wise to form a Map",
        "List(1, 2).zipToMap(List('z', 'x'))" → "Map(1 → 'z', 2 → 'x')",
        "   List(1).zipToMap(List('z', 'x'))" → "Map(1 → 'z')",
        "List(1, 2).zipToMap(List('z'))     " → "Map(1 → 'z')"
      ),
      Partial("zipExact", "List[B] ⇒ (List[(A, B)], Option[Either[List[A], List[B]]])",
        "Losslessly combine with another list element-wise",
          "List(1, 2).zipExact(List('z', 'x'))" → "(List((1, 'z'), (2, 'x')), None)",
          "   List(1).zipExact(List('z', 'x'))" → "(List((1, 'z')), Some(Right('x')))",
          "List(1, 2).zipExact(List('z'))     " → "(List((1, 'z')), Some(Left(1)))"
      ),
      Partial("zipExactWith", "List[B] ⇒ ((A, B) ⇒ C) ⇒ (List[C], Option[Either[List[A], List[B]]])",
        "Losslessly combine with another list element-wise",
        "List(1, 2).zipExactWith(List(10.0, 20.0))(_ + _)" → "(List(11.0, 22.0), None)",
        "   List(1).zipExactWith(List(10.0, 20.0))(_ + _)" → "(List(11.0), Some(Right(20.0))",
        "List(1, 2).zipExactWith(List(10.0      ))(_ + _)" → "(List(11.0), Some(Left(2)))"
      ),
      Partial("sortPromoting", "A* ⇒ (implicit Ordering[A]) ⇒ List[A]",
        "When sorting promote specified elements, otherwise use existing ordering",
        "List(\"red\", \"green\", \"blue\", \"cyan\").sortPromoting(\"cyan\", \"green\")" → "List(\"cyan\", \"green\", \"blue\", \"red\")"
      ),
      Partial("sortDemoting", "A* ⇒ (implicit Ordering[A]) ⇒ List[A]",
        "When sorting demote specified elements, otherwise use existing ordering",
        "List(\"blue\", \"red\", \"cyan\", \"green\").sortDemoting(\"green\", \"blue\")" → "List(\"cyan\", \"red\", \"green\", \"blue\")"
      ),
      Partial("toNel", "Option[NonEmptyList[A]]", "Convert to non-empty list, if possible",
        "Nil.toNel" → "None", "List(1, 2).toNel" → "Some(NonEmptyList(1, 2))"
      )
    ), "List[(A, B)" → List(
      Partial("mapC", "A => B => C => List[C]", "Curried map method",
        "List((1, 2), (2, 3)).mapC(a ⇒ b ⇒ a * b)" → "List(2, 6)"
      )
    ), "List[List[A]]" → List(
      Partial("cartesianProduct", "List[List[A]]", "Every permutation of elements choosen from each list",
        "List(List(1,2,3), List(8,9)).cartesianProduct" → "List(List(1,8), List(1,9), List(2,8), List(2,9), List(3,8), List(3,9))"
      )
    ), "Set[A]" → List(
      Partial("notContains", "A ⇒ Boolean", "Tests if some element is absent in this set",
        "Set(1, 2).notContains(1)" → "false", "Set(1, 2).notContains(3)" → "true"
      ),
      Partial("mutable", "mutable.Set[A]", "Convert to mutable set"),
      Partial("toMutable", "mutable.Set[A]", "Convert to mutable set"),
      Partial("powerSet", "Set[Set[A]]", "All possble subsets of the set",
        "Set(1, 2, 3).powerSet" → "Set(1, 2, 3), Set(1, 2), Set(1, 3), Set(2, 3), Set(1), Set(2), Set(3), Set())"
      )
    ), "Stream[A]" → List(
      Partial("tailOption", "Option[Stream[A]]", "The tail of the stream if it is not empty, None otherwise",
        "Stream.empty[Int].tailOption" → "None"
      ),
      Partial("uncons", "(⇒ B, Stream[A] ⇒ B) ⇒ B", "Convert to B depending on whether the stream is empty or not",
        "   Stream.empty.uncons(\"[]\", s ⇒ s\"[${s.length} elements]\"" → "\"[]\"",
        "Stream(1, 2, 3).uncons(\"[]\", s ⇒ s\"[${s.length} elements]\"" → "\"[3 elements]\""
      ),
      Partial("unconsC", "(⇒ B, A ⇒ (⇒ Stream[A]) ⇒ B) ⇒ B", "Convert to B with head & tail if non empty, default otherwise",
        "   Stream.empty.unconsC(\"[]\", h ⇒ t ⇒ s\"[$h .. ${t.length} more]\"" → "\"[]\"",
        "Stream(1, 2, 3).unconsC(\"[]\", h ⇒ t ⇒ s\"[$h .. ${t.length} more]\"" → "\"[1, 2 more]\""
      ),
      Partial("lazyScanLeft", "B ⇒ ((B, A) ⇒ B) ⇒ Stream[B]", "scanLeft that works on infinite or blocking streams",
        "(1 #:: {synchronized(wait(0)); Stream(2)}).lazyScanLeft(0)(_ + _).take(1).toList" → "List(1)"
      ),
      Partial("reverseInits", "Stream[Stream[A]]", "inits that works on infinite or blocking streams",
        "Stream.iterate(0)(_ + 1).reverseInits.take(3)" → "Stream(Stream.empty, Stream(0), Stream(0, 1))"
      )
    ), "Array[A]" → List(
      Partial("copyTo", "(Int, Array[A], Int, Int) ⇒ Array[A]", "Copy subset of an array to another",
        "Array(1, 2, 3, 4, 5).copyTo(2, Array(0, 0, 0, 0, 0), 1, 3)" → "Array(0, 3, 4, 5, 0)"
      )
    ), "Array[Byte]" → List(
      Partial("toHex", "String", "Convert to hex string",
        "Array[Byte](126, 87, -85, 30).toHex" → "7e57ab1e"
      ),
      Partial("toHex", "Int ⇒ String", "Convert to hex & prefix up to a certain length",
        "Array[Byte](126, 87, -85, 30).toHex(10)" → "007e57ab1e"
      ),
      Partial("copyUpToN", "(Long, InputStream, OutputStream) ⇒ Int",
        "Use array as buffer to copy up to n bytes from an InputStream to an OutputStream"
      ),
      Partial("readUpToN", "(Long, InputStream) ⇒ Int", "Read up to n bytes from an InputStream")
    ), "GTL[A]" → List(
      Partial("collectHistogram", "PartialFunction[A, B] ⇒ Map[B, Int]",
        "Calculate how many occurences of a property of each element",
          """|List("foo", "food", "bar", "oo").collectAttributeCounts {
             |  case word if word.size > 2 ⇒ word.size
             |}""" → """|// 2 words of length 3, 1 of length 4
                        |Map(3 → 2, 4 → 1)"""
      ),
      Partial("optHistogram", "(A ⇒ Option[B]) ⇒ Map[B, Int]",
        "Calculate how many occurences of a property of each element",
          """|List("foo", "food", "bar", "oo").optAttributeCounts(
             |  word ⇒ if (word.size <= 2) None else Some(word.size)
             |)""" → """|// 2 words of length 3, 1 of length 4
                        |Map(3 → 2, 4 → 1)"""
      ),
      Partial("histogram", "(A ⇒ B) ⇒ Map[B, Int]",
        "Calculate how many occurences of a property of each element",
          """|List("foo", "food", "bar", "oo").attributeCounts(
             |  word ⇒ word.size
             |)""" →
          """|// 1 word length 1, 2 of length 3, 1 of length 4
             |Map(2 → 1, 3 → 2, 4 → 1)"""
      ),
      Partial("onlyOption", "Option[A]", "Head if gtl has 1 element, None otherwise",
        "Nil.onlyOption" → "None", "Set(1).onlyOption" → "Some(1)", "Vector(1, 2).onlyOption" → "None"
      ),
      Partial("onlyOrThrow", "(CC[A] ⇒ Exception) ⇒ A", "Head if list gtl 1 element, throws otherwise",
        "Nil.onlyOrThrow(l ⇒ new Throwable(\"Not singleton: \"l.length)" → "throws \"Not singleton: 0\"",
        "Set(1).onlyOrThrow(s ⇒ new Throwable(\"Not singleton: \"s.length)" → "1",
        "Vector(1, 2).onlyOrThrow(v ⇒ new Throwable(\"Not singleton: \"v.length)" → "throws \"Not singleton: 2\""
      ),
      Partial("onlyEither", "Either[CC[A], A]", "Right if gtl has 1 element, Left otherwise",
        "Nil.onlyEither" → "Left(Nil)", "Set(1).onlyEither" → "Right(1)", "Vector(1, 2).onlyEither" → "Left(Vector(1, 2))"
      ),
      Partial("asMap.withEntries", "(A ⇒ (K, V)) ⇒ Map[K, V]", "Build a map by specifying entries",
        "List(2, 4).asMap.withEntries(i ⇒ (i/2, i*2))" → "Map(1 → 4, 2 → 8)"
      ),
      Partial("asMap.withEntries", "(A ⇒ K, A ⇒ V) ⇒ Map[K, V]", "Build a map by specifying keys & values",
        "List(2, 4).asMap.withEntries(i ⇒ i/2, i ⇒ i*2)" → "Map(1 → 4, 2 → 8)"
      ),
      Partial("asMap.withEntries", "(A ⇒ K1, A ⇒ K2, A ⇒ V) ⇒ Map[K1, Map[K2, V]]",
        "Build a nested map by specifying keys & values",
          "List(2, 4).asMap.withEntries(i ⇒ i/2, i ⇒ i, i ⇒ i*2)" → "Map(1 → Map(2 → 4), 2 → Map(4 → 8))"
      ),
      Partial("asMap.withEntries", "(A ⇒ K1, A ⇒ K2, A ⇒ K3, A ⇒ V) ⇒ Map[K1, Map[K2, Map[K3, V]]]",
        "Build a nested map by specifying keys & values",
          """List(2, 4).asMap.withEntries(
          |  i ⇒ i/2, i ⇒ i, i ⇒ i*10, i ⇒ i*2
          |)""" → """|Map(
                     |  1 → Map(2 → Map(20 → 4)),
                     |  2 → Map(4 → Map(40 → 8))
                     |)"""
      ),
      Partial("asMap.withSomeEntries", "(A ⇒ Option[(K, V)]) ⇒ Map[K, V]",
        "Build a map by optionally specifying entries",
          "List(2, 4).asMap.withSomeEntries(i ⇒ if (i == 2) Some((i/2, i*2)) else None)" → "Map(1, 4)"
      ),
      Partial("asMap.withSomeEntries", "(A ⇒ Option[K], A ⇒ Option[V]) ⇒ Map[K, V]",
        "Build a map by optionally specifying keys & values",
          """List(3, 5, 15).asMap.withSomeEntries(
           |  i ⇒ if (i%3 == 0) Some(i/3) else None,
           |  i ⇒ if (i%5 == 0) Some(i/5) else None
           |)""" → "Map(5 → 3)"
      ),
      Partial("asMap.withPFEntries", "PartialFunction[A, (K, V)] ⇒ Map[K, V]",
        "Build a map by partially specifying entries",
          "List(2, 4).asMap.withPFEntries { case 2 ⇒ (22, 222) }" → "Map(22 → 222)"
      ),
      Partial("asMap.withPFEntries", "(PartialFunction[A, K], PartialFunction[A, V]) ⇒ Map[K, V]",
        "Build a map by partially specifying keys & values",
          """List(1, 2, 3).asMap.withPFEntries({
            |  case 1 ⇒ 11
            |  case 3 ⇒ 33
            |}, {
            |  case 2 ⇒ 222
            |  case 3 ⇒ 333
            |})""" → "Map(33 → 333)"
      ),
      Partial("asMultiMap.withEntries", "(A ⇒ (K, V)) ⇒ MultiMap[GTL, [K, V]",
        "Build a multi map by specifying entries",
          "List(1, 2, 3).asMultiMap[List].withEntries(i ⇒ (i%2, i))" → "Map(0 → List(2), 1 → List(1, 3))",
          " List(1, 2, 3).asMultiMap[Set].withEntries(i ⇒ (i%2, i))" → "Map(0 → Set(2),  1 → Set(1, 3))"
      ),
      Partial("asMultiMap.withEntries", "(A ⇒ K, A ⇒ V) ⇒ MultiMap[GTL, K, V]",
        "Build a multi map by specifying keys & values",
          "List(1, 2, 3).asMultiMap[List].withEntries(i ⇒ i%2, i ⇒ i))" → "Map(0 → List(2), 1 → List(1, 3))",
          " List(1, 2, 3).asMultiMap[Set].withEntries(i ⇒ i%2, i ⇒ i))" → "Map(0 → Set(2),  1 → Set(1, 3))"
      ),
      Partial("asMultiMap.withEntries", "(A ⇒ K1, A ⇒ K2, A ⇒ V) ⇒ Map[K1, MultiMap[GTL, K2, V]]",
        "Builds a nested multi map by specifying keys & values",
              """|List(1, 2, 3, 4).asMultiMap[List].withEntries(
                 |  i ⇒ i%2, i ⇒ i/2, i ⇒ i
                 |)""" →
            """|Map(
               |  0 -> Map(1 -> List(2), 2 -> List(4)),
               |  1 -> Map(0 -> List(1), 1 -> List(3))
               |)"""
      ),
      Partial("asMultiMap.withEntries", "(A ⇒ K1, A ⇒ K2, A ⇒ K3, A ⇒ V) ⇒ Map[K1, Map[K2, MultiMap[GTL, K3, V]]]",
        "Builds a nested multi map by specifying keys & values...",
          """|List(1, 2, 3, 4).asMultiMap[List].withEntries(
             |  i ⇒ i%2, i ⇒ i/2, i ⇒ i*10, i ⇒ i
             |)""" →
              """|Map(
                 |  0 -> Map(2 -> Map(40 -> List(4)), 1 -> Map(20 -> List(2))),
                 |  1 -> Map(1 -> Map(30 -> List(3)), 0 -> Map(10 -> List(1)))
                 |)""",
          """|List(1, 2, 3, 4).asMultiMap[Set].withEntries(
             |  i ⇒ i%2, i ⇒ i/2, i ⇒ i*10, i ⇒ i
             |)""" →
            """|Map(
               |  0 -> Map(2 -> Map(40 -> Set(4)), 1 -> Map(20 -> Set(2))),
               |  1 -> Map(1 -> Map(30 -> Set(3)), 0 -> Map(10 -> Set(1)))
               |)"""
        ),
      Partial("asMultiMap.withSomeEntries", "(A ⇒ Option[(K, V)]) ⇒ MultiMap[GTL, K, V]", ""),
      Partial("asMultiMap.withSomeEntries", "(A ⇒ Option[K], A ⇒ Option[V]) ⇒ MultiMap[GTL, K, V]", ""),
      Partial("asMultiMap.withPFEntries", "PartialFunction[A, (K, V)] ⇒ MultiMap[GTL, K, V]", ""),
      Partial("asMultiMap.withPFEntries", "(PartialFunction[A, K], PartialFunction[A, V]) ⇒ MultiMap[GTL, K, V]", ""),
      Partial("ungroupBy", "(A ⇒ B) ⇒ GTL[GTL[A]]", ""),
      Partial("partitionByPF", "PartialFunction[A, B] ⇒ (GTL[A], GTL[B])", ""),
      Partial("all", "A ⇒ Boolean", ""),
      Partial("none", "A ⇒ Boolean", ""),
      Partial("seqFold", "B ⇒ ((B, A) ⇒ Option[B]) ⇒ Option[B]", ""),
      Partial("seqMap", "(A ⇒ Option[B]) ⇒ (implicit CanBuildFrom[Nothing, B, To]) ⇒ Option[To]", ""),
      Partial("apoFold", "B ⇒ ((B, A) ⇒ Either[C, B]) ⇒ Either[C, B]", "")
    ), "GTL[V]" → List(
      Partial("asMap.withKeys",            "(V ⇒ K) ⇒ Map[K, V]", ""),
      Partial("asMap.withSomeKeys",        "(V ⇒ Option[K]) ⇒ Map[K, V]", ""),
      Partial("asMap.withManyKeys",        "(V ⇒ List[K]) ⇒ Map[K, V]", ""),
      Partial("asMap.withUniqueKeys",      "(V ⇒ K) ⇒ Option[Map[K, V]]", ""),
      Partial("asMap.withPFKeys",          "PartialFunction[V, K] ⇒ Map[K, V]", ""),
      Partial("asMultiMap.withKeys",       "(V ⇒ K) ⇒ MultiMap[GTL, K, V]", ""),
      Partial("asMultiMap.withSomeKeys",   "(V ⇒ Option[K]) ⇒ MultiMap[GTL, K, V]", ""),
      Partial("asMultiMap.withManyKeys",   "(V ⇒ List[K]) ⇒ MultiMap[GTL, K, V]", ""),
      Partial("asMultiMap.withUniqueKeys", "(V ⇒ K) ⇒ Option[MultiMap[GTL, K, V]]", ""),
      Partial("asMultiMap.withPFKeys",     "PartialFunction[V, K] ⇒ MultiMap[GTL, K, V]", "")
    ), "GTL[K]" → List(
      Partial("asMap.withValues", "(K ⇒ V) ⇒ Map[K, V]", ""),
      Partial("asMap.withSomeValues", "(K ⇒ Option[V]) ⇒ Map[K, V]", ""),
      Partial("asMap.withPFValues", "PartialFunction[K, V] ⇒ Map[K, V]", ""),
      Partial("asMap.withConstValue", "V ⇒ Map[K, V]", ""),
      Partial("asMultiMap.withValues", "(K ⇒ V) ⇒ MultiMap[GTL, K, V]", ""),
      Partial("asMultiMap.withSomeValues", "(K ⇒ Option[V]) ⇒ MultiMap[GTL, K, V]", ""),
      Partial("asMultiMap.withPFValues", "PartialFunction[K, V] ⇒ MultiMap[GTL, K, V]", ""),
      Partial("asMultiMap.withConstValue", "V ⇒ MultiMap[GTL, K, V]", "")
      // as[F]
    ), "GTL[(K, V)]" → List(
      Partial("toMultiMap", "MultiMap[F, K, V]", "")
    ), "GTL[Either[L, R]]" → List(
      Partial("partitionEithers", "(GTL[L], GTL[R])", "Partition eithers into lefts & rights",
        "List(Right(3.0), Left(1), Right(4.0), Left(1)).partitionEithers" → "(List(1, 1), List(3.0, 4.0))"
      )
    ), "GTL[L \\/ R]" → List(
      Partial("partitionDisjunctions", "(GTL[L], GTL[R])", "Partition disjunctions into lefts & rights",
        "List(\\/-(3.0), -\\/(1), \\/-(4.0), -\\/(1)).partitionDisjunctions" → "(List(1, 1), List(3.0, 4.0))"
      )
    ), "Map[K, V]" → List(
      Partial("getOrThrow", "(K, String) ⇒ V", "Retrieve value associated with key or throw exception with message",
        "Map(1 → 2).getOrThrow(1, \"Alas, no 1\")" → "2",
        "Map(2 → 3).getOrThrow(1, \"Alas, no 1\")" → "throw new IllegalArgumentException(\"Alas no 1\")"
      ),
      Partial("getOrThrow", "(K, ⇒ Exception) ⇒ V", "Retrieve value associated with key or throw exception",
        "Map(1 → 2).getOrThrow(1, new Exception(\"No such element\"))" → "2",
        "Map(2 → 3).getOrThrow(1, new Exception(\"No such element\"))" → "throw new Exception(\"No such element\")"
      ),
      Partial("mapKeysEagerly", "(K ⇒ C) ⇒ Map[C, V]", "Eagerly applies a function to each key",
        "Map(1 → 2, 2 → 3).mapKeysEagerly(_ * 10)" → "Map(10 → 2, 20 → 3)"
      ),
      Partial("mapValuesEagerly", "(V ⇒ W) ⇒ Map[K, W]", "Eagerly applies a function to each value",
        "Map(1 → 2, 2 → 3).mapValuesEagerly(_ * 10)" → "Map(1 → 20, 2 → 30)"
      ),
      Partial("mapEntries", "(K ⇒ V ⇒ (C, W)) ⇒ Map[C, W]", "Applies a function to each entry, result must be a Tuple2",
        "Map(1 → 2, 2 → 3).mapEntries(k ⇒ v ⇒ (k * 10, v + 0.5))" → "Map(10 → 2.5, 20 → 3.5)"
      ),
      Partial("seqMapKeys", "(K ⇒ Option[C]) ⇒ Option[Map[C, V]]", "Eagerly applies a function to each key, produces a Map if it never equals None",
        "Map(2 → 4, 4 → 6).seqMapKeys(k ⇒ (k % 2 == 0).option(k / 2)))" → "Some(Map(1 → 4, 2 → 6))",
        "       Map(1 → 3).seqMapKeys(k ⇒ (k % 2 == 0).option(k / 2)))" → "None"
      ),
      Partial("seqMapValues", "(V ⇒ Option[W]) ⇒ Option[Map[K, W]]", "Eagerly applies a function to each value, produces a Map if it never equals None",
        "Map(2 → 4, 4 → 6).seqMapValues(v ⇒ (v % 2 == 0).option(v / 2)))" → "Some(Map(2 → 2, 4 → 3))",
        "       Map(1 → 3).seqMapValues(v ⇒ (v % 2 == 0).option(v / 2)))" → "None"
      ),
      Partial("seqMapEntries", "(K ⇒ V ⇒ Option[(C, W)]) ⇒ Option[Map[C, W]]", "Applies a function to each entry, produces a Map if it never equals None",
        "Map(2 → 4, 4 → 6).seqMapEntries(k ⇒ v => (k % 2 == 0).option((k / 2) -> (v / 2)))" → "Some(Map(1 → 2, 2 → 3))",
        "       Map(1 → 3).seqMapEntries(k ⇒ v => (k % 2 == 0).option((k / 2) -> (v / 2)))" → "None"
      ),
      Partial("collectKeys", "PartialFunction[K, C] ⇒ Map[C, V]", "Applies a partial function to each key",
        "Map(1 → 2, 2 → 3).collectKeys { case 2 → 20 }" → "Map(20 → 3)" // TODO [28 Oct 2015] Remove this or updateKeys
      ),
      Partial("updateKeys", "PartialFunction[K, C] ⇒ Map[C, V]", "Applies a partial function to each key",
        "Map(1 → 2, 2 → 3).updateKeys { case 2 → 20 }" → "Map(20 → 3)"
      ),
      Partial("updateKeys", "(K ⇒ Option[C]) ⇒ Map[C, V]", "Applies a function to each key, retains only Somes",
        "Map(1 → 2, 2 → 3).updateKeys(k ⇒ if (k == 2) Some(20) else None)" → "Map(20 → 3)"
      ),
      Partial("updateValue", "(K, V ⇒ Option[V]) ⇒ Map[K, V]", "Update or remove a single entry",
        "Map(1 → 2, 2 → 3).updateValue(1, _ ⇒ None)" → "Map(2 → 3)"
      ),
      Partial("collectValues", "PartialFunction[V, W] ⇒ Map[K, W]", "Applies a partial function to each value",
        "Map(1 → 2, 2 → 3).collectValues { case 2 ⇒ 20 }" → "Map(1 → 20)" // TODO [28 Oct 2015] Remove this or updateValues
      ),
      Partial("updateValues", "PartialFunction[V, W] ⇒ Map[K, W]", "Applies a partial function to each value",
        "Map(1 → 2, 2 → 3).updateValues { case 2 ⇒ 20 }" → "Map(1 → 20)"
      ),
      Partial("updateValues", "(V ⇒ Option[W]) ⇒ Map[K, W]", "Applies a function to each value, retains only Somes",
        "Map(1 → 2, 2 → 3).updateValues(v ⇒ if (v == 2) Some(20) else None)" → "Map(1 → 20)"
      ),
      Partial("keyExists", "Predicate[K] ⇒ Boolean", "Determine if any key matches the predicate",
        "Map(1 → 2).keyExists(_ > 1)" → "false", "Map(2 → 3).keyExists(_ > 1)" → "true"
      ),
      Partial("valueExists", "Predicate[V] ⇒ Boolean", "Determine if any value matches the predicate",
        "Map(1 → 2).valueExists(_ > 2)" → "false", "Map(2 → 3).valueExists(_ > 2)" → "true"
      ),
      Partial("filterValues", "Predicate[V] ⇒ Map[K, V]", "Retain those values that match a predicate",
        "Map(1 → 2, 2 → 3).filterValues(_ > 2)" → "Map(2 → 3)"
      ),
      Partial("filterValuesNot", "Predicate[V] ⇒ Map[K, V]", "Discard those value that match a predicate",
        "Map(1 → 2, 2 → 3).filterValuesNot(_ > 2)" → "Map(1 → 2)"
      ),
      Partial("filterKeysNot", "Predicate[K] ⇒ Map[K, V]", "Discard those keys that match a predicate",
        "Map(1 → 2, 2 → 3).filterKeysNot(_ > 1)" → "Map(1 → 2)"
      ),
      Partial("findValue", "Predicate[V] ⇒ Option[V]", "Search for a value that matches a predicate",
        "Map(1 → 2).findValue(_ > 2)" → "None", "Map(2 → 3).findValue(_ > 2)" → "Some(3)"
      ),
      Partial("findKey", "Predicate[K] ⇒ Option[K]", "Search for a key that matches a predicate",
        "Map(1 → 2).findValue(_ > 1)" → "None", "Map(2 → 3).findValue(_ > 1)" → "Some(2)"
      ),
      Partial("sorted", "implicit Ordering[K] ⇒ SortedMap[K, V]", "Sort map by keys",
        "Map(1 → 2, 2 → 3).sorted(Ordering.Int.reverse)" → "SortedMap(2 → 3, 1 → 2)"
      ),
      Partial("reverse", "(Set[K] ⇒ K) ⇒ Map[V, K]", "Reverse the keys & values, using the function to choose between keys",
        "Map(1 → 2, 2 → 3, 3 → 3).reverse(_.min)" → "Map(2 → 1, 3 → 2)"
      ),
      Partial("reverseToMultiMap", "MultiMap[Set, V, K]", "Reverse the keys & values, retaining all data",
        "Map(1 → 2, 2 → 3, 3 → 3).reverseToMultiMap" → "Map(2 → Set(1), 3 → Set(2, 3))"
      ),
      Partial("containsAll", "Option[K] ⇒ Boolean", "Determine if the map contains all the provided keys",
        "Map(1 → 2).containsAll(None)" → "true", "Map(1 → 2).containsAll(Some(10))" → "false",
        "Map(1 → 2).containsAll(Some(1))" → "true"
      ),
      Partial("containsAll", "GTL[K] ⇒ Boolean", "Determine if the map contains all the provided keys",
        "Map(1 → 2).containsAll(Nil)" → "true", "Map(1 → 2).containsAll(List(10))" → "false",
        "Map(1 → 2).containsAll(List(1))" → "true"
      ),
      Partial("containsAny", "Option[K] ⇒ Boolean", "Determine if the map contains any of the provided keys",
        "Map(1 → 2).containsAny(None)" → "false", "Map(1 → 2).containsAny(Some(10))" → "false",
        "Map(1 → 2).containsAny(Some(1))" → "true"
      ),
      Partial("containsAny", "GTL[K] ⇒ Boolean", "Determine if the map contains any of the provided keys",
        "Map(1 → 2).containsAny(Nil)" → "true", "Map(1 → 2).containsAny(List(10))" → "false",
        "Map(1 → 2).containsAny(List(1))" → "true"
      ),
      Partial("containsEntry", "(K, V) ⇒ Boolean", "Determine if the map contains the provided entry",
        "Map(1 → 2).containsEntry(1, 2)" → "true", "Map(1 → 2).containsEntry(1, 1)" → "false"
      ),
      Partial("containsEntry", "((K, V)) ⇒ Boolean", "Determine if the map contains the provided entry",
        "Map(1 → 2).containsEntry((1, 2))" → "true", "Map(1 → 2).containsEntry((1, 1))" → "false"
      ),
      Partial("get", "Option[K] ⇒ Option[V]", "Lookup the provided key if Some",
        "Map(1 → 2).get(None)" → "None", "Map(1 → 2).get(Some(2))" → "None", "Map(1 → 2).get(Some(1))" → "Some(2)"
      ),
      Partial("emptyTo", "Map[K, V] ⇒ Map[K, V]", "Converts empty map to provided value, leaves other maps alone",
        "Map(1 → 2).emptyTo(Map(2 → 3))" → "Map(1 → 2)", "Map().emptyTo(Map(2 → 3))" → "Map(2 → 3)"
      ),
      Partial("uncons", "(A, Map[K, V] ⇒ A) ⇒ A", "Convert to A depending on whether the map is empty or not",
        "     Map().uncons(\"empty\", m ⇒ \"non-empty: \" + m.size)" → "\"empty\"",
        "Map(1 → 2).uncons(\"empty\", m ⇒ \"non-empty: \" + m.size)" → "\"non-empty: 1\""
      ),
      Partial("entryFor.minKey", "implicit Ordering[K] ⇒ Option[(K, V)]", "The entry with the minimum key",
        "Map(1 → 2, 2 → 3).entryFor.minKey" → "Some((1, 2))",
        "Map(1 → 2, 2 → 3).entryFor.minKey(Ordering.Int.reverse)" → "Some((2, 3))"
      ),
      Partial("entryFor.maxKey", "implicit Ordering[K] ⇒ Option[(K, V)]", "The entry with the maximum key",
        "Map(1 → 2, 2 → 3).entryFor.maxKey" → "Some((2, 3))",
        "Map(1 → 2, 2 → 3).entryFor.maxKey(Ordering.Int.reverse)" → "Some((1, 2))"
      ),
      Partial("entryFor.minValue", "implicit Ordering[V] ⇒ Option[(K, V)]", "The entry with the minimum value",
        "Map(1 → 2, 2 → 3).entryFor.minValue" → "Some((1, 2))",
        "Map(1 → 2, 2 → 3).entryFor.minValue(Ordering.Int.reverse)" → "Some((2, 3))"
      ),
      Partial("entryFor.maxValue", "implicit Ordering[V] ⇒ Option[(K, V)]", "The entry with the maximum value",
        "Map(1 → 2, 2 → 3).entryFor.maxValue" → "Some((2, 3))",
        "Map(1 → 2, 2 → 3).entryFor.maxValue(Ordering.Int.reverse)" → "Some((1, 2))"
      ),
      Partial("entryFor.matchingKey", "Predicate[K] ⇒ Option[(K, V)]", "The entry whose key matches the predicate",
        "Map(1 → 2, 2 → 3).entryFor.matchingKey(_ > 2)" → "Some((2, 3))"
      ),
      Partial("entryFor.matchingValue", "Predicate[V] ⇒ Option[(K, V)]", "The entry whose value matches the predicate",
        "Map(1 → 2, 2 → 3).entryFor.matchingValue(_ > 3)" → "Some((2, 3))"
      ),
      Partial("keyFor.minValue", "implicit Ordering[V] ⇒ Option[K]", "The key whose entry has the minimum value",
        "Map(1 → 2, 2 → 3).keyFor.minValue" → "Some(1)",
        "Map(1 → 2, 2 → 3).keyFor.minValue(Ordering.Int.reverse)" → "Some(2)"
      ),
      Partial("keyFor.maxValue", "implicit Ordering[V] ⇒ Option[K]", "The key whose entry has the maximum value",
        "Map(1 → 2, 2 → 3).keyFor.maxValue" → "Some(2)",
        "Map(1 → 2, 2 → 3).keyFor.maxValue(Ordering.Int.reverse)" → "Some(1)"
      ),
      Partial("valueFor.minKey", "implicit Ordering[K] ⇒ Option[V]", "The value whose entry has the minimum key",
        "Map(1 → 2, 2 → 3).valueFor.minKey" → "Some(2)",
        "Map(1 → 2, 2 → 3).valueFor.minKey(Ordering.Int.reverse)" → "Some(3)"
      ),
      Partial("valueFor.maxKey", "implicit Ordering[K] ⇒ Option[V]", "The value whose entry has the maximum key",
        "Map(1 → 2, 2 → 3).valueFor.maxKey" → "Some(3)",
        "Map(1 → 2, 2 → 3).valueFor.maxKey(Ordering.Int.reverse)" → "Some(2)"
      ),
      Partial("andThenM", "Map[V, W] ⇒ Map[K, W]", "Join the values of the map with the keys of another",
        "Map(1 → 2, 2 → 3).andThenM(Map(2 → \"two\"))" → "Map(1 → \"two\")"
      ),
      Partial("composeM", "Map[C, K] ⇒ Map[C, V]", "Join the keys of the map with the values of another",
        "Map(1 → 2, 2 → 3).composeM(Map(\"two\" → 2))" → "Map(\"two\" → 3)"
      ),
      Partial("partitionKeysBy", "PartialFunction[K, C] ⇒ (Map[K, V], Map[C, V])",
        "Applies a partial function to each key but retain the subset of the input where the function isn't defined",
        "Map(1 → 2, 2 → 3).partitionKeysBy { case 2 ⇒ \"two\" }" → "(Map(1 → 2), Map(\"two\" → 3))"
      ),
      Partial("partitionValuesBy", "PartialFunction[V, W] ⇒ (Map[K, V], Map[K, W])",
        "Applies a partial function to each value but retain the subset of the input where the function isn't defined",
        "Map(1 → 2, 2 → 3).partitionValuesBy { case 2 ⇒ \"two\" }" → "(Map(2 → 3), Map(1 → \"two\"))"
      ),
      Partial("partitionEntriesBy", "PartialFunction[(K, V), (C, W)] ⇒ (Map[K, V], Map[C, W])",
        "Applies a partial function to each entry but retain the subset of the input where the function isn't defined",
        "Map(1 → 2, 2 → 3).partitionValuesBy { case (1, 2) ⇒ (\"one\", \"two\") }" → "(Map(2 → 3), Map(\"one\" → \"two\"))"
      ),
      Partial("calcIfNonEmpty", "(Map[K, V] ⇒ B) ⇒ Option[B]", "Calculate result if non empty, None otherwise",
        "Map(1 → 2, 2 → 3).calcIfNonEmpty(_.length)" → "Some(2)", "Map().calcIfNonEmpty(_.length)" → "None"
      ),
      Partial("mutable", "mutable.Map[K, V]", "Convert to mutable Map"),
      Partial("toMutable", "mutable.Map[K, V]", "Convert to mutable Map")
    ), "MultiMap[F, K, V]" → List(
      Partial("select", "F[V] ⇒ W ⇒ Map[K, W]", "Choose a slice of the multimap, equivalent to Map[K, F[V]].mapValuesEagerly",
        "Map(1 → List(1, 11), 2 → List(2, 22)).select(_.last)" → "Map(1 → 11, 2 → 22)"
      ),
      Partial("merge", "MultiMap[F, K, V] ⇒ MultiMap[F, K, V]", "Combines two multimaps by concatenating their values",
        "Map(1 → List(1), 3 → List(3)).merge(Map(2 → List(2), 3 → List(33))" →
          "Map(1 → List(1), 2 → List(2), 3 → List(3, 33))"
      ),
      Partial("append", "(K, F[V]) ⇒ MultiMap[F, K, V]", "Appends a single entry, concatenating if already present",
        "Map(1 → List(11)).append(2, List(22))"  → "Map(1 → List(11), 2 → List(22))",
        "Map(1 → List(11)).append(1, List(111))" → "Map(1 → List(11, 111))"
      ),
      Partial("multiMap.head", "Map[K, V]", "Selects the head element of each value",
        "Map(1 → List(11), 2 → List(22, 222)).multiMap.head" → "Map(1 → 11, 2 → 22)"
      ),
      Partial("multiMap.tail", "MultiMap[F, K, V]", "Selects the tail of each value, only retains non-empty values",
        "Map(1 → List(1, 11), 2 → List(22), 3 → Nil).multiMap.tail" → "Map(1 → List(11))"
      ),
      Partial("multiMap.reverse", "MultiMap[F, V, K]", "Reverses the multimap",
        "Map('a' → List(1, 2), 'b' → List(2, 3)).reverse" → "Map(1 → List('a'), 2 → List('a', 'b'), 3 → List('b'))"
      ),
      Partial("multiMap.mapEntries", "(K ⇒ F[V] ⇒ (C, F[W])) ⇒ MultiMap[F, C, W]", "Map over each entry to produce another multimap",
        "Map(2 → List(2, 22)).mapEntries(key ⇒ values ⇒ (key * 10, List(values.sum)))" → "Map(20 → List(24))"
      ),
      Partial("multiMap.mapEntriesU", "(K ⇒ F[V] ⇒ (C, G[W])) ⇒ MultiMap[G, C, W]", "Map over each entry to produce another type of multimap",
        "Map(2 → List(2, 22)).mapEntriesU(key ⇒ values ⇒ (key * 10, Set(values.sum)))" → "Map(20 → Set(24))"
      ),
      Partial("multiMap.values", "F[V]", "Concatenate values",
        "Map(1 → List(1), 2 → List(2, 22)).multiMap.values" → "List(1, 2, 22)"
      ),
      Partial("multiMap.sliding", "Int ⇒ F[MultiMap[F, K, V]]", "Produce a sequence of multimaps that slide across the original",
        "Map(1 → List(11, 12, 13), 2 → List(21, 22, 23)).multiMap.sliding(2)" ->
          "List(Map(1 → List(11, 12), 2 → List(21, 22)), Map(1 → List(12, 13), 2 → List(22, 23)))"
      ),
      Partial("pop", "K ⇒ MultiMap[F, K, V]", "Removes head value associated with key, discarding empty values entirely",
        "Map(1 → List(2, 3), 2 → List(3)).pop(1)" → "Map(1 → List(3), 2 → List(3))",
        "Map(1 → List(2, 3), 2 → List(3)).pop(2)" → "Map(1 → List(2, 3))",
        "Map(1 → List(2, 3), 2 → List(3)).pop(3)" → "Map(1 → List(2, 3), 2 → List(3)))"
      ),
      Partial("sequence", "F[Map[K, V]]", "Convert a multimap to a sequence of maps",
        "Map(1 → List(10, 11), 2 → List(20, 21)).sequence" → "List(Map(1 → 10, 2 → 20), Map(1 → 11, 2 → 21))"
      ),
      Partial("headTailOption", "Option[(Map[K, V], MultiMap[F, K, V])]",
        "The head & tail if the multimap is non empty, None otherwise",
        "Map(1 → List(10, 11), 2 → List(20)).headTailOption" → "Some(Map(1 → 10, 2 → 20), Map(1 → List(11)))",
        "Map(1 → Nil, 2 → List(20)).headTailOption"          → "Some(Map(2 → 20), Map())",
        "Map(1 → (Nil: List[Int])).headTailOption"           → "None",
        "MultiMap.empty[List, Int, Int]"                     → "None"
      ),
      Partial("flatMapValues", "(V ⇒ F[W]) ⇒ MultiMap[F, K, W]", "Expand each value to a collection of values",
        "Map(0 → List(1, 2)).flatMapValues(v ⇒ List(v, -v))" → "Map(0 → List(1, -1, 2, -2))"
      ),
      Partial("flatMapValuesU", "(V ⇒ G[W]) ⇒ MultiMap[G, K, W]", "Expand each value to a different type of collection of values",
        "Map(0 → List(1, 2)).flatMapValuesU(v ⇒ Set(v, -v))" → "Map(0 → Set(1, -1, 2, -2))"
      ),
      Partial("getOrEmpty", "K ⇒ F[V]", "Retrive the values associated with a key or an empty collection",
        "Map(1 → List(2)).getOrEmpty(1)" → "List(2)", "Map(1 → List(2)).getOrEmpty(2)" → "Nil"
      ),
      Partial("onlyOption", "Option[Map[K, V]]", "Returns Some map if the multimap only contains empty or singleton containers, None otherwise",
        "Map(1 → Nil, 2 → List(22)).onlyOption" → "Some(Map(2 → 22))", "Map(1 → List(1, 11)).onlyOption" → "None"
      )
    ), "NestedMap[K1, K2, V]" → List(
      Partial("append", "(K1, K2, V) ⇒ NestedMap[K1, K2, V]", "Append a value",
        "Map(1 → Map(2 → 3)).append(1, 2, 4)" → "Map(1 → Map(2 → 4))",
        "Map(1 → Map(2 → 3)).append(1, 3, 4)" → "Map(1 → Map(2 → 3, 3 → 4))",
        "Map(1 → Map(2 → 3)).append(2, 3, 4)" → "Map(1 → Map(2 → 3), 2 → Map(3 → 4))"
      ),
      Partial("+", "((K1, K2, V)) ⇒ NestedMap[K1, K2, V]", "Append a value",
        "Map(1 → Map(2 → 3)) + ((1, 2, 4))" → "Map(1 → Map(2 → 4))",
        "Map(1 → Map(2 → 3)) + ((1, 3, 4))" → "Map(1 → Map(2 → 3, 3 → 4))",
        "Map(1 → Map(2 → 3)) + ((2, 3, 4))" → "Map(1 → Map(2 → 3), 2 → Map(3 → 4))"
      ),
      Partial("flipNesting", "NestedMap[K2, K1, V]", "Flips the inner & outer keys",
        "Map(1 → Map(2 → 3), 6 → Map(3 → 4, 4 → 5)).flipNesting" → "Map(2 → Map(1 → 3), 3 → Map(6 → 4), 4 → Map(6 → 5))"
      ),
      Partial("getOrEmpty", "K1 ⇒ Map[K2, V]", "Retrive the values associated with a key or an empty collection",
        "Map(1 → Map(2 → 3)).getOrEmpty(1)" → "Map(2 → 3)", "Map(1 → Map(2 → 3)).getOrEmpty(2)" → "Map()"
      ),
      Partial("nestedMap.mapValuesEagerly", "(V ⇒ W) ⇒ NestedMap[K1, K2, W]", "Eagerly applies a function to each value",
        "Map(1 → Map(2 → 3)).nestedMap.mapValueEagerly(_ * 10)" → "Map(1 → Map(2 → 30))"
      ),
      Partial("nestedMap.mapKeysEagerly", "(K2 ⇒ C) ⇒ NestedMap[K1, C, V]", "Eagerly applies a function to each inner key",
        "Map(1 → Map(2 → 3)).nestedMap.mapKeysEagerly(_ * 10)" → "Map(1 → Map(20 → 3))"
      )
    ), "Predicate[A]" → List(
      Partial("ifSome", "Predicate[Option[A]]", "Lift predicate to apply to Option",
        "((i: Int) ⇒ i == 2).ifSome.apply(None)" → "false",
        "((i: Int) ⇒ i == 2).ifSome.apply(Some(1))" → "false",
        "((i: Int) ⇒ i == 2).ifSome.apply(Some(2))" → "true"
      ),
      Partial("and", "Predicate[A] ⇒ Predicate[A]", "A predicate that's true only when both input predicates are true",
        "((i: Int) ⇒ i % 3 == 0).and((i: Int) ⇒ i % 5 == 0).apply(1)" → "false",
        "((i: Int) ⇒ i % 3 == 0).and((i: Int) ⇒ i % 5 == 0).apply(3)" → "false",
        "((i: Int) ⇒ i % 3 == 0).and((i: Int) ⇒ i % 5 == 0).apply(5)" → "false",
        "((i: Int) ⇒ i % 3 == 0).and((i: Int) ⇒ i % 5 == 0).apply(15)" → "true"
      ),
      Partial("or", "Predicate[A] ⇒ Predicate[A]", "A predicate that's true when either input predicates are",
        "((i: Int) ⇒ i % 3 == 0).or((i: Int) ⇒ i % 5 == 0).apply(1)" → "false",
        "((i: Int) ⇒ i % 3 == 0).or((i: Int) ⇒ i % 5 == 0).apply(3)" → "true",
        "((i: Int) ⇒ i % 3 == 0).or((i: Int) ⇒ i % 5 == 0).apply(5)" → "true",
        "((i: Int) ⇒ i % 3 == 0).or((i: Int) ⇒ i % 5 == 0).apply(15)" → "true"
      ),
      Partial("exists", "Predicate[List[A]]", "A predicate that's true when there exists an element that passes",
        "((i: Int) ⇒ i == 2).exists.apply(Nil)" → "false",
        "((i: Int) ⇒ i == 2).exists.apply(List(1))" → "false",
        "((i: Int) ⇒ i == 2).exists.apply(List(2))" → "true",
        "((i: Int) ⇒ i == 2).exists.apply(List(1, 2))" → "true"
      ),
      Partial("guard", "(A ⇒ B) ⇒ PartialFunction[A, B]", "Limit a function by a predicate",
        "((i: Int) ⇒ i == 2).guard(_ * 10).lift(1)" → "None",
        "((i: Int) ⇒ i == 2).guard(_ * 10).lift(2)" → "Some(20)"
      ),
      Partial("cond", "(⇒ B, ⇒ B) ⇒ (A ⇒ B)", "")
    ), "(A ⇒ B)" → List(
      Partial("attempt", "(A ⇒ Try[B])", ""),
      Partial("guardWith", "Predicate[A] ⇒ PartialFunction[A, B]", "")
    ), "(A ⇒ B ⇒ C)" → List(
      Partial("tupled", "((A, B)) ⇒ C", "Convert curried function to a tupled one")
    ), "PartialFunction[A, B]" → List(
    ), "(A ⇒ Option[B])" → List(
      Partial("unlift", "PartialFunction[A, B]", "Create partial function")
    ), "PartialFunction[A, B]" → List(
      Partial("toLeft", "A ⇒ Either[B, A]", ""),
      Partial("toRight", "A ⇒ Either[A, B]", ""),
      Partial("either", "A ⇒ Either[A, B]", ""),
      Partial("partition", "CC[A] ⇒ (CC[A], CC[B])", ""),
      Partial("isUndefinedAt", "A ⇒ Boolean", ""),
      Partial("first", "PartialFunction[(A, C), (B, C)]", ""),
      Partial("second", "PartialFunction[(C, A), (C, B)]", ""),
      Partial("unify", "A ⇒ A", ""),
      Partial("map", "(B ⇒ C): PartialFunction[A, C]", ""),
      Partial("contramap", "(C ⇒ A)", "PartialFunction[C, B]"),
      Partial("***", "PartialFunction[C, D] ⇒ PartialFunction[(A, C), (B, D)]", ""),
      Partial("&&&", "PartialFunction[A, C] ⇒ PartialFunction[A, (B, C)]", ""),
      Partial("|||", "PartialFunction[C, B] ⇒ PartialFunction[Either[A, C], B]", ""),
      Partial("\\/", "PartialFunction[C, B] ⇒ PartialFunction[A \\/ C, B]", "")
    ), "Ordering[A]" → List(
      Partial("promote", "A* ⇒ Ordering[A]", "Promote occurences of the provided values when sorting",
        "List(10, 7, 5, 2, 7).sorted(Ordering.Int.promote(5, 7))" → "List(5, 7, 7, 2, 10)"
      ),
      Partial("demote", "A* ⇒ Ordering[A]", "Demote occurences of the provided values when sorting",
        "List(10, 7, 5, 2, 7).sorted(Ordering.Int.demote(5, 7))" → "List(2, 10, 5, 7, 7)"
      )
    ), "Numeric[A]" → List(
      Partial("xmap", "(A ⇒ B, B ⇒ A) ⇒ Numeric[B]", "Converts to operate on a new type",
        "implicitly[Numeric[Int]].xmap[String](_.toString, Integer.parseInt).plus(\"2\", \"3\")" → "\"5\""
      )
    ), "String" → List(
      Partial("emptyTo", "String ⇒ String", "Convert empty strings to another",
        """"".emptyTo("replacement")"""          → """"replacement"""",
        """"non-empty".emptyTo("replacement")""" → """"non-empty""""
      ),
      Partial("prefixPadTo", "(Int, Char) ⇒ String", ""),
      Partial("suffixWith", "String ⇒ String", "Add suffix to string if absent",
        """"file".suffixWith(".txt")"""     → """"file.txt"""",
        """"file.txt".suffixWith(".txt")""" → """"file.txt""""
      ),
      Partial("prefixWith", "String ⇒ String", "Add prefix to string if absent",
        """"file".prefixWith("dir/")"""     → """"dir/file"""",
        """"dir/file".prefixWith("dir/")""" → """"dir/file""""
      ),
      Partial("affixWith", "String => String ⇒ String", "Add prefix & suffix to string if absent",
        """"file".affixWith("dir/", ".txt")"""         → """"dir/file.txt"""",
        """"file.txt".affixWith("dir/", ".txt")"""     → """"dir/file.txt"""",
        """"dir/file".affixWith("dir/", ".txt")"""     → """"dir/file.txt"""",
        """"dir/file.txt".affixWith("dir/", ".txt")""" → """"dir/file.txt""""
      ),
      Partial("stripAffixes", "String => String ⇒ String", "Remove prefix & suffix from string",
        """"file".stripAffixes("dir/", ".txt")"""         → """"file"""",
        """"file.txt".stripAffixes("dir/", ".txt")"""     → """"file"""",
        """"dir/file".stripAffixes("dir/", ".txt")"""     → """"file"""",
        """"dir/file.txt".stripAffixes("dir/", ".txt")""" → """"file""""
      ),
      Partial("sharedPrefix", "String ⇒ (String, String, String)",
        "Split string into parts shared with the beginning of another, along with the remainder of each",
          "\"1234\".sharedPrefix(\"1243\")" -> "(\"12\", \"34\", \"43\")"
      ),
      Partial("md5", "String", ""),
      Partial("toByteArray", "Array[Byte]", ""),
      Partial("toByteArray", "CharSet ⇒ Array[Byte]", "")
    ), "Random" → List(
      Partial("between", "(Int, Int) ⇒ Int", "Generate a random number between two others",
        "Random.between(0, 10)" → "9"
      )
    ), "ThreadFactory" → List(
      Partial("naming", "(Int ⇒ String) ⇒ ThreadFactory", "Adapt threads created by factory to be named",
        "threadFactory.naming(i ⇒ s\"Thread: $i\").newThread(...).getName" → "Thread: 0"
      )
    ), "Callable[A]" → List(
      Partial("attempt", "Callable[Try[A]]", "Wrap result in Try")
    ), "Date" → List(
      Partial("addDay", "Int ⇒ Date", "Adds n days to date",
        "Date().addDay(2)" → ""
      )
    ), "InputStream" → List(
      Partial("closeAfter", "(InputStream ⇒ A) ⇒ A", "Close after applying function",
        "new ByteArrayInputStream(Array(123)).closeAfter(_.read())" → "123"
      ),
      Partial("attemptClose", "Try[Unit]", "Close & catch exceptions"),
      Partial("closeIf", "Boolean ⇒ InputStream", "Close if boolean is true"),
      Partial("closeUnless", "Boolean ⇒ InputStream", "Close if boolean is false"),
      Partial("drain", "(OutputStream, Boolean, Boolean) ⇒ InputStream",
        "Drain all bytes into an OutputStream, optionally closing afterwards"
      ),
      Partial(">>", "OutputStream ⇒ InputStream", "Drain all bytes into an OutputStream"),
      Partial("buffered", "BufferedInputStream", "Wrap in a BufferedInputStream"),
      Partial("gunzip", "GZIPInputStream", "Wrap in a GZIPInputStream"),
      Partial("readN", "(OutputStream, Long) ⇒ InputStream", "Read n bytes into an OutputStream & throw if unable to"),
      Partial("readUpToN", "(OutputStream, Long) ⇒ Long", "Read up to n bytes into an OutputStream"),
      Partial("toByteArray", "Array[Byte]", "Convert to an array of bytes")
    ), "OutputStream" → List(
      Partial("closeAfter", "(OutputStream ⇒ A) ⇒ A", "Close after applying a function"),
      Partial("attemptClose", "Try[Unit]", "Close & catch exceptions"),
      Partial("closeIf", "Boolean ⇒ OutputStream", "Close if boolean is true"),
      Partial("closeUnless", "Boolean ⇒ OutputStream", "Close if boolean is false"),
      Partial("drain", "(InputStream, Boolean, Boolean) ⇒ OutputStream",
        "Drain all bytes from an InputStream, optionally closing each afterwards"
      ),
      Partial("<<", "InputStream ⇒ OutputStream", "Drain all bytes from an InputStream"),
      Partial("buffered", "BufferedOutputStream", "Wrap in a BufferedOutputStream"),
      Partial("gzip", "GZIPOutputStream", "Wrap in a GZIPOutputStream"),
      Partial("writeN", "(InputStream, Long) ⇒ OutputStream", "Write n bytes into an InputStream & throw if unable to"),
      Partial("writeUpToN", "(InputStream, Long) ⇒ Long", "Write up to n bytes into an InputStream")
    ), "File" → List(
      Partial("isJar", "Boolean", "Check if file has '.jar' extension"),
      Partial("isClass", "Boolean", "Check if file has '.class' extension"),
      Partial("isJava", "Boolean", "Check if file has '.java' extension"),
      Partial("isScala", "Boolean", "Check if file has '.scala' extension"),
      Partial("missing", "Boolean", "Check if file is missing"),
      Partial("isChildOf", "File ⇒ Boolean", "Check if file is a child of another",
        "new File(\"/etc/password\").isChildOf(\"/etc\")" → "true",
        "new File(\"/etc/password\").isChildOf(\"/\")" → "false"
      ),
      Partial("isParentOf", "File ⇒ Boolean", "Check if file is parent of another",
        "new File(\"/etc\").isParentOf(\"/etc/password\")" → "true",
        "   new File(\"/\").isParentOf(\"/etc/password\")" → "false"
      ),
      Partial("isAncestorOf", "File ⇒ Boolean", ""),
      Partial("isContainedIn", "File ⇒ Boolean", ""),
      Partial("contains", "File ⇒ Boolean", ""),
      Partial("hasExtension", "String ⇒ Boolean", ""),
      Partial("/", "String ⇒ File", ""),
      Partial("named", "String ⇒ File", ""),
      Partial("canon", "File", ""),
      Partial("relativeTo", "File ⇒ File", ""),
      Partial("writeLines", "(List[String], Boolean) ⇒ File", ""),
      Partial("writeBytes", "(Array[Byte], Boolean) ⇒ File", ""),
      Partial("write", "(String, Boolean) ⇒ File", ""),
      Partial("deleteRecursively", "File", ""),
      Partial("deleteRecursivelyOnExit", "File", ""),
      Partial("changeToDirectory", "File", ""),
      Partial("create", "Boolean ⇒ File", ""),
      Partial("touch", "File", ""),
      Partial("children", "Stream[File]", ""),
      Partial("childDirs", "Stream[File]", ""),
      Partial("tree", "Stream[File]", ""),
      Partial("ancestors", "Stream[File]", ""),
      Partial("path", "List[String]", ""),
      Partial("outputStream", "Boolean ⇒ FileOutputStream", ""),
      Partial("source", "implicit Codec ⇒ BufferedSource", ""),
      Partial("readString", "implicit Codec ⇒ String", ""),
      Partial("readBytes", "Array[Byte]", ""),
      Partial("readLines", "implicit Codec ⇒ List[String]", ""),
      Partial("className", "File ⇒ String", ""),
      Partial("md5", "String", "")
    ), "mutable.Builder[A, B]" → List(
      Partial("+++=", "TraversableOnce[TraversableOnce[A]] ⇒ mutable.Builder[A, B]",
        "Adds the elements of several traversables",
          "(new ListBuffer[Int] +++= List(List(1, 2), List(3), List(4))).result()" → "List(1, 2, 3, 4)"
      ),
      Partial("on", "(C ⇒ A) ⇒ mutable.Builder[C, B]", "Change the type of element consumed by the builder",
        "(new ListBuffer[Double].on[Int](_.toDouble)) += 3).result()" → "List(3.0)"
      ),
      Partial("reset", "B", "Return result & clear"),
      Partial("run", "(mutable.Builder[A, B] ⇒ Discarded)* ⇒ B", "Update the builder & produce a result",
        "new ListBufer[Int].run(_ += 1, _ ++= List(2, 3)" → "List(1, 2, 3)"
      )
    ), "mutable.Map[K, V]" → List(
      Partial("retainKeys", "Predicate[K] ⇒ mutable.Map[K, V]", "Retain entries whose keys match the predicate"),
      Partial("retainValues", "Predicate[V] ⇒ mutable.Map[K, V]", "Retain entries whose values match the predicate")
    ), "CodecJson[A]" → List(
      Partial("beforeDecode", "(Json ⇒ Json) ⇒ CodecJson[A]", "Pre-processes json before decoding"),
      Partial("afterDecode", "(A ⇒ A) ⇒ CodecJson[A]", "Converts after decoding"),
      Partial("beforeEncode", "(A ⇒ A) ⇒ CodecJson[A]", "Converts before encoding"),
      Partial("afterEncode", "(Json ⇒ Json) ⇒ CodecJson[A]", "Post-processes json after encoding"),
      Partial("andThen", "(Json ⇒ Json) ⇒ CodecJson[A]", "Post-processes json after encoding"),
      Partial("compose", "(Json ⇒ Json) ⇒ CodecJson[A]", "Pre-processes json before decoding")
    ), "CodecJson[Map[K, V]]" → List(
      Partial("xmapKeys", "(K ⇒ C) ⇒ (C ⇒ K) ⇒ CodecJson[Map[C, V]]", "Converts map keys before encoding & after decoding"),
      Partial("xmapValues", "(V ⇒ W) ⇒ (W ⇒ V) ⇒ CodecJson[Map[K, W]]", "Converts map values before encoding & after decoding")
    ), "DecodeJson[A]" → List(
      Partial("beforeDecode", "(Json ⇒ Json) ⇒ DecodeJson[A]", "Pre-processes json before decoding"),
      Partial("compose", "(Json ⇒ Json) ⇒ DecodeJson[A]", "Pre-processes json before decoding"),
      Partial("upcast", "DecodeJson[B]", "Upcasts results after decoding")
    ), "DecodeJson[Map[K, V]]" → List(
      Partial("mapKeys", "(K ⇒ C) ⇒ DecodeJson[Map[C, V]]", "Converts map keys after decoding",
        """mapDecoder.mapKeys(_.reverse).decodeJson({ "foo" → "bar" })""" → """Map("oof" → "bar")"""
      ),
      Partial("mapValues", "(V ⇒ W) ⇒ DecodeJson[Map[K, W]]", "Converts map values after decoding",
        """mapDecoder.mapValues(_.reverse).decodeJson({ "foo" → "bar" })""" → """Map("foo" → "rab")"""
      )
    ), "EncodeJson[A]" → List(
      Partial("afterEncode", "(Json ⇒ Json) ⇒ EncodeJson[A]", "Post-processes json after encoding"),
      Partial("andThen", "(Json ⇒ Json) ⇒ EncodeJson[A]", "Post-processes json after encoding"),
      Partial("downcast", "EncodeJson[B]", "Downcasts values before encoding")
    ), "EncodeJson[Map[K, V]]" → List(
      Partial("contramapKeys", "(C ⇒ K) ⇒ EncodeJson[Map[C, V]]", "Converts map keys before encoding",
        """mapEncoder.contramapKeys(_.reverse).encodeJson(Map("foo" → "bar"))""" → """{ "oof" → "bar" }"""
      ),
      Partial("contramapValues", "(W ⇒ V) ⇒ EncodeJson[Map[K, W]]", "Converts map values before encoding",
        """mapEncoder.contramapValues(_.reverse).encodeJson(Map("foo" → "bar"))""" → """{ "foo" → "rab" }"""
      )
    ), "Json" → List(
      Partial("filterNulls", "Json", "Recursively removes null values",
        """null"""                        → """null""",
        """{ "a": null, "b": 3 }"""       → """{ "b": 3 }""",
        """[ "a", null, "b" ]"""          → """[ "a", "b" ]""",
        """{ "o": [ "a", null, "b" ] }""" → """{ "o": [ "a", "b" ] }""",
        """[ { "a": null, "b": 3 } ]"""   → """[ { "b": 3 } ]"""
      )
    ), "Traversal[Json, Json]" → List(
      Partial("string", "Traversal[Json, String]", "compose with string prism",
        """Traversal.id[Json].string.getAll(Json.jString("foo"))""" → """List("foo")""",
        """Traversal.id[Json].string.getAll(Json.jNumber(3))"""     → """Nil"""
      ),
      Partial("int", "Traversal[Json, String]", "compose with int prism",
        """Traversal.id[Json].int.getAll(Json.jString("foo"))""" → """Nil""",
        """Traversal.id[Json].int.getAll(Json.jNumber(3))"""     → """List(3)"""
      )
    ), "(L \\/ R)" → List(
      Partial("tap", "(L ⇒ Discarded, R ⇒ Discarded) ⇒ L \\/ R", "Perform one action or another",
        "   -\\/(1).tap(l ⇒ print(\"left: \" + l), r ⇒ print(\"right: \" + r))" → "Prints \"left: 1\"",
        "\\/-(true).tap(l ⇒ print(\"left: \" + l), r ⇒ print(\"right: \" + r))" → "Prints \"right: true\""
      ),
      Partial("tapLeft", "(L ⇒ Discarded) ⇒ L \\/ R", "Perform action if Left",
        "   -\\/(1).tapLeft(l ⇒ print(\"left: \" + l))" → "Prints \"left: 1\"",
        "\\/-(true).tapLeft(l ⇒ print(\"left: \" + l))" → "Does nothing"
      ),
      Partial("tapRight", "(R ⇒ Discarded) ⇒ L \\/ R", "Perform action if Right",
        "   -\\/(1).tap(r ⇒ print(\"right: \" + r))" → "Does nothing",
        "\\/-(true).tap(r ⇒ print(\"right: \" + r))" → "Prints \"right: true\""
      ),
      Partial("addTo", "(Growable[L], Growable[R]) ⇒ L \\/ R", "Adds values into growables",
        "      -\\/(1).addTo(ints, strings)" → "ints += 1",
        "\\/-(\"foo\").addTo(ints, strings)" → "strings += \"foo\""
      ),
      Partial("removeFrom", "(Shrinkable[L], Shrinkable[R]) ⇒ L \\/ R", "Removes values from a shrinkables",
        "      -\\/(1).removeFrom(ints, strings)" → "ints -= 1",
        "\\/-(\"foo\").removeFrom(ints, strings)" → "strings -= \"foo\""
      ),
      Partial("leftFlatMap", "L => L \\/ R => L \\/ R", "flatMap over the left, equivalent to swap.flatMap.swap",
        "   \\/-(123).leftFlatMap(in => in match { case \"left\" => \\/-(100); case _ => -\\/(in) }" → "\\/-(123)",
        "-\\/(\"left\").leftFlatMap(in => in match { case \"left\" => \\/-(100); case _ => -\\/(in) }" → "\\/-(100)",
        " -\\/(\"101\").leftFlatMap(in => in match { case \"left\" => \\/-(100); case _ => -\\/(in) }" → "-\\/(\"101\")"
      )
    ), "(L \\/ (L \\/ R))" → List(
      Partial("flatten", "L \\/ R", "Flattens two level disjunction into one",
        "-\\/(1).flatten" → "-\\/(1)", "\\/-(-\\/(2)).flatten" → "-\\/(2)", "\\/-(\\/-(\"s\"))).flatten" → "\\/-(\"s\")"
      )
    ), "((L \\/ R) \\/ R)" → List(
      Partial("flatten", "L \\/ R", "Flattens two level disjunction into one",
        "-\\/(-\\/(1)).flatten" → "-\\/(1)", "-\\/(\\/-(\"s\")).flatten" → "\\/-(\"s\")", "\\/-(\"s\").flatten" → "\\/-(\"s\")"
      )
    ), "NonEmptyList[A]" → List(
      Partial("unique", "NonEmptyList[A]", "Remove duplicates",
        "NonEmptyList(3, 1, 4, 3, 4).unique" → "NonEmptyList(3, 1, 4)"
      ),
      Partial("uniqueBy", "(A ⇒ B) ⇒ NonEmptyList[A]", "Remove items with duplicate properties",
        "NonEmptyList(\"foo\", \"bar\", \"bare\", \"food\").uniqueBy(_.length)" → "NonEmptyList(\"foo\", \"bare\")"
      ),
      Partial("filter", "Predicate[A]", "filters the NonEmptyList",
        "   NonEmptyList(1).filter(_ % 2 == 0)" → "None",
        "NonEmptyList(1, 2).filter(_ % 2 == 0)" → "Some(NonEmptyList(2))"
      ),
      Partial("filterNot", "Predicate[A]", "filters the NonEmptyList",
        "   NonEmptyList(2).filterNot(_ % 2 == 0)" → "None",
        "NonEmptyList(1, 2).filterNot(_ % 2 == 0)" → "Some(NonEmptyList(1))"
      ),
      Partial("onlyOption", "Option[A]", "Head if non-empty list has 1 element, None otherwise",
        "NonEmptyList(1).onlyOption" → "Some(1)", "NonEmptyList(1, 2).onlyOption" → "None"
      ),
      Partial("onlyEither", "Either[NonEmptyList[A], A]", "Right if non-empty list has 1 element, Left otherwise",
        "NonEmptyList(1).onlyEither" → "Right(1)", "NonEmptyList(1, 2).onlyEither" → "Left(NonEmptyList(1, 2))"
      ),
      Partial("onlyDisjunction", "NonEmptyList[A] \\/ A", "\\/- if list has 1 element, -\\/ otherwise",
        "NonEmptyList(1).onlyDisjunction" → "\\/-(1)", "NonEmptyList(1, 2).onlyDisjunction" → "-\\/(NonEmptyList(1, 2))"
      )
    ), "NonEmptyList[A: Order]" → List(
      Partial("max", "A", "Maximum value",
        "NonEmptyList(3, 1, 4).max" → "4"
      ),
      Partial("min", "A", "Minimum value",
        "NonEmptyList(3, 1, 4).min" → "1"
      )
    ), "NonEmptyList[V]" → List(
      Partial("asMap...", "", ""),
      Partial("asMultiMap...", "", "")
    ), "NonEmptyList[K]" → List(
      Partial("as[F]", "", "")
    ))).toString().getBytes(Charset.forName("UTF-8")))

    fos.flush()
    fos.close()

    null // returning null => Generate in 1s, return file => 30s !?
  }

  object Partial {
    def apply(name: String, partSignature: String, description: String, examples0: (String, String)*): Partial =
      new Partial(name, partSignature, description, examples0.toList, Nil)
  }

  class Partial(val name: String, signature: String, val description: String, examples: List[(String, String)], seeOthers: List[String]) {
    def seeOther(names: String*) = new Partial(name, signature, description, examples, names.toList ++ seeOthers)
    def pimp(category: String) = Pimp(category, name, signature, description, examples, seeOthers)
  }

  case class Pimp(
    category: String, name: String, partSignature: String, description: String,
    examples0: List[(String, String)] = Nil, seeOthers: List[String] = Nil
  ) {

    def signature = category + " ⇒ " + partSignature
    def dataName = category + "_" + name
    def idName = "id_" + dataName

    def href = "#" + dataName
    def src = "https://github.com/stacycurl/pimpathon/blob/master/core/src/main/scala/pimpathon/any.scala#L13"

    def seeAlso: NodeSeq = NodeSeq.Empty
//      <div class="see">
//        See also
//        <a href="#subtract">|></a>.
//      </div>

    def examples = if (examples0.isEmpty) NodeSeq.Empty else {
//      val maxExpressionLength = examples0.flatMap(_._1.split("\n").toList).map(_.length).max

      examples0.flatMap {
        case (expression, result) => {
          <div style="display: flex">
            <pre style="flex: 1"><code class="hljs javascript">{expression.stripMargin}</code></pre>
            <pre><code class="hljs-comment">{result.stripMargin}</code></pre>
          </div>
        }
      }

//      <pre><code class="hljs javascript">{examples0.flatMap {
//        case (expressions, result0) ⇒ {
//          expressions.split("\n").zip(result0 #:: Stream.continually("")).map {
//            case (expression, result) if result != "" ⇒ {
//              val padding = " " * (maxExpressionLength - expression.length)
//
//              <span>{Text(expression)}</span><span class="hljs-comment">  {padding}// =&gt; {result}</span><br/>
//            }
//            case (expression, "") ⇒ <span>{Text(expression)}</span><br/>
//          }
//        }
//      }}</code></pre>
    }
      //<pre><code class="hljs javascript"></code></pre>
  }

  def template(version: String, partials0: String ▶: List[Partial]): Elem = {
    val partials = partials0
//      .filterKeys(_ == "GTL[A]").mapValues(_.filter(p ⇒ p.name == "asMultiMap.withEntries" && p.description.contains("nested")))

    template(version, partials.flatMap {
      case (category, ps) ⇒ if (ps.isEmpty) List(Pimp(category, "todo", "todo", "todo")) else ps.map(_.pimp(category))
    })
  }

  def template(version: String, pimps: Iterable[Pimp]): Elem =
    <html class="docs-page">
      <head>
        <meta charset="UTF-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <title>Pimpathon Documentation</title>
        <link rel="stylesheet" type="text/css" href="bootstrap-3.3.5.css"/>
      </head>
      <body>
        <input type="checkbox" id="open-nav"/>
        <header class="navbar navbar-fixed-top navbar-inverse container-fluid">
          <div class="navbar-header">
            <label class="open-nav" for="open-nav"></label>
            <a class="navbar-brand" href="/">
              <strong>Pimpathon</strong>
              <span class="version">{version}</span>
            </a>
          </div>
          <div class="navbar-left">
            <ul class="nav navbar-nav">
              <li><a href="https://github.com/stacycurl/pimpathon">GitHub</a></li>
            </ul>
          </div>
        </header>
        <aside class="sidebar container-fluid">
          <div class="form-group has-feedback filter">
            <input class="form-control"
                   tabindex="1"
                   id="name-filter"
                   placeholder="Filter"
                   type="text"
                   data-bind="textInput: filter"
                   autofocus="autoFocus"
            />
            <span class="form-control-feedback">
              <span></span>
            </span>
          </div>

          <ul class="nav nav-pills nav-stacked toc">{pimps.map(pimp ⇒ {
            <li class="func" data-name={pimp.dataName} data-category={pimp.category}>
              <a href={pimp.href}>{pimp.name}<span data-category={pimp.category} class="label label-category pull-right">{pimp.category}</span></a>
            </li>
          })}</ul>
        </aside>
        <main class="container-fluid">{pimps.map(pimp ⇒ {
          <section class="card" id={pimp.dataName}>
            <h2>
              <a tabindex="2" class="name" href={pimp.href} id={pimp.idName}>{pimp.name}</a>
              <span class="pull-right">
                <span class="label label-category">{pimp.category}</span>
                <a target="_blank" title="View source on GitHub" href={pimp.src}><small>GH</small></a>
              </span>
            </h2>

            <div class="sig btn btn-link"><span class="caret rotated"></span>{pimp.signature}</div>
            <div class="description"><p>{pimp.description}</p></div>

            {pimp.seeAlso}
            {pimp.examples}
          </section>
        })}</main>
        <script src="main.js"></script>
      </body>
    </html>
}
