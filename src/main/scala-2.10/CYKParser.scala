import scala.collection.breakOut

/**
 * Created by sergej on 16.12.15.
 */
class CYKParser(lexicon: Map[String, Set[Category]], rules: List[Rule]) {

  def apply(sentence: String): Set[Category] = apply(sentence.split(" ").toIndexedSeq)

  def apply(tokens: IndexedSeq[String]): Set[Category] = {
    def raiseType(left: Category, right: Category) : (Category, Category) = {
      val leftRaised = ForwardTypeRaising(left, right) match {
        case None => left
        case Some(c : Category) => c
      }
      val rightRaised = BackwardTypeRaising(left, right) match {
        case None => right
        case Some(c : Category) => c
      }
      (leftRaised, rightRaised)
    }

    val numItems = tokens.length

    tokens.foreach { token =>
      if (!lexicon.contains(token))
        throw new MissingLexicalEntryException("\n\nError: word '" + token + "' not in lexicon!\n")
    }

    val chart: Array[Array[Set[Category]]] = Array.fill(numItems, numItems)(Set[Category]())

    for (j <- 0 until numItems) {
      chart(j)(j) = lexicon(tokens(j))

      for (i <- (0 to j - 1).reverse) {
        chart(i)(j) =
          (for (
            k <- i to j - 1;
            left <- chart(i)(k);
            right <- chart(k + 1)(j);
            (leftRaised, rightRaised) = raiseType(left, right);
            rule <- rules;
            result <- rule(leftRaised, rightRaised)
          ) yield result)(breakOut)
      }
    }
    chart(0)(numItems - 1)
  }
}

/** A companion object for the CYKParser class. */
object CYKParser {
  /**
   * Create a CkyParser object with the provided lexicon and using all
   * possible rules.
   */
  def apply(lexicon: Map[String, Set[Category]]) = new CYKParser(lexicon, Rule.allRules)

}
