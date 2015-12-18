/**
 * Created by sergej on 16.12.15.
 */
case class LexicalEntry(word: String, category: Category)

object Lexicon {
  lazy val catParser = new CategoryParser

  def apply (entries: List[String]) = {
    val validLines = entries.filter(line => line != "" && !line.startsWith("#"))
    val lentries = validLines.map(entry => catParser.parseLexEntry(entry))
    lentries.groupBy(_.word).mapValues {
      entries => entries.map(_.category).toSet
    }
  }
}

class MissingLexicalEntryException (msg: String) extends Throwable(msg) {
  override def fillInStackTrace = this
}


