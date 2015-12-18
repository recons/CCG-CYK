import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by sergej on 16.12.15.
 */
class CategoryParser extends JavaTokenParsers {
  // Turn a category string (like "s\np") into a Category object.
  def apply(catString: String): Category = parseAll(cat, catString) match {
    case Success(x, remainder) => x
    case Failure(msg, remainder) => throw new RuntimeException(msg)
  }

  // Turn a lexical entry (like "walks := s\np") into a LexicalEntry object.
  def parseLexEntry(entryString: String): LexicalEntry = parseAll(lexEntry, entryString) match {
    case Success(x, remainder) => x
    case Failure(msg, remainder) => {
      throw new CategoryParserException("\n\nCouldn't process the following entry:\n\n" + entryString
        + "\n\nError: "  + msg + "\n")
    }
  }

  // The remaining methods specify the production rules of the
  // context-free grammar that defines Categories and lexical entries.

  def lexEntry: Parser[LexicalEntry] =
    word ~ ":=" ~ cat ^^ { case w ~ ":=" ~ c => LexicalEntry(w,c) }

  def cat: Parser[Category] = complexCat | atomCat

  def innerCat: Parser[Category] = atomCat | "(" ~> complexCat <~ ")"

  def atomCat: Parser[AtomCategory] =
    acString ^^ {
      case ac => AtomCategory(ac)
    }


  def complexCat: Parser[ComplexCategory] =
    innerCat ~ slash ~ innerCat ^^ { case r~s~a => ComplexCategory(r,s,a) }

  def slash: Parser[Slash] = (
    "\\" ^^ (x => Left)
      | "/"  ^^ (x => Right)
    )

  def acString: Parser[String] = """[a-z][a-z0-9_]*""".r
  def constant: Parser[String] = """[a-z0-9][A-Za-z0-9_]*""".r
  def varString: Parser[String] = """[A-Z][A-Z0-9_]*""".r
  def word: Parser[String] = """[^\s]+""".r
}

class CategoryParserException (msg: String) extends Throwable(msg) {
  override def fillInStackTrace = this
}
