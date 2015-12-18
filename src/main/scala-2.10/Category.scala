/**
 * Created by sergej on 16.12.15.
 */
sealed trait Slash

/** A left slash in a CCG category. */
case object Left extends Slash { override def toString = "\\" }

/** A right slash in a CCG category. */
case object Right extends Slash { override def toString = "/" }

/** A trait for CCG categories. */
sealed trait Category {
  def equals (that: Category): Boolean
  val arity: Int
}

/** A companion object for the Cat trait. Contains a number of helper methods. */
object Category {
  def toStringHandleParens(cat: Category): String = cat match {
    case ComplexCategory(res, slash, arg) =>
      "(" + toStringHandleParens(res) + slash + toStringHandleParens(arg) + ")"
    case ac: AtomCategory =>
      ac.toString
  }
}

/**
 * An atomic category, like np, s and n.
 */
case class AtomCategory (name: String) extends Category {
  val arity = 1

  def equals (that: Category) = that match {
    case ac: AtomCategory => name == ac.name
    case _ => false
  }

  override def toString = name
}

/** A complex category, like s\np and (n\n)/(s/np) */
case class ComplexCategory (res: Category, slash: Slash, arg: Category) extends Category {
  val arity = res.arity + 1

  def equals(that: Category) = that match {
    case ComplexCategory(ores, oslash, oarg) =>
      oslash.equals(slash) && oarg.equals(arg) && ores.equals(res)
    case _ => false
  }

  override def toString =
    Category.toStringHandleParens(res) + slash.toString + Category.toStringHandleParens(arg)

}

class WorkerActor extends Actor {}

