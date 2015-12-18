/**
 * Created by sergej on 16.12.15.
 */
object Rule {
  lazy val abRules = List(ForwardApplication, BackwardApplication) ::: List(ForwardConjunction, BackwardConjunction)
  lazy val compositionRules = abRules ::: List(ForwardHarmonicComposition, BackwardHarmonicComposition) ::: List(ForwardCrossedComposition, BackwardCrossedComposition)
  lazy val allRules = compositionRules
}

trait Rule {
  def apply (first: Category, second: Category): Option[Category]
}

object ForwardApplication extends Rule {
  def apply (first: Category, second: Category) = first match {
    case ComplexCategory(res, Right, arg) =>
      if (arg == second) Some(res) else None
    case _ => None
  }
}

object BackwardApplication extends Rule {
  def apply (first: Category, second: Category) = second match {
    case ComplexCategory(res, Left, arg) =>
      if (arg == first) Some(res) else None
    case _ => None
  }
}

abstract class Composition(fslash: Slash, sslash: Slash) extends Rule {
  def getResult (fres: Category, farg: Category, sres: Category, sarg: Category): Option[Category]

  def apply (first: Category, second: Category) = (first, second) match {
    case (ComplexCategory(fres, firstCategorySlash, farg), ComplexCategory(sres, secondCategorySlash, sarg)) =>
      if (fslash.equals(firstCategorySlash) && sslash.equals(secondCategorySlash))
        getResult(fres, farg, sres, sarg)
      else
        None
    case _ => None
  }

}

class ForwardComposition(fslash: Slash, sslash: Slash) extends Composition(fslash, sslash) {
  def getResult (fres: Category, farg: Category, sres: Category, sarg: Category) =
    if (farg == sres) Some(ComplexCategory(fres, sslash, sarg)) else None
}

class BackwardComposition(fslash: Slash, sslash: Slash) extends Composition(fslash, sslash) {
  def getResult (fres: Category, farg: Category, sres: Category, sarg: Category) =
    if (sarg == fres) Some(ComplexCategory(sres, fslash, farg)) else None

}

object ForwardHarmonicComposition extends ForwardComposition(Right, Right)
object ForwardCrossedComposition extends ForwardComposition(Right, Left)
object BackwardHarmonicComposition extends BackwardComposition(Left, Left)
object BackwardCrossedComposition extends BackwardComposition(Right, Left)

object ForwardTypeRaising extends Rule {
  def apply (first: Category, second: Category) = (first, second) match {
    case (AtomCategory(x), ComplexCategory(ComplexCategory(f, Left, AtomCategory(s)), Right, arg)) =>
      if (x == s) Some(ComplexCategory(f, Right, ComplexCategory(f, Left, AtomCategory(s)))) else None
    case _ => None
  }
}

object BackwardTypeRaising extends Rule {
  def apply (first: Category, second: Category) = (first, second) match {
    case (ComplexCategory(ComplexCategory(f, Right, AtomCategory(s)), Left, arg), AtomCategory(x)) =>
      if (x == s) Some(ComplexCategory(f, Left, ComplexCategory(f, Right, AtomCategory(s)))) else None
    case _ => None
  }
}

object ForwardConjunction extends Rule {
  def apply (first: Category, second: Category) = second match {
    case AtomCategory("conj") => Some(ComplexCategory(first, Right, first))
    case _ => None
  }
}

object BackwardConjunction extends Rule {
  def apply (first: Category, second: Category) = first match {
    case AtomCategory("conj") => Some(ComplexCategory(second, Left, second))
    case _ => None
  }
}