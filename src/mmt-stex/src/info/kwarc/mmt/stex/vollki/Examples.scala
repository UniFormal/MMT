package info.kwarc.mmt.stex.vollki
/*
import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.stex.xhtml.HTMLParser.HTMLNode

case class Example(text: HTMLNode, generated_knowledge: List[CognitiveValue],  // = annotation \objective{}{}{} + (remember,none,whatever-this-is-an-example-for)
                  preconditions:List[CognitiveValue] // <- automatically contains everything included via \usemodule or \importmodule
                                                      // ... that is actually being referenced in the example text
                                                      // as \precondition{remember}{none}{whatever}
                  )

object Example {
  private def getExamples(t: GlobalName): List[Example] = ???

  // --------------------------------------------------------------------

  /**
    * Should filter examples by preconditions and some learning *goal* and return the best suited examples
    * for a given user (by whatever metric).
    * @param user a User model with .getValue (CognitiveDimension x Concept) -> Double
    * @param learning_object triple of e.g. (understand,apply,group)
    */
  def getExamples(user: UserModel, learning_object:CognitiveValue): Unit = {
    //val all_examples = getExamples(learning_object).filter(preconditions are satisfied)
    /*
    val all_examples = getExamples(learning_object).filter(preconditions are satisfied)
    val introductory = all_examples.filter(_.generated_knowledge.contains(learning_object))
    val safe = all_examples.filter(_.generated_knowledge.isEmpty)
    val generates_knowledge = all_examples.filter{ ex =>
      val gn = ex.generated_knowledge
      gn.nonEmpty && !gn.contains(learning_object)
    }
    introductory ::: safe ::: generates_knowledge
     */
  }
}

 */