package info.kwarc.mmt.api.presentation

/**
 * describes the special attributes used in generated HTML and MathML
 */
object HTMLAttributes {
   /**
    * HTML5-compliant pseudo namespace of MMT-generated attributes
    */
   val prefix = "data-mmt-"
   /** reference to the declaration of the present symbol occurrence
    *  present on <mo> elements
    */
   val symref = prefix + "symref"
   /** position of the declaration of the present variable occurrence
    *  present on <mi> elements
    */
   val varref = prefix + "varref"
   /** reference to the source of the present object
    *  present on some grouping MathML elements
    */
   val source = prefix + "source"
   /** reference to declaration in which the present object occurs (3-part MMT URI)
    *  present on <math> elements
    */
   val owner = prefix + "owner"
   /** the component of the owner in which the present object occurs (string, e.g., "definition" or "type")
    *  present on <math> elements
    */
   val component = prefix + "component"
   /** the position within the present object, i.e., the parallel markup pointer (as produced by [[Position]])
    *  present on all grouping MathML elements
    */
   val position = prefix + "position"
}

