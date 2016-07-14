package info.kwarc.mmt.api

import symbols._
import objects._

/**
 * messages define the abstract syntax of a protocol for communicating between content processing systems
 * 
 * The classes use string arguments (as opposed to [[Declaration]] and [[Term]]) together with format strings.
 * 
 * It exposes the main content-related commands that can be sent to MMT.
 * In particular, messages can be passed to MMT via HTTP using [[web.MessageHandler]]
 */
sealed abstract class Message {
  
}


/** messages that result in structural changes to theories and are thus stateful */
sealed abstract class StructureMessage extends Message {
  
}

/** messages that call algorithms on MMT objects
 *  they may create state changes only indirectly by recording the result of the algorithm in the current theory
 */
sealed abstract class ObjectMessage extends Message {
  /** the context of the judgment (if omitted use the minimal context in which the input is well-formed) */
  def context: Option[Context]
  /** inFormat the format in which the input term is given */
  def inFormat: String
  /** the input term */
  def in: String
  /** inFormat the format in which the output term is to be presented */
  def outFormat: String
}

/**
 * adding a declaration to a theory
 * @param path the URI of the declaration to retrieve
 * @param outFormat the format used for presenting the declaration (see [[presentation.Presenter]])
 */
case class GetMessage(path: Path, outFormat: String) extends StructureMessage

/**
 * adding a declaration to a theory
 * @param path the URI of the declaration to delete
 */
case class DeleteMessage(path: Path) extends StructureMessage

/**
 * adding a declaration to a theory
 * @param thy the theory to which the declaration is added
 * @param inFormat the format used for interpreting the declaration (see [[checking.Interpreter]])
 * @param decl the declaration to add
 */
case class AddMessage(thy: Term, inFormat: String, decl: String) extends StructureMessage

/**
 * adding a declaration to a theory
 * @param thy the theory to which the declaration is added
 * @param inFormat the format in which the declaration is given
 * @param decl the new declaration (replaces the declaration given by its URI)
 */
case class UpdateMessage(thy: Term, inFormat: String, decl: String) extends StructureMessage

/**
 * exhaustively simplify a term in to a term out
 * postcondition: thy |- in = out
 */
case class EvaluateMessage(context: Option[Context], inFormat: String, in: String, outFormat: String) extends ObjectMessage

/**
 * infer the type of a given term
 * postcondition: thy |- in : out
 */
case class InferMessage(context: Option[Context], inFormat: String, in: String, outFormat: String) extends ObjectMessage

/**
 * find a term of a given type
 * postcondition: thy |- out : in
 */
case class ProveMessage(context: Option[Context], inFormat: String, in: String, outFormat: String) extends ObjectMessage

/** type of responses for a [[Message]] */
sealed abstract class Response

case class StructureResponse(id: String) extends Response

case class ObjectResponse(result: String, tp: String) extends Response

case class ErrorResponse(message: String) extends Response
