package mizar.objects

/**
 * 	objects.Reasoning
 * 	Contains classes for handling Mizar Reasoning	
 * 	@author Mihnea Iancu
 *  @version 10/01/2011
 */


class MizLet(val nr : Int, val types : List[MizTyp])
class MizConclusion //TODO
class MizAssume(val props : List[MizProposition])
class MizGiven(val nr : Int, val exSt : MizProposition, val types : List[MizTyp], val props : List[MizProposition])
class MizTake(val term : MizTerm)
class MizTakeAsVar(val nr : Int, val typ : MizTyp, val term : MizTerm)

class MizReasoning