package info.kwarc.mmt.stex.ml

import info.kwarc.mmt.api.ParseError
import info.kwarc.mmt.api.archives.source
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.{File, Reflection}
import info.kwarc.mmt.stex.parsing.stex.{DefinameRule, DefiniendumRule, ImportModuleRuleLike, STeXParser, SymDeclRule, SymDefRule, SymnameRule, SymnamesRule, SymrefApp, SymrefRule, TextSymDeclRule, UseStructureRule, VarDefRule, VarSeqRule}
import info.kwarc.mmt.stex.parsing.{Environment, MacroApplication, PlainMacro, PlainText, SkipCommand, TeXTokenLike}

import scala.collection.mutable

sealed trait TargetClass
case object NonSemantic extends TargetClass
case object Semantic extends TargetClass
case class PreToken(str:String,tk: TeXTokenLike,cls:TargetClass)

case class SubResult(str: String, score: Float, start: Int, end: Int)

case class Result(str: String, tk: TeXTokenLike, res: List[SubResult])

object Model {
  def tokenizeAll(controller:Controller,macrostrip:Boolean = true)(cont : List[PreToken] => Unit) = {
    val parser = new STeXParser(controller)
    parser.init()
    val files = controller.backend.getArchives.collect {
      case a if a.properties.get("format").contains("stex") && (a / source).exists() =>
        (a,(a / source).descendants.filter{f => f.getExtension.contains("tex") && !a.ignore((a / source).relativize(f).toFilePath)})
    }
    files.foreach {case (a,fs) => fs.foreach { f =>
      try {
        print(".")
        tokenize(parser(f, Some(a)), macrostrip) match {
          case Nil =>
          case tks => cont(tks)
        }
      } catch {
        case ParseError(_) =>
      }
    }}
  }


  case class Result(training_loss:Float,validation_loss:Float,epochs:Int)
  def fineTune(controller:Controller,jarfile:File,modelpath:File,macrostrip:Boolean = true) = {
    val reflection = new Reflection(jarfile)
    import info.kwarc.mmt.api.utils.Reflection._
    val model = reflection.getRefClass("info.kwarc.mmt.stex.lsp.languagemodel.HuggingFaceTokenModel").getInstance()
    var tks:List[Array[String]] = Nil
    var lbls:List[Array[Int]] = Nil
    tokenizeAll(controller) { ls =>
      tks ::= ls.map(_.str).toArray
      lbls ::= ls.map(_.cls match {
        case NonSemantic => 0
        case Semantic => 1
      }).toArray
    }
    import scala.jdk.CollectionConverters._
    val jtks = tks.reverse.asJava
    val jlbls = lbls.reverse.asJava
    val rescls = reflection.getRefClass("ai.djl.training.TrainingResult")
    val res = reflection.safely {
      model.method("finetune",Reflected(rescls),modelpath.toString,jtks,jlbls)
    }
    val ret = Result(
      res.method("getTrainLoss",float),
      res.method("getValidateLoss",float),
      res.method("getEpoch",int)
    )
    print("Done")
    ret
  }

  def tokenize(textokens: List[TeXTokenLike],macrostrip:Boolean) = {
    var ret: List[PreToken] = Nil

    def dostuff(tk: TeXTokenLike): Unit = tk match {
      case info.kwarc.mmt.stex.parsing.Math(_, _, _) =>
      case txt: PlainText if !txt.str.isBlank =>
        ret ::= PreToken(txt.str, txt, NonSemantic)
      case ma: SymrefApp => ma.rule match {
        case Some(SymrefRule(_, _) | SymnameRule(_, _, _) | SymnamesRule(_, _, _) | DefinameRule(_, _, _) | DefiniendumRule(_)) => ret ::= PreToken(ma.asPlainString, ma, Semantic)
        //case _ if !macrostrip => ma.iterateChildren(dostuff)
        case _ =>
      }
      case ma: MacroApplication => ma.rule match {
        case Some(_: SymDeclRule | _: TextSymDeclRule | _: SymDefRule | _: VarDefRule | _: VarSeqRule | _: ImportModuleRuleLike | _: UseStructureRule | _: SkipCommand) =>
        case _ => ma.iterateChildren(dostuff)
      }
      case pm: PlainMacro if !macrostrip => ret ::= PreToken(pm.toString, pm, NonSemantic)
      case o => o.iterateChildren(dostuff)
    }

    val alltks = textokens.collectFirst { case e: Environment if e.toString.startsWith("\\begin{document") => e.allChildren }.getOrElse(textokens)
    alltks.foreach(dostuff)
    ret.reverse
  }

}

class Model(jarfile:File,zipfile:File,macrostrip:Boolean) {
  private val reflection = new Reflection(jarfile)
  import info.kwarc.mmt.api.utils.Reflection._
  private val tokenmodel = reflection.getRefClass("info.kwarc.mmt.stex.lsp.languagemodel.HuggingFaceTokenModel").getInstance()
  reflection.safely { tokenmodel.method("initialize", unit, zipfile.toString) }
  private val predictionresult = reflection.getRefClass("info.kwarc.mmt.stex.lsp.languagemodel.PredictionResult")
  private val singleprediction = reflection.getRefClass("info.kwarc.mmt.stex.lsp.languagemodel.SinglePrediction")
  private val charspan = reflection.getRefClass("ai.djl.huggingface.tokenizers.jni.CharSpan")
  private def predictOrig(ls:Array[String]) = tokenmodel.method("predict",Reflected(predictionresult),ls)

  def tokenize(textokens: List[TeXTokenLike]) = Model.tokenize(textokens,macrostrip)

  def predict(textokens: List[TeXTokenLike]) = {
    predictOnTokens(tokenize(textokens))
  }
  def predictOnTokens(ls: List[PreToken]) = {
    var todo : List[PreToken] = Nil
    var ret : List[Result] = Nil
    var reti : Int = 0
    var curr = 0.0
    ls.foreach { tk =>
      todo ::= tk
      curr += tk.str.length.toDouble / 3.5
      if (curr > 512) {
        curr = 0
        val (a,i) = predictOnTokensI(todo.reverse)
        todo = Nil
        reti += i
        ret = ret ::: a
      }
    }
    if (todo.nonEmpty) {
      val (a, i) = predictOnTokensI(todo.reverse)
      reti += i
      ret = ret ::: a
    }
    (ret,reti)
  }
  private def predictOnTokensI(ls:List[PreToken]) = {
    val orig = predictOrig(ls.map(_.str).toArray)
    val numTokens = orig.field("numTokens",int)
    val groupedPredictions = orig.field("groupedPredictions",JList(JLinkedList(Reflected(singleprediction))))
    (groupedPredictions.zip(ls).collect {
      case (res,tk) if tk.cls == NonSemantic => Result(tk.str,tk.tk,res.map{r =>
        val cs = r.field("charSpan",Reflected(charspan))
        val start = cs.method("getStart",int)
        val end = cs.method("getEnd",int)
        SubResult(r.field("token",string),r.field("positiveCertainty",float),tk.tk.startoffset + start,tk.tk.startoffset + end)
      })
    },numTokens)
  }
}