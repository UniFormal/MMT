package info.kwarc.mmt.oeis.parser

import info.kwarc.mmt.oeis.processor.{TextParserIns, TextParser}

import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import scala.xml._

object DocumentParser {
  //base is mmt-api/trunk
  val dictionary = Source.fromFile("../../mmt-oeis/resources/dictionary").getLines().toSet

  private val IDregex = "A\\d+".r

  private def assertion(xclass : String, cmpval : String) : Elem ={
    <assertion class={xclass}>
      {CMP(cmpval)}
    </assertion>
  }

  private def omtext(xclass : String, cmpval : String) : Elem = {
    <omtext class={xclass}>
      {CMP(cmpval)}
    </omtext>
  }

  private def CMP(value : String) : Elem = <CMP>{value}</CMP>

  private def omdoc(element : Elem) : Elem = <omdoc:p>{element}</omdoc:p>

  private def getTheoryID(line : String) : List[String] ={
    (IDregex findAllIn  line).toList
  }

  def fromReaderToXML(source : BufferedSource) : Elem = {

    def addHeaders(xml : List[Elem], theory : String) : Elem = {
      <omdoc xmlns="http://omdoc.org/ns" xmlns:omdoc="http://omdoc.org/ns" xmlns:html="http://www.w3.org/1999/xhtml" xml:id={theory+".omdoc"}>
        <!--This OMDoc file is generated from an OEIS entry, do not edit-->
        <theory name={theory}>
          {xml}
        </theory>
      </omdoc>
    }

    def omdocWrapperCMP(xclass : String, cmpval : String) = omtext(xclass, cmpval)
    def omdocWrapperAs(xclass : String, cmpval : String) = assertion(xclass, cmpval)

    var theory : Option[String] = None

    val xml: List[Any] = source.getLines().toList.collect({
      case line if line.length > 2 =>
        val contentIndex: Option[Match] = IDregex.findFirstMatchIn(line)

        if(!contentIndex.isEmpty && theory.isEmpty){
          theory = Some(contentIndex.get.matched)
        }

        line.substring(0,2) match{
          case "%N" =>  omdocWrapperCMP("name", line.substring(contentIndex.get.end))
          case "%S" =>  omdocWrapperCMP("starts-with", line.substring(contentIndex.get.end))
          case "%C" =>  omdocWrapperCMP("comment", line.substring(contentIndex.get.end))
          case "%D" =>  omdocWrapperCMP("reference", line.substring(contentIndex.get.end))
          case "%H" =>  omdocWrapperCMP("link", line.substring(contentIndex.get.end))
          case "%F" =>  formulaWrap(line.substring(contentIndex.get.end), theory.get)
          case "%Y" =>  omdocWrapperAs("crossref", line.substring(contentIndex.get.end))
          case "%K" =>  omdocWrapperAs("keywords", line.substring(contentIndex.get.end))
          case "%A" =>  omdocWrapperAs("author", line.substring(contentIndex.get.end))
          case "%p" =>  omdocWrapperCMP("maple", line.substring(contentIndex.get.end))
          case "%t" =>  omdocWrapperCMP("mathematica", line.substring(contentIndex.get.end))
          case "%o" =>  omdocWrapperCMP("program", line.substring(contentIndex.get.end))
          case "%O" =>  omdocWrapperAs("offset", line.substring(contentIndex.get.end))
          case "%E" =>  omdocWrapperAs("extensions", line.substring(contentIndex.get.end))
          case "%e" =>  omdocWrapperAs("example", line.substring(contentIndex.get.end))
          case "%T" =>  omdocWrapperAs("***** UUUU *****", line.substring(contentIndex.get.end))
          case "%U" =>  omdocWrapperAs("***** IIII *****", line.substring(contentIndex.get.end))
          case "%I" =>  omdocWrapperAs("***** TTTT *****", line.substring(contentIndex.get.end))
          case a if line.startsWith("%") => omdocWrapperAs("notsupported","Unexpected tag!")
          case _ =>
        }
    })


    addHeaders(xml collect {case a : Elem => a}, theory.get)
  }


  def split(s:String, l:List[String]):List[(String,String)] = {
    if(s == null){
      return Nil
    }

    val delimRegex = l.mkString("|")
    val r = "("+delimRegex+")?(.*?)(("+delimRegex+").*)?"
    val R = r.r

    s match {
      case R(delim, text, rest, _) if delim == null => (text, "") :: split(rest, l)
      case R(delim, text, rest, _) => (text, delim) :: split(rest, l)
      case x => Nil
    }
  }

  def extractFormula(line : String): (List[String], List[Int]) = {
    val month = "(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"

    /*Month day year*/
    val date = month + "\\s*\\d{0,4}\\s*\\d{0,4}"

    val delimiters = List(date, " " ,
      ":",
      "(?<![A-Z]{1})(?<!\\.)[\\.\\;,](?!\\d+)(?!\\.)(?![A-Za-z\\:])"
    )

    /*More than 2 characters not followed by any kind of open paran
    * OR _NAME's Surname_
    * */
    val word: Regex = "([A-Za-z]{2,}\\b(?!(\\(|\\{\\[)))|(([A-Za-z]+\\.+(?!(\\(|\\{\\[)))+)|(_*[A-Z]+[\\sA-Za-z'\\.]+(?!\\()_*)".r

    /*words that should not be searched in a dictionary, they are not words by definition*/
    val funcs : List[String] = List("mod", "pi", "phi", "log", "divide", "divides")
    var balancedPar = 0

    def isWord(token : String, dropped : Boolean) : Boolean = {
      if(dropped && token.trim == "-") return true
      if(funcs.contains(token.trim)) return false

      val result = dictionary.contains(token.trim) ||
        delimiters.exists(x => token.matches(x)) ||
        token.matches(word.regex) ||
        token.trim.matches(word.regex) ||
        delimiters.exists(x => token.matches(x.trim)) ||
        token.trim == ""

      /*Detect words of the form -- (continues here) OR bla (is greater) */
      if(token.length > 4 && (token(0) == '(' || token(token.length-1) == ')')){
        var newToken = token
        if(newToken(0) == '('){
          newToken = newToken.substring(1)
        }
        if(newToken(newToken.length-1) == ')'){
          newToken = newToken.dropRight(1)
        }
        return result || isWord(newToken, dropped)
      }

      result
    }

    def isDelim(token : String) : Boolean = {
      delimiters.exists(x => token.matches(x.trim)) || delimiters.exists(x => token.matches(x))
    }

    def computeBalancedPar(word : String) : Int = {
      word.count(_ == '(') - word.count(_ == ')') +
        word.count(_ == '{') - word.count(_ == '}') +
        word.count(_ == '[') - word.count(_ == ']')
    }

    //contains pairs (word, delimiter) - delimiters will be needed to put the line together again
    var words: List[(String,String)] = split(line, delimiters )
    var transformed : List[String] = Nil
    var formulaPosition : List[Int] = Nil

//    println(words)
    var temp : List[String] = Nil
    var delims : List[String] = Nil

    //was a "-" dropped in the last formula try
    var dropped = false

    //Take the words first, when you can't see words anymore start taking formulas until you spot a word
    while(words.nonEmpty) {

      while (words.nonEmpty && isWord(words.head._1, dropped)) {
        balancedPar += computeBalancedPar(words.head._1)
        temp = temp :+ words.head._1
        delims = delims :+ words.head._2
        words = words.tail
        dropped = false
      }

      transformed = transformed :+ temp.zip(delims).map( wordDelim => wordDelim._2 + wordDelim._1).mkString("")

      if(words.nonEmpty){
        temp = Nil
        delims = Nil

        /*if we have a sum(x, then we know that the comma doesn't finish it (unbalanced parans)
        * OR prod(x=1..t: the colon doesn't finish the formula
        * */
        var isFormulaUnfinished = false

        while(words.nonEmpty && (!isWord(words.head._1, dropped) || isFormulaUnfinished)){
          balancedPar += computeBalancedPar(words.head._1)
          isFormulaUnfinished = false
          if(temp.length == 0 && words.head._2.trim == ":"){
            delims = delims :+ ""
          }else {
            delims = delims :+ words.head._2
          }
          temp = temp :+ words.head._1
          words = words.tail

          //not checking before while loop because the comma can only be after at least one expr.
          if(words.nonEmpty) {
            //if there is a comma inside two parans then it is part of the formula
            if ((words.head._2 == "," || words.head._2 == ":") && balancedPar > 0) {
              isFormulaUnfinished = true
            }
          }
        }

        //deal with sentences like x+23+a(x) - Euler
        if(temp.last == "-"){
          words = (temp.last, delims.last)::words
          temp = temp.dropRight(1)
          delims = delims.dropRight(1)
          dropped = true
        }


        transformed = transformed :+ temp.zip(delims).map( wordDelim => wordDelim._2 + wordDelim._1).mkString("")
        formulaPosition = formulaPosition :+ (transformed.length -1)
      }

      temp = Nil
      delims = Nil
    }

    transformed -> formulaPosition
  }


  private def parsedFormulaWrap(expr :Expression, theory : String) : Elem= {
    <OMOBJ>
      {expr.toNode(theory)}
    </OMOBJ>
  }

  def formulaWrap(line : String, theory : String ) : Elem = {
    <omdoc:p class="formula">
        {TextParserIns.parseLine(line, theory) match {
          case Some(a) => a.toNode(theory)
          case None => <CMP>{line}</CMP>
          }
        }
    </omdoc:p>
  }

//  private def formulaWrapper(line : String, theory : String) : Elem = {
//    val extracted = extractFormula(line)
//    val res = (extracted._1.toArray, extracted._2.toArray)
//    val formulaPosition = res._2
//    val tokenizedLine = res._1
//
//    val omdoc : Elem =
//      <CMP>
//        {
//        (0 to tokenizedLine.length - 1).map( i =>
//        {
//          if (formulaPosition.contains(i)) {
//            val parse = formulaParser.parse(res._1(i), theory)
//            parse match {
//              case Some(expression) =>
//                parsedFormulaWrap(expression, theory)
//              case None => {
//                tokenizedLine(i)
//              }
//            }
//          } else {
//            {tokenizedLine(i)}
//          }
//        })
//        }
//      </CMP>
//
//    val result  : Elem =
//      <omdoc:p class="formula">
//        {omdoc}
//      </omdoc:p>
//
//    result
//  }

  def getFormulas(source : BufferedSource) : List[String] = {

    val formula: List[(List[String], List[Int])] = source.getLines().toList.collect({
      case line if line.length > 2 =>

        val contentIndex: Option[Match] = IDregex.findFirstMatchIn(line)
        val scalaerror  = line.substring(0,2) match{
          case "%F" => extractFormula(line.substring(contentIndex.get.end))
          case _ => Nil -> Nil
        }

        scalaerror

    })


    formula.map(pair =>{
      pair._1.zipWithIndex.map({ case (word, index) =>
        if (pair._2.contains(index))
          "FORMULA " + word
        else
          word
      })
    }).flatten
  }

  def main(args : Array[String]) : Unit = {
    val res = extractFormula("Recurrence:A(x) + 3 + Prod_{i=1..t: t^2+4}. - Elhaida, Mar 20, 1994 ")
    println(res)
    println(res._1.zipWithIndex)
  }

}
