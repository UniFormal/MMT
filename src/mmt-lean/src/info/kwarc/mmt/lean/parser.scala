package info.kwarc.mmt.lean

import java.io.{ FileInputStream, InputStream }
import java.nio.charset.Charset

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait ExportFileCommand
case class ExportedModification(modification: Modification) extends ExportFileCommand
case class ExportedNotation(notation: Notation) extends ExportFileCommand

private class TextExportParser {
  val name: mutable.ArrayBuffer[Name] = mutable.ArrayBuffer[Name]()
  val level: mutable.ArrayBuffer[Level] = mutable.ArrayBuffer[Level]()
  val expr: mutable.ArrayBuffer[Expr] = mutable.ArrayBuffer[Expr]()

  name += Name.Anon
  level += Level.Zero

  @tailrec final def write[T](b: mutable.ArrayBuffer[T], i: Int, t: T, default: T): Unit =
    b.size match {
      case `i` => b += t
      case s if s < i =>
        b += default
        write(b, i, t, default)
      case s if s > i =>
        b(i) = t
    }
}

private final class LinesParser(textExportParser: TextExportParser, bytes: Array[Byte], end: Int) {
  import textExportParser._

  var index = 0
  def hasNext(): Boolean = index < end
  def cur(): Char = bytes(index).toChar
  def next(): Char = {
    if (!hasNext()) throw new IndexOutOfBoundsException
    val c = cur()
    index += 1
    c
  }

  def consume(c: Char): Unit = if (next() != c)
    throw new IllegalArgumentException(s"expected $c, got ${cur()}")
  def consume(s: String): Unit = s.foreach(consume)

  def lines(): Vector[ExportFileCommand] = {
    val out = Vector.newBuilder[ExportFileCommand]
    // FR parser always expects Unix file endings; Lean on Windows produces Windows file endings; so we consume \r as well if we find it
    while (hasNext()) { line().foreach(out += _); if (cur() == '\r') consume('\r'); consume('\n') }
    out.result()
  }

  def line(): Option[ExportFileCommand] =
    next() match {
      case c if '0' <= c && c <= '9' =>
        val n = long(c - '0').toInt
        consume(' '); consume('#')
        next() match {
          case 'N' => write(name, n, nameDef(), Name.Anon)
          case 'U' => write(level, n, levelDef(), Level.Zero)
          case 'E' => write(expr, n, exprDef(), Sort.Prop)
        }
        None
      case '#' =>
        next() match {
          case 'A' =>
            consume('X')
            val n = spc(nameRef())
            val t = spc(exprRef())
            val ups = univParams()
            Some(ExportedModification(AxiomMod(n, ups, t)))
          case 'D' =>
            consume('E'); consume('F')
            val n = spc(nameRef())
            val t = spc(exprRef())
            val v = spc(exprRef())
            val ups = univParams()
            Some(ExportedModification(DefMod(n, ups, t, v)))
          case 'Q' =>
            consume("UOT")
            Some(ExportedModification(QuotMod))
          case 'I' =>
            consume('N')
            next() match {
              case 'F' =>
                consume("IX ")
                Some(ExportedNotation(Infix(nameRef(), spc(num()), spc(rest()))))
              case 'D' =>
                val numParams = spc(num())
                val n = spc(nameRef())
                val t = spc(exprRef())
                val numIntros = spc(num())
                val rest = restOf(num())
                val (intros, ps) = rest.splitAt(2 * numIntros)
                Some(ExportedModification(IndMod(
                  n, ps.view.map(name).map(Level.Param).toVector, t,
                  numParams, intros.grouped(2).map { case Seq(in, it) => (name(in), expr(it)) }.toVector)))
            }
          case 'P' =>
            next() match {
              case 'R' =>
                consume("EFIX ")
                Some(ExportedNotation(Prefix(nameRef(), spc(num()), spc(rest()))))
              case 'O' =>
                consume("STFIX ")
                Some(ExportedNotation(Postfix(nameRef(), spc(num()), spc(rest()))))
            }
        }
    }

  def num(): Int = long().toInt
  def long(): Long =
    next() match { case c if '0' <= c && c <= '9' => long(c - '0') }
  def long(acc: Long): Long =
    cur() match {
      case c if '0' <= c && c <= '9' =>
        next()
        long(10 * acc + (c - '0'))
      case _ => acc
    }

  def rest(): String = {
    val start = index
    // FR allow \r line ending (see other FR comment)
    def nextNL(): Int = if (cur() == '\n' || cur() == '\r') index else { next(); nextNL() }
    new String(bytes, start, math.max(nextNL() - start, 0), LinesParser.UTF8)
  }

  def nameRef(): Name = name(num())
  def nameDef(): Name =
    next() match {
      case 'S' => Name.Str(spc(nameRef()), spc(rest()))
      case 'I' => Name.Num(spc(nameRef()), spc(long()))
    }

  def levelRef(): Level = level(num())
  def levelDef(): Level =
    next() match {
      case 'S' => Level.Succ(spc(levelRef()))
      case 'M' => Level.Max(spc(levelRef()), spc(levelRef()))
      case 'I' => Level.IMax(c('M', spc(levelRef())), spc(levelRef()))
      case 'P' => Level.Param(spc(nameRef()))
    }

  @inline def restOf[T](p: => T): Vector[T] = {
    val out = Vector.newBuilder[T]
    while (cur() == ' ') { next(); out += p }
    out.result()
  }

  def binderInfo(): BinderInfo = {
    consume('#'); consume('B')
    next() match {
      case 'D' => BinderInfo.Default
      case 'I' => BinderInfo.Implicit
      case 'C' => BinderInfo.InstImplicit
      case 'S' => BinderInfo.StrictImplicit
    }
  }

  @inline def c[T](c: Char, p: => T): T = { consume(c); p }
  @inline def spc[T](p: => T): T = c(' ', p)

  def exprRef(): Expr = expr(num())
  def exprDef(): Expr =
    next() match {
      case 'V' =>
        Var(spc(num()))
      case 'S' =>
        Sort(spc(levelRef()))
      case 'C' =>
        Const(spc(nameRef()), restOf(levelRef()))
      case 'A' =>
        App(spc(exprRef()), spc(exprRef()))
      case 'L' =>
        val b = spc(binderInfo())
        val n = spc(nameRef())
        val d = spc(exprRef())
        val e = spc(exprRef())
        Lam(Binding(n, d, b), e)
      case 'P' =>
        val b = spc(binderInfo())
        val n = spc(nameRef())
        val d = spc(exprRef())
        val e = spc(exprRef())
        Pi(Binding(n, d, b), e)
      case 'Z' =>
        val n = spc(nameRef())
        val t = spc(exprRef())
        val v = spc(exprRef())
        val e = spc(exprRef())
        Let(Binding(n, t, BinderInfo.Default), v, e)
    }

  def univParams(): Vector[Level.Param] =
    restOf(Level.Param(nameRef()))
}
object LinesParser {
  val UTF8: Charset = Charset.forName("UTF-8")
}

object TextExportParser {
  @tailrec private def reverseIndexOf(chunk: Array[Byte], needle: Byte, from: Int): Int =
    if (chunk(from) == needle) from
    else if (from == 0) -1
    else reverseIndexOf(chunk, needle, from - 1)

  def parseStream(in: InputStream): LazyList[ExportFileCommand] = {
    def bufSize = 8 << 10
    case class Chunk(bytes: Array[Byte], endIndex: Int)
    def readChunksCore(buf: Array[Byte], begin: Int): LazyList[Chunk] = {
      val len = in.read(buf, begin, buf.length - begin)
      if (len <= 0) LazyList.empty else {
        val nl = reverseIndexOf(buf, '\n', begin + len - 1)
        if (nl == -1) {
          // no newline found in the whole chunk,
          // this should only happen at the end but let's try again to make sure
          Chunk(buf, len) #:: readChunks()
        } else {
          val nextBuf = new Array[Byte](bufSize)
          val reuse = (begin + len) - (nl + 1)
          System.arraycopy(buf, nl + 1, nextBuf, 0, reuse)
          Chunk(buf, nl + 1) #:: readChunksCore(nextBuf, reuse)
        }
      }
    }
    def readChunks(): LazyList[Chunk] = readChunksCore(new Array[Byte](bufSize), 0)
    val parser = new TextExportParser
    readChunks().flatMap(chunk => new LinesParser(parser, chunk.bytes, chunk.endIndex).lines())
  }

  def parseFile(fn: String): LazyList[ExportFileCommand] = parseStream(new FileInputStream(fn))
}
