package info.kwarc.mmt.stex.search

import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.archives.{Archive, Dim}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.{File, JSONArray, JSONObject, JSONString}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.{Field, FieldType, StringField, TextField, Document => LuceneDocument}
import org.apache.lucene.index._
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.grouping.{GroupDocs, GroupingSearch}
import org.apache.lucene.search.{IndexSearcher, Sort}
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.BytesRef

import scala.collection.mutable
import scala.util.Try

class SearchDocument(private[search] val file : File,sourcefile:File,archive:Archive,uri:Path) {

  private[search] var docs : List[LuceneDocument] = Nil
  def add(tp:String,text:String,source:String,others:(String,String)*) = {
    val d = new LuceneDocument()
    docs ::= d
    d.add(new UriField(uri.toString))
    d.add(new StringField("archive",archive.id,Field.Store.YES))
    d.add(new StringField("sourcefile",(archive / info.kwarc.mmt.api.archives.source).relativize(sourcefile).toString,Field.Store.YES))
    d.add(new StringField("type",tp,Field.Store.YES))
    d.add(new TextField("text",text,Field.Store.YES))
    d.add(new TextField("source",source,Field.Store.YES))
    others.foreach(p => d.add(new TextField(p._1,p._2,Field.Store.YES)))
  }
  def save = Lucene.write(this)
}

class UriField(value:String) extends Field("DocURI",value,Lucene.ftype) with IndexableField {
  override def numericValue(): Number = value.hashCode

  override def binaryValue(): BytesRef = new BytesRef(value)
}

object Lucene {
  val ftype = new FieldType() with IndexableFieldType
  ftype.setOmitNorms(true)
  ftype.setIndexOptions(IndexOptions.DOCS)
  ftype.setStored(true)
  ftype.setTokenized(false)
  ftype.setDocValuesType(DocValuesType.SORTED)
  ftype.freeze()

  private lazy val dict = ???
  private[search] lazy val analyzer = new StandardAnalyzer()/*new Analyzer {
    override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
      val tokenizer = new StandardTokenizer()
      val output = new TokenFilter(new LowerCaseFilter(tokenizer)) {
        override def incrementToken(): Boolean = {
          if (!input.incrementToken()) return false
          ???
        }
      }
      new TokenStreamComponents(tokenizer,output)
    }
  }*/
  def embedd(s : String) : Array[Float] = ???
  def write(sd : SearchDocument) = {
    sd.file.mkdirs()
    sd.file.descendants.foreach(_.delete())
    val dir = FSDirectory.open(sd.file.toJava.toPath)
    val config = new IndexWriterConfig(analyzer)
    val writer = new IndexWriter(dir,config)
    sd.docs.foreach(writer.addDocument)
    writer.flush()
    writer.close()
  }
}

class Searcher(controller:Controller) {
  private val stores = mutable.HashMap.empty[String,MultiReader]
  private var searcher : Option[IndexSearcher] = None
  private def addArchiveI(a : Archive) = {
    val dir = a / Dim("export", "lucene")
    val directoryReaders = if (dir.exists()) {
      dir.descendants.collect {
        case f if f.isFile => f.up
      }.distinct.flatMap{d =>
        if (d.children.exists(_.name.startsWith("segments"))) {
          val dir = FSDirectory.open(d.toJava.toPath)
          try {
            /*Try(*/ Some(DirectoryReader.open(dir)) //).toOption.flatten
          } catch {
            case _ : IndexFormatTooOldException =>
              print("")
              None
          }
        } else None
      }
    } else Nil
    val reader = new MultiReader(directoryReaders :_*)
    stores(a.id) = reader
  }
  private def update = {
    searcher = Some(new IndexSearcher(new MultiReader(stores.values.toList :_*)))
  }
  def addArchive(a : Archive*) = {
    a.foreach(addArchiveI)
    update
  }
  addArchive(controller.backend.getArchives :_*)

  def search(s : String, results : Int = 10, types:List[String]=Nil, skiparchives:List[String] = Nil) : List[SearchResult] = {
    val gs = new GroupingSearch("DocURI").setGroupSort(Sort.RELEVANCE).setCachingInMB(4.0,true).setAllGroups(true)
      .setGroupDocsLimit(10).setGroupDocsOffset(0).setSortWithinGroup(Sort.RELEVANCE)
    var qs = ""
    types match {
      case Nil => qs = "(text: " + s + ") OR (title: " + s + ")"
      case List("title") => qs = "(title: " + s + ")"
      case ls =>
        qs = ls.map(f => "(type: \"" + f + "\")").mkString("("," OR ",")")
        qs += " AND ((text: " + s + ") OR (title: " + s + "))"
    }
    val query = new QueryParser("text",Lucene.analyzer).parse(qs)
    val search = skiparchives match {
      case Nil => searcher.getOrElse(return Nil)
      case ls =>
        new IndexSearcher(new MultiReader(controller.backend.getArchives.filterNot(a => ls.contains(a.id)).flatMap(a => stores.get(a.id)):_*))
    }
    val res = gs.search(search,query,0,results).groups.toList.asInstanceOf[List[GroupDocs[Nothing]]].sortBy(p => -p.maxScore)
    //.map(rs => (rs.maxScore,rs.scoreDocs.map(d => (d.score, search.doc(d.doc))).sortBy(-_._1).toList))
    res.map{rs =>
      SearchResult(rs.maxScore,rs.scoreDocs.map(d => (d.score,search.doc(d.doc))).sortBy(-_._1).toList)
    }
  }
}

case class SearchResult(score:Float,docs:List[(Float,LuceneDocument)]) {
  lazy val archive = docs.head._2.get("archive")
  lazy val sourcefile = docs.head._2.get("sourcefile")
  lazy val fragments = docs.map(d => (d._2.get("type"),if (d._2.getFields("title").nonEmpty) d._2.get("title") else "",d._2.get("source")))
}

class SearchServer extends ServerExtension("stexsearch") {
  lazy val searcher = new Searcher(controller)
  override def apply(request: ServerRequest): ServerResponse = {
    val types = request.parsedQuery("types").getOrElse("").split(',').map(_.trim).filterNot(_ == "").toList
    val query = request.parsedQuery("query").getOrElse {
      return ServerResponse.JsonResponse(JSONArray())
    }
    val results = searcher.search(query, 10, types)
    ServerResponse.JsonResponse(JSONArray(
      results.map(sr => JSONObject(
        ("archive", JSONString(sr.archive)),
        ("sourcefile", JSONString(sr.sourcefile)),
        ("title", JSONString(sr.fragments.head._2)),
        ("html", JSONString(sr.fragments.head._3))
      ))
        : _*))

  }
}
