package info.kwarc.mmt.guidedtours.utils;

import java.lang.String
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.modules.DeclaredTheory
import objects._
import libraries._
import scala.concurrent._
import tiscaf._
import scala.collection.mutable.HashMap._
import info.kwarc.mmt.api.web._
import scala.util.parsing.json._
import scala.util._
import scala.annotation.tailrec
import scala.io._
import scala.io.Source

/**
 * MarkovClustering implements the Markov clustering (MCL) algorithm for graphs, using a HashMap-based sparse representation of a Markov matrix, i.e., an adjacency matrix m that is normalised to one. Elements in a column / node can be interpreted as decision probabilities of a random walker being at that node.
 *
 * @see http://java-ml.sourceforge.net/api/0.1.1/net/sf/javaml/clustering/mcl/MarkovClustering.html
 */
class MarkovClusterer(
  val maxResidual: Double,
  val pGamma: Double,
  val loopGain: Double,
  val maxZero: Double
) {

  /**
   * Default constructor.
   */
  def this() = this(0.001, 1.5, 0.0, 0.001)

  /**
   * Convenience method for using pixel arrays with the Markov clustering algorithm in JavaML.
   * @param dataset
   */
  def cluster(dataset: Array[Array[Double]], topics: List[Path]) : Array[Array[Path]] = {
    val matrix = new MarkovClustering().run(new SparseMatrix(dataset), maxResidual, pGamma, loopGain, maxZero)
    //println(dataset.size)
    //dataset(0).foreach( o => println(o) )
    println(matrix)

    // convert matrix to output dataset
    val sparseMatrixSize = matrix.getSize
    // find number of attractors (non-zero values) in diagonal
    var attractors = 0
    (0 to (sparseMatrixSize(0)-1)).foreach { i =>
      var value = matrix.get(i, i)
      if(value != 0) attractors += 1
    }
    // create cluster for each attractor with value close to 1
    (0 to sparseMatrixSize(0) - 1).map { i => {
      //println(i)
      val column = matrix.getColum(i).values().toArray()
      
      //println(matrix.getColum(i))
      val clusters = column.foldLeft(topics.length - 1, List[Path]())((b, a) => {
        println(a,b)
        if(matrix.get(i, b._1) > 0)
          (b._1 - 1, b._2 :+ topics(b._1))
        else
          (b._1 - 1, b._2)
      })
      
      clusters._2.toArray
    }}.toArray
    
    /*var finalClusters = new scala.collection.mutable.ListBuffer[Array[Path]]()

    
    val res = (0 to (sparseMatrixSize(0)-1)).map { i =>
      println(i) 
      var cluster = new scala.collection.mutable.ListBuffer[Path]()
      val value = matrix.getColum(i)
      
      println(value.values.toArray().deep)
      
      value.values()
      .toArray().map { x => {
        if (x.asInstanceOf[Double] > 0) {
          //println(value)
          (0 to (sparseMatrixSize(0)-1)).foreach { j =>
            if(matrix.get(i, j) > 0) {
              cluster.append(topics(j))
              println(cluster.toArray.deep)
            }
          }
          
          if(cluster.length > 0)
            cluster.toArray[Path]
            //finalClusters.append(cluster.toArray[Path])
        }
      }.asInstanceOf[Path]
      }
    }
    
    res.toArray[Array[Path]]

    //finalClusters.toArray*/
  }
}