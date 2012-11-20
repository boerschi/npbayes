package npbayes.distributions

import scala.collection.mutable.HashMap
import scala.util.Random
import scala.collection.mutable.WeakHashMap
import npbayes.utils.Utils
import org.apache.commons.math3.special.Gamma
import scala.collection.mutable.LinkedList
import scala.collection.JavaConversions._
import java.util.TreeMap

import npbayes.wordseg._

abstract class HEURISTIC
case object EXACT extends HEURISTIC
case object MINPATH extends HEURISTIC
case object MAXPATH extends HEURISTIC

/**
 * this follows closely Sharons's/Mark's implementation
 * I found this way of bookkeeping more efficient than maintaining
 * several independent Maps (obs->counts, obs->tableCounts, obs->tables)
 * 
 * not sure whether the java-TreeMap is really necessary and whether the
 * internal conversions lead to huge performance losses
 * @author bborschi
 *
 */
class TypeCount {
  var nCust = 0
  var nTables = 0
  val nCust_nTables: HashMap[Int,Int] = new HashMap
  //val nCust_nTables: TreeMap[Int,Int] = new TreeMap
  
  def isEmpty: Boolean =
    nCust==0 && nTables==0
  
  def sanity: Boolean = {
    var nn = 0
    var mm = 0
    for ((nC,nT) <- nCust_nTables) {
      nn += nC*nT
      mm += nT
    }
    assert(nn==nCust)
    assert(mm==nTables)
    nn==nCust && mm==nTables
  }
    
  def sitAtNew = {
    nCust+=1
    nTables+=1
    nCust_nTables(1) = nCust_nTables.getOrElse(1,0)+1 
  }
  
  def sitAtOld(r: Double, discount: Double): Unit = {
	def inner(tables: Iterator[(Int,Int)],current: Double): Unit = 
      if (tables.isEmpty)
        throw new Error("Couldn't add to table")
      else {
        val (tableSize: Int,nTables: Int) = tables.next
        if (current-(tableSize-discount)*nTables<=0) {        
          val n1 = tableSize+1 //one more table with that many customers
          if (nTables-1==0) //no more tables of this size
            nCust_nTables.remove(tableSize)
          else
            nCust_nTables(tableSize)-=1
          nCust_nTables(n1) = nCust_nTables.getOrElse(n1, 0)+1 
        }
        else
          inner(tables,current-(tableSize-discount)*nTables)
      }
	inner(nCust_nTables.iterator,r)
    nCust+=1
  }
  
  def remove(r: Int): Int = {
	def inner(tables: Iterator[(Int,Int)],current: Int=0): Int = 
      if (tables.isEmpty)
        throw new Error("Couldn't remove")
      else {
        val (tableSize,nTs) = tables.next
        if (r<current+tableSize*nTs) {
          val n1 = tableSize-1 //one more table of this size
          if (nTs-1 == 0)
            nCust_nTables.remove(tableSize)
          else
            nCust_nTables(tableSize)-=1
          if (n1==0) //one less table
            nTables-=1
          else
            nCust_nTables(n1) = nCust_nTables.getOrElse(n1,0)+1
          nCust-=1
          n1
        } else
          inner(tables, current+tableSize*nTs)
      }
	inner(nCust_nTables.iterator,0)
  }
}

class CRP[T](var concentration: Double, var discount: Double, val base: PosteriorPredictive[T], val assumption: HEURISTIC=EXACT) extends PosteriorPredictive[T] {
  val _random = new Random()
  
/*  val hmObsCounts: HashMap[T,Int] = new HashMap() //overall-count
  val hmTableCounts: HashMap[T,Int] = new HashMap() //maps each observation to the number of tables
  val hmTables: HashMap[T,HashMap[Int,Int]] = new HashMap() //Goldwater-Style representation
*/
  val labelTabels: HashMap[T,TypeCount] = new HashMap
  val emptyCount = new TypeCount
  
  def _oCount(o: T): Int = labelTabels.getOrElse(o, emptyCount).nCust//hmObsCounts.getOrElse(o, 0)
  var _oCount = 0
  def _tCount(o: T): Int = labelTabels.getOrElse(o, emptyCount).nTables//hmTableCounts.getOrElse(o, 0)
  
  
  var _tCount = 0
  
  /**
   * predictive probability, taking into account the additional
   * observation prev
   */
  def apply(obs: T,prevs: List[T]) = {
    for (prev <- prevs)
      update(prev)
    val res=predProb(obs)
    for (prev <- prevs)
      remove(prev)
    res
  }
  
    def sanityCheck: Boolean = {
/*  	  _oCount==hmObsCounts.values.foldRight(0)(_+_) &&
  	  _tCount==hmTableCounts.values.foldRight(0)(_+_) &&
  	  {for ((obs,c) <- hmObsCounts.toList)
  	     yield c==hmTables(obs).map(x=>x._1*x._2).sum && 
  	     hmTableCounts(obs)==hmTables(obs).values.sum}.foldLeft(true)(_&&_)*/
      var nn=0
      var tt=0
      for ((word,counts) <- labelTabels.toList) {
        assert(counts.sanity)
        nn+=counts.nCust
        tt+=counts.nTables
      }
      assert(nn==_oCount)
      assert(tt==_tCount)
      nn==_oCount && tt==_tCount
  	}
  
  def isEmpty: Boolean = _oCount==0
  
  /**
   * full logProb, including base-distribution
   */
  override def logProb = {
    logProbSeating + base.logProb
  }
  
  def logProbSeating =
    _logProbSeating(concentration,discount)

  /**
   * just the seating-arrangement
   */
  def _logProbSeating(concentration: Double, discount: Double): Double = {
    //cf e.g. Goldwate et al., 2011, p.2342 (1-Param,discount=0) and p.2345 (2-Param)
    var res = Gamma.logGamma(concentration)-Gamma.logGamma(_oCount+concentration)
    for (tokenCount <- labelTabels.values)
      for ((nC,nT) <- tokenCount.nCust_nTables)
    	  res += ((Gamma.logGamma(nC-discount)-Gamma.logGamma(1-discount)))*nT
    if (discount==0)
      res += _tCount*math.log(concentration)
    else
      res += (_tCount*math.log(discount)+Gamma.logGamma(concentration/discount+_tCount)-
    		  Gamma.logGamma(concentration/discount))
    res
  }
  
  def _pSitAtOld(obs: T) =
    if (_oCount==0)
      0
    else
      (_oCount(obs)-discount*_tCount(obs)) / (_oCount+concentration)
      
  def _pSitAtNew(obs: T) =
    (concentration+discount*_tCount)*base(obs) / (_oCount+concentration)

  
  def update (obs: T): Double = {
    assumption match  {
     case EXACT =>   
      val oldT = _oCount(obs)-discount*_tCount(obs)
      val newT = (concentration+discount*_tCount)*base(obs)
      val p = _random.nextDouble*(oldT+newT)
      _oCount+=1
	  if (p < oldT) {
	    labelTabels(obs).sitAtOld(p,discount)
	    oldT/(_oCount-1+concentration)
	  } else {
	    labelTabels.getOrElseUpdate(obs, new TypeCount).sitAtNew
	    base.update(obs)
	    _tCount+=1
	    newT
	  }
     case MINPATH => 
       if (_pSitAtOld(obs)==0) { 
         labelTabels(obs).sitAtNew
         _pSitAtNew(obs)
       } else {
    	 labelTabels(obs).sitAtOld(0, discount)
    	 _pSitAtOld(obs)
       }
     case MAXPATH => 
       labelTabels(obs).sitAtNew
       _pSitAtNew(obs)
    } 
  }
  
  def predProb(obs: T) = {
	  _pSitAtOld(obs)+_pSitAtNew(obs)    
  }
	
  def remove (obs: T) = {
    val counts = labelTabels(obs)
    _oCount-=1
    val r = (_random.nextDouble*counts.nCust).toInt
    if (labelTabels(obs).remove(r)==0) {
      base.remove(obs)
      _tCount-=1
      if (counts.isEmpty)
        labelTabels.remove(obs)
      0
    } else {
      0
    }
  }

  def _logProbSeatingByConc: (Double => Double) = 
    _logProbSeating(_: Double, discount)
  
  def _logProbSeatingByDisc: (Double => Double) =
    _logProbSeating(concentration, _: Double)
    
  
  /**
   * tells you what the normalized probabilites of sitting down are
   */
  def __seatChoice(obs: T) = {
    val pOld = _pSitAtOld(obs)
    val pNew = _pSitAtNew(obs)
    val p = pOld + pNew
    List(("old:",pOld/p),("new:",pNew/p))
  }
  
  }