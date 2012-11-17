package npbayes.distributions

import scala.collection.mutable.HashMap
import scala.util.Random
import scala.collection.mutable.WeakHashMap
import npbayes.utils.Utils
import org.apache.commons.math3.special.Gamma
import scala.collection.mutable.LinkedList

import npbayes.wordseg._

abstract class HEURISTIC
case object EXACT extends HEURISTIC
case object MINPATH extends HEURISTIC
case object MAXPATH extends HEURISTIC

object CRP {
  	def seatAt(seating: HashMap[Int,Int],tableSize: Int) = {
	  if (tableSize==0)
	    seating(1)=seating.getOrElse(1,0)+1
	  else {
		  val oneLessTS = seating(tableSize)-1
		  if (oneLessTS==0)
			seating.remove(tableSize)
		  else
		    seating(tableSize)=oneLessTS
		  seating(tableSize+1)=seating.getOrElse(tableSize+1,0)+1
	  }
	}
	
  	
  	/**
  	 * returns true if a table gets empty
  	 */
	def removeFrom(seating: HashMap[Int,Int],tableSize: Int): Boolean = {
	  val oneLessTS = seating(tableSize)-1
	  if (oneLessTS==0) {
	    seating.remove(tableSize)
	  }
	  else {
	    seating(tableSize)=oneLessTS
	  }
	  if (tableSize>1) {
	    seating(tableSize-1)=seating.getOrElse(tableSize-1,0)+1
	    false
	  } else
		true
	}
}

/**
 * this 
 * @author bborschi
 *
 */
class TypeCount {
  var nCust = 0
  var nTables = 0
  val nCust_nTables: HashMap[Int,Int] = new HashMap
  
  def isEmpty: Boolean =
    nCust==0 && nTables==0
  
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
        if (r<current+(tableSize-discount)*nTables) {        
          val n1 = tableSize+1 //one more table with that many customers
          if (nTables-1==0) //no more tables of this size
            nCust_nTables.remove(tableSize)
          else
            nCust_nTables(tableSize)-=1
          nCust_nTables(n1) = nCust_nTables.getOrElse(n1, 0)+1 
        }
        else
          inner(tables,current+(tableSize-discount)*nTables)
      }
	inner(nCust_nTables.iterator,0)
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
      true
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
/*    for (w: T <- hmTables.keySet)
      for ((nC: Int,nT: Int) <- hmTables(w))
        res += ((Gamma.logGamma(nC-discount)-Gamma.logGamma(1-discount)))*nT*/
    
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
//      (_oCount(obs)) / (_oCount+concentration)
      
  def _pSitAtNew(obs: T) =
    (concentration+discount*_tCount)*base(obs) / (_oCount+concentration)

  
  
    
  /**
   * sit at any table with probability proportional to size-discount
   * ==> sit at any of the n tables with m customers with prob prop to (m-discount)*n
   * ==> denominator is _oCount(obs)-_tCount(obs)*discount 
   * 
   */
/*  def __seatAtOld(obs: T): Double = {
	def inner(seating: HashMap[Int,Int],r: Double, current: Double): Int = 
      if (seating.isEmpty)
        throw new Error("Couldn't add to "+obs+" "+hmTables)
      else {
        val (tableSize,nTables) = seating.head
//        if (sitAt<=current+tableSize*nTables-discount*nTables) {
        if (r<current+(tableSize-discount)*nTables) {        
          CRP.seatAt(hmTables(obs),tableSize)
          tableSize
        }
        else
//          inner(seating.tail,sitAt,current+tableSize*nTables-discount*nTables)
          inner(seating.tail,r,current+(tableSize-discount)*nTables)
      }
	val nCust=inner(hmTables(obs),_random.nextDouble*(_oCount(obs)-_tCount(obs)*discount),0)
    Utils.incr(hmObsCounts, obs)
    val res = (nCust-discount)/(_oCount+concentration) 
    _oCount += 1
    res
  }
  
  def __seatAtNew(obs: T): Double = {
    Utils.incr(hmObsCounts, obs)
    Utils.incr(hmTableCounts,obs)
    CRP.seatAt(hmTables.getOrElseUpdate(obs, new HashMap()),0)
    val res = _pSitAtNew(obs)
    base.update(obs)
    _tCount += 1
    _oCount += 1
    res
  }*/
  
  
  def update (obs: T): Double = {
    assumption match  {
     case EXACT =>   { 
      val oldT = _pSitAtOld(obs)
      val newT = _pSitAtNew(obs)
      val p=_random.nextDouble*(oldT+newT)
      _oCount+=1
	  if (p < oldT) {
	    labelTabels(obs).sitAtOld(p*labelTabels(obs).nCust, discount)
	    oldT
//		_seatAtOld(obs)
	  } else {
	    labelTabels.getOrElseUpdate(obs, new TypeCount).sitAtNew
	    base.update(obs)
	    _tCount+=1
	    newT
		//_seatAtNew(obs)
	  }
     }
     case MINPATH => if (_pSitAtOld(obs)==0) {
       labelTabels(obs).sitAtNew
       _pSitAtNew(obs)}
    			//		_seatAtNew(obs)
    				else {
    				  labelTabels(obs).sitAtOld(0, discount)
    				  _pSitAtOld(obs)}
    					//_seatAtOld(obs)
     case MAXPATH => 
       labelTabels(obs).sitAtNew
       _pSitAtNew(obs) //_seatAtNew(obs)
    } 
  }
  
  def predProb(obs: T) = {
	  _pSitAtOld(obs)+_pSitAtNew(obs)    
  }
	
  def remove (obs: T) = {
    val counts = labelTabels(obs)
    _oCount-=1
    if (labelTabels(obs).remove(_random.nextInt(counts.nCust))==0) {
      base.remove(obs)
      _tCount-=1
      if (counts.isEmpty)
        labelTabels.remove(obs)
      0
    } else {
      0
    }
  }
 /*   
  /**
   * remove from a table with probability proportional to its size
   * 
   *   ==> remove from any of the n tables with m customers with probability proportional to n*m
   *   ==> denominator is total number of customers of type obs
   */
  def remove (obs: T): Double = {
	def inner(seating: HashMap[Int,Int],r: Double,current: Int=0): Unit = 
      if (seating.isEmpty)
        throw new Error("Couldn't remove from "+obs)
      else {
        val (tableSize,nTables) = seating.head
        if (r<current+tableSize*nTables) {
          if (CRP.removeFrom(hmTables(obs),tableSize)) {
            _tCount -=1
            val nObsTables = hmTableCounts(obs)-1
            if (nObsTables==0)
              hmTableCounts.remove(obs)
            else
              hmTableCounts(obs)=nObsTables
            base.remove(obs)
          }
          if (hmTables(obs).isEmpty)
            hmTables.remove(obs)
        }
        else
          inner(seating.tail,r,current+tableSize*nTables)
      }
	def _inner(seating: HashMap[Int,Int],r: Double): Unit = 
      if (seating.isEmpty)
        throw new Error("Couldn't remove from "+obs)
      else {
        val (tableSize,nTables) = seating.head
        if (r-tableSize*nTables<=0) {
          if (CRP.removeFrom(hmTables(obs),tableSize)) {
            _tCount -=1
            val nObsTables = hmTableCounts(obs)-1
            if (nObsTables==0)
              hmTableCounts.remove(obs)
            else
              hmTableCounts(obs)=nObsTables
            base.remove(obs)
          }
          if (hmTables(obs).isEmpty)
            hmTables.remove(obs)
        }
        else
          _inner(seating.tail,r-tableSize*nTables)
      }
//    inner(hmTables(obs),_random.nextInt(_oCount(obs)))
	inner(hmTables(obs),_random.nextDouble*_oCount(obs))
    Utils.decr(hmObsCounts,obs)
    _oCount-=1
    predProb(obs)
  }
*/  
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