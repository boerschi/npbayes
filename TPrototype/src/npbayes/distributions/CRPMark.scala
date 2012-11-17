package npbayes.distributions

import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.special.Gamma
import scala.collection.mutable.LinkedList

import npbayes.wordseg._



class CRPMark[T](val concentration: Double, val discount: Double, val base: PosteriorPredictive[T]) extends PosteriorPredictive[T] {
  
  class TypeBook {
    var n=0  //customers
    var m=0  //total number of tables
    val n_m: scala.collection.mutable.Map[Int,Int] = scala.collection.mutable.Map[Int,Int]()
    
    def insert_old(r: Double) = {
      def inner(iter: Iterator[(Int,Int)], remR: Double): Unit = {
        val (nCust,nTab) = iter.next()
        if ((remR - nTab*(nCust-discount))<=0) {
          val oneMore = nCust +1
          if (nTab-1 == 0)
            n_m.remove(nCust)
          else
            n_m(nCust)=nTab-1
          n_m(oneMore) = n_m.getOrElse(oneMore,0)+1
        } else {
          inner(iter,remR-nTab*(nCust-discount))
        }
      }
      inner(n_m.iterator,r)
      n+=1
    }
  
    def insert_new() = {
      n+=1
      m+=1
      n_m(1) = n_m.getOrElse(1, 0)+1
    }
    
    def empty: Boolean = {
      assert(m <= n)
      n == 0
    }
    
    def erase(r: Int) = {
      n-=1
      def inner(iter: Iterator[(Int,Int)], remR: Int): Int = {
        val (nCust,nTab) = iter.next()
        if (remR-nCust*nTab<=0) {
          val n1 = nCust-1
          if (nTab-1==0) {
            n_m.remove(nCust)
          } else {
            n_m(nCust)-=1
          }
          if (n1==0)
            m-=1
          else
            n_m(n1) = n_m.getOrElse(n1, 0)+1
          n1
        } else {
          inner(iter,remR-nCust*nTab)
        }
      }
      inner(n_m.iterator,r)
    }
  }
  
  val label_tables: scala.collection.mutable.Map[T,TypeBook] = scala.collection.mutable.Map[T,TypeBook]().withDefaultValue(new TypeBook)
  
  val _random = new MersenneTwister
  var _oCount = 0
  def _oCount(o: T): Int = label_tables(o).n
  def _tCount(o: T): Int = label_tables(o).m
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
      assert(concentration>=0)
      assert(discount<=1 && discount >=0)
      var nn=0
      var mm=0
      for (counts <- label_tables.values) {
        nn+=counts.n
        mm+=counts.m
      }
     return (_oCount==nn) && (_tCount==mm)
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
/*    for (w: T <- hmTables.keySet)
      for ((nC: Int,nT: Int) <- hmTables(w))
        res += ((Gamma.logGamma(nC-discount)-Gamma.logGamma(1-discount)))*nT */
    for (counts <- label_tables.values)
      for ((nCust,nTab) <- counts.n_m)
        res += nTab * (Gamma.logGamma(nCust-discount)-Gamma.logGamma(1-discount))
        

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

  

  def predProb(obs: T) = {
    val obsCounts = label_tables(obs)
    val p_old = (obsCounts.n-discount*obsCounts.m)/(_oCount+concentration)
    val p_new = base(obs)*(_tCount*discount+concentration)/(_oCount+concentration)
    p_old+p_new
  }
	
    

  
  def remove(obs: T) = {
    val obsCounts = label_tables(obs)
    val r: Int = (obsCounts.n * _random.nextDouble).toInt
    _oCount-=1
    if (obsCounts.erase(r)==0) {
      _tCount -= 1
      base.remove(obs)
      if (obsCounts.empty)
        label_tables.remove(obs)
    }
    _oCount
  }
  
  def update(obs: T) = {
    val obsCounts = label_tables.getOrElseUpdate(obs, new TypeBook)
    val p_old = (obsCounts.n-obsCounts.m*discount)
    val p_new = base(obs)*(_tCount*discount+concentration)
    var p = p_old+p_new
    val r: Double = p*(_random.nextDouble)
    if (r<p_old)
      obsCounts.insert_old(r)
    else {
      if (p_old>0) {
        0==0
      }
      obsCounts.insert_new()
      _tCount+=1
      base.update(obs)
    }
    p = p/(_oCount+concentration)
    _oCount += 1
    p
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