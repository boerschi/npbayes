package npbayes.distributions

import scala.collection.mutable.HashMap
import scala.util.Random
import scala.collection.mutable.WeakHashMap
import npbayes.Utils
import org.apache.commons.math3.special.Gamma


abstract class HEURISTIC
case object EXACT extends HEURISTIC
case object MINPATH extends HEURISTIC
case object MAXPATH extends HEURISTIC


object distributions {
  val ASSUMPTION: HEURISTIC = EXACT
}

trait Observation  {
  def label: String
}

class Word(val label: String, val phones: Vector[Short]) extends Observation {
  def + (that: Word) =
    Word(this.phones.++(that.phones))
  override def toString = label
  def size = label.length
}

object Word {
  val symbolTable = npbayes.wordseg.data.SymbolTableString
  implicit def string2Word(x: String) = 
    new Word(x,Vector.empty.++(x.split(" ").map((x: String)=>symbolTable(x))))
  
  def constructWord(label: Vector[Short]): Word =
    new Word(label.map((x: Short) => symbolTable(x)).mkString, label)
  
  
  val _Scache: WeakHashMap[String,Word] = new WeakHashMap
  val _Vcache: WeakHashMap[Vector[Short],Word] = new WeakHashMap
  def apply(label: Vector[Short]): Word = _Vcache.getOrElseUpdate(label, constructWord(label))
}

trait PosteriorPredictive[T<: Observation] {
  def logProb: Double = 0
  def apply(obs: T) = predProb(obs: T)
  def predProb (obs: T): Double
  def update (obs: T): Double
  def remove (obs: T): Double
}


class Monkey[T<: Word](val nPhones: Int, val pStop: Double) extends PosteriorPredictive[T] {
  var _logProb: Double = 0
  val _pPhon: Double = 1.0/nPhones
  val _norm = pStop/(1-pStop)
  override def logProb = _logProb
  def remove(obs: T) = {
    val res = predProb(obs)
    _logProb -= math.log(res)
    res
  }
  def update(obs: T) = {
    val res = predProb(obs)
    _logProb += math.log(res)
    res
  }
  def predProb(obs: T) = 
    math.pow(_pPhon*(1-pStop),obs.size)*_norm
}


class CRP[T<: Observation](val concentration: Double, val discount: Double, val base: PosteriorPredictive[T]) extends PosteriorPredictive[T] {
  val _random = new Random()
  val hmObsCounts: HashMap[T,Int] = new HashMap() //overall-count
  val hmTableCounts: HashMap[T,Int] = new HashMap() //maps each observation to the number of tables
  val hmTables: HashMap[T,Vector[Int]] = new HashMap() //maps each label to a list of tables with this label

  def _oCount(o: T): Int = hmObsCounts.getOrElse(o, 0)
  var _oCount = 0
  def _tCount(o: T): Int = hmTableCounts.getOrElse(o, 0)
  var _tCount = 0
  
  override def logProb = {
    //cf e.g. Goldwate et al., 2011, p.2342 (1-Param,discount=0) and p.2345 (2-Param)
    var res = Gamma.logGamma(1+concentration)-Gamma.logGamma(_oCount+concentration)
    for (w: T <- hmTables.keySet)
      for (n_k <- hmTables(w))
        res += (Gamma.logGamma(n_k-discount)-Gamma.logGamma(1-discount))
    if (discount==0)
      res += (_tCount-1)*math.log(concentration)
    else
      res += (_tCount*math.log(discount)+Gamma.logGamma(concentration/discount+_tCount)-
    		  Gamma.logGamma(concentration/discount))
    res + base.logProb
  }
  
  def _pSitAtOld(obs: T) =
    if (_oCount==0)
      0
    else
      (_oCount(obs)-discount*_tCount(obs)) / (_oCount+concentration)
      
  def _pSitAtNew(obs: T) =
    if (_oCount==0)
      base(obs)
    else
      (concentration+discount*_tCount)*base(obs) / (_oCount+concentration)
  
  
  def _seatAtOld(obs: T): Double = {
    val sitAt = _random.nextDouble()*(_oCount(obs)-discount*_tCount(obs))
    var current: Double = 0
    var i = 0
    var nCust = 0
    println(sitAt)
    _oCount += 1
    while (true) {
      nCust = hmTables(obs)(i)
      current += (nCust-discount)
      println(current)
      if (sitAt<=current) {
        Utils.incr(hmObsCounts, obs)
        hmTables(obs)=hmTables(obs).updated(i, nCust+1)
        return (nCust-discount)/(_oCount-1+concentration)
      }
      i+=1
    }
    throw new Error("_seatAtOld("+obs+")")
  }
  
  def _seatAtNew(obs: T): Double = {
    Utils.incr(hmObsCounts, obs)
    Utils.incr(hmTableCounts,obs)
    hmTables(obs) = hmTables.getOrElse(obs, Vector.empty):+1
    val res = _pSitAtNew(obs)
    _oCount += 1
    _tCount += 1
    res
  }
    
  def _addCustomer (obs: T): Double = distributions.ASSUMPTION match  {
    case EXACT =>   if (_random.nextDouble < _pSitAtOld(obs)) {
    					_seatAtOld(obs)
    				} else {
    					_seatAtNew(obs)
    				}
    case MINPATH => if (_pSitAtOld(obs)==0)	
    					_seatAtNew(obs)
    				else
    					_seatAtOld(obs)
    case MAXPATH => _seatAtNew(obs)
  } 
  
  def predProb(obs: T) =
	_pSitAtOld(obs)+_pSitAtNew(obs)
    
  def update (obs: T) =
  	_addCustomer(obs)    
  
  def remove (obs: T): Double = {
    var current=0
    var i=0
    var nCust=0
    val removeFrom = _random.nextInt(_oCount(obs))
    println(removeFrom)
    while (true) {
      nCust = hmTables(obs)(i)
      current += nCust
      println(current)
      if (removeFrom<=current) {
        Utils.decr(hmObsCounts,obs)
        _oCount-=1
        if (nCust-1==0) {
          _tCount-=1
          Utils.decr(hmTableCounts,obs)
          base.remove(obs)
        }
        return predProb(obs) //overall probability of adding back in
      }
      i+=1
    }
    throw new Error("remove "+obs)  
  }
}