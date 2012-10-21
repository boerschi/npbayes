package npbayes.distributions

import scala.collection.mutable.HashMap
import scala.util.Random
import scala.collection.mutable.WeakHashMap
import npbayes.Utils

trait Observation  {
  def label: String
}

class Word(val label: String) extends Observation {
  override def toString = label
  def size = label.length
}

object Word {
  val symbolTable = npbayes.wordseg.data.SymbolTableString
  implicit def string2Word(x: String) = Word(x)
  
  def constructWord(label: Vector[Short]): Word =
    new Word(label.map((x: Short) => symbolTable(x)).mkString)
  
  
  val _Scache: WeakHashMap[String,Word] = new WeakHashMap
  val _Vcache: WeakHashMap[Vector[Short],Word] = new WeakHashMap
  def apply(label: String): Word = _Scache.getOrElseUpdate(label, new Word(label))
  def apply(label: Vector[Short]): Word = _Vcache.getOrElseUpdate(label, constructWord(label))
}

trait PosteriorPredictive[T<: Observation] {
  def apply(obs: T) = predProb(obs: T)
  def predProb (obs: T): Double
  def update (obs: T): Double
  def remove (obs: T): Double
}


class Monkey[T<: Word](val nPhones: Int, val pStop: Double) extends PosteriorPredictive[T] {
  val _pPhon: Double = 1.0/nPhones
  val _norm = pStop/(1-pStop)
  def remove(obs: T) = predProb(obs)
  def update(obs: T) = predProb(obs)
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
    
  def _addCustomer (obs: T): Double = {
    if (_random.nextDouble < _pSitAtOld(obs)) {
      println("at old")
      _seatAtOld(obs)
    } else {
      println("at new")
      _seatAtNew(obs)
    }
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