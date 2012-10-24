package npbayes.distributions

import scala.collection.mutable.HashMap
import scala.util.Random
import scala.collection.mutable.WeakHashMap
import npbayes.Utils
import org.apache.commons.math3.special.Gamma
import scala.collection.mutable.LinkedList


class Categorical[T] {
  var outcomes: List[(T,Double)] = List.empty
  var partition: Double = 0
  
  def add(event: T, prob: Double): Unit = {
    outcomes = outcomes:+((event,prob))
    partition += prob
  }
  
  override def toString = {
    var res=""
    for ((out,prob) <- outcomes)
      res+=out+" "+prob/partition+"\n"
    res
  }
  
  def sample: T = {
    def inner(events: List[(T,Double)],cur: Double,flip: Double): T = events match {
      case List() => throw new Error("Categorical.sample: couldn't produce sample")
      case (res,prob)::tail => {
        if (flip<=cur+prob)
          res
        else
          inner(tail,cur+prob,flip)
      }
    }
    inner(outcomes,0,distributions.RANDOM.nextDouble*partition)
  }
}

abstract class HEURISTIC
case object EXACT extends HEURISTIC
case object MINPATH extends HEURISTIC
case object MAXPATH extends HEURISTIC


object distributions {
  val ASSUMPTION: HEURISTIC = EXACT
  val RANDOM: Random = new Random()
}

trait Observation  {
  def label: String
}

class Word(val label: String, val phones: Vector[Short]) extends Observation {
  def + (that: Word) =
    Word(this.phones.++(that.phones))
  override def toString = label
  def size = phones.size
  
  def equals(obj: Word) = 
      phones==obj.phones
  
  override def hashCode = phones.hashCode
}

object Word {
  val symbolTable = npbayes.wordseg.data.SymbolTableString
  implicit def getWordOrConstruct(label: String) =  {
    var res = _Scache.getOrElse(label,null)
    if (res==null) {
      res = new Word(label,Vector.empty.++(label.split(" ").map((x: String)=>symbolTable(x))))
      _Scache(label)=res
      _Vcache(res.phones)=res
    }
    res
  }
  
  implicit def getWordOrConstruct(label: Vector[Short]): Word = {
    var res = _Vcache.getOrElse(label,null)
    if (res==null) {
    	res = new Word(label.map((x: Short) => symbolTable(x)).mkString(" "), label)
    	_Vcache(label) = res
    	_Scache(res.label) = res
    }
    res
  }
  
  
  val _Scache: WeakHashMap[String,Word] = new WeakHashMap
  val _Vcache: WeakHashMap[Vector[Short],Word] = new WeakHashMap
  def apply(label: Vector[Short]): Word = {
    val res = _Vcache.getOrElseUpdate(label, getWordOrConstruct(label))
    _Scache(res.label) = res
    res
  }
  def apply(label: String): Word = {
    val res =_Scache.getOrElseUpdate(label, getWordOrConstruct(label))
    _Vcache(res.phones)=res
    res
  }
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
//    println("Base-Remove: "+obs)
    val res = predProb(obs)
    _logProb -= math.log(res)
    res
  }
  def update(obs: T) = {
//    println("Base-Update: "+obs)
    val res = predProb(obs)
    _logProb += math.log(res)
    res
  }
  def predProb(obs: T) = 
    math.pow(_pPhon*(1-pStop),obs.size)*_norm
}


class CRP[T<: Observation](val concentration: Double, val discount: Double, val base: PosteriorPredictive[T], val assumption: HEURISTIC=EXACT) extends PosteriorPredictive[T] {
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
    if (_oCount==0) {
      base(obs)
    } else {
      (concentration+discount*_tCount)*base(obs) / (_oCount+concentration)
    }
  
  
  def _seatAtOld(obs: T): Double = {
    val sitAt = _random.nextDouble()*(_oCount(obs)-discount*_tCount(obs))
    var current: Double = 0
    var i = 0
    var nCust = 0
//    println("sit at old ("+_pSitAtOld(obs)/(_pSitAtOld(obs)+_pSitAtNew(obs))+" vs "+_pSitAtNew(obs)/(_pSitAtOld(obs)+_pSitAtNew(obs)))
//    println("Prob-SitAt:"+sitAt)
    _oCount += 1
    while (true) {
      nCust = hmTables(obs)(i)
      current += (nCust-discount)
      //println(current)
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
//	println("sit at new ("+_pSitAtNew(obs)/(_pSitAtOld(obs)+_pSitAtNew(obs))+" vs "+_pSitAtOld(obs)/(_pSitAtOld(obs)+_pSitAtNew(obs)))
    Utils.incr(hmObsCounts, obs)
    Utils.incr(hmTableCounts,obs)
    hmTables(obs) = hmTables.getOrElse(obs, Vector.empty):+1
    val res = _pSitAtNew(obs)
    base.update(obs)
    _oCount += 1
    _tCount += 1
    res
  }
    
  def _addCustomer (obs: T): Double = assumption match  {
    case EXACT =>   { 
      val oldT = _pSitAtOld(obs)
      val newT = _pSitAtNew(obs)
      val p=_random.nextDouble*(oldT+newT)
//      println(p)      
	  if (p <= oldT) {
		_seatAtOld(obs)
	  } else {
		_seatAtNew(obs)
	  }
    }
    case MINPATH => if (_pSitAtOld(obs)==0)	
    					_seatAtNew(obs)
    				else
    					_seatAtOld(obs)
    case MAXPATH => _seatAtNew(obs)
  } 
  
  def predProb(obs: T) = {
//	  println("get prob for "+obs+": "+(_pSitAtOld(obs)+_pSitAtNew(obs)).toString)
	  _pSitAtOld(obs)+_pSitAtNew(obs)    
  }
	
    
  def update (obs: T) =
  	_addCustomer(obs)    
  
  def remove (obs: T): Double = {
    var current=0
    var i=0
    var nCust=0
//    println("Remove-Element:"+obs)
    val removeFrom = _random.nextInt(_oCount(obs))
//    println("Remove from:"+removeFrom)
    while (true) {
      nCust = hmTables(obs)(i)
      current += nCust
      //println(current)
      if (removeFrom<=current) {
        Utils.decr(hmObsCounts,obs)
        _oCount-=1
        if (nCust-1==0) {
          _tCount-=1
          Utils.decr(hmTableCounts,obs)
          base.remove(obs)
          if (_oCount(obs)==0)
            hmTables.remove(obs)
          else
            hmTables(obs)=hmTables(obs).take(i)++hmTables(obs).drop(i+1)
        } else {
          hmTables(obs)=hmTables(obs).updated(i, nCust-1)
        }
        return predProb(obs) //overall probability of adding back in
      }
      i+=1
    }
    throw new Error("remove "+obs)  
  }
}