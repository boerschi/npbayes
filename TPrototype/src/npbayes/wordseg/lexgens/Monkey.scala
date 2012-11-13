package npbayes.wordseg.lexgens

import npbayes.distributions.PosteriorPredictive
import npbayes.wordseg.data.WordType

/**
 * Monkey-Generator for both Unigram and Bigram model
 * the Unigram generator sets UB to zero
 * @author bborschi
 *
 * @param <T>
 */
class Monkey(val nPhones: Int, val pStop: Double,val UB: WordType=null, val pUB: Double=0.5) extends PosteriorPredictive[WordType] {
  val _pPhon: Double = 1.0/nPhones
  var _nWords = 0
  var _nPhons = 0
  var _nUBS = 0
  
  override def logProb = {
    if (UB==null) {
      _nWords*math.log(pStop*_pPhon)+(_nPhons-_nWords)*math.log((1-pStop)*_pPhon) 
    } else {
      _nWords*(math.log(pStop*_pPhon)+math.log(1-pUB))+(_nPhons-_nWords)*math.log((1-pStop)*_pPhon)+
      _nUBS*math.log(pUB)
    }
  }
  
  def remove(obs: WordType) = {
	val res = predProb(obs)
    if (UB==null) {
	  _nWords-=1
	  _nPhons-=obs.size
	} else {
	  if (obs==UB)
	    _nUBS-=1
	  else {
	    _nWords-=1
	    _nPhons-=obs.size	    
	  }
	}
	res
  }
  def update(obs: WordType) = {
    val res = predProb(obs)
    if (UB==null) {
      _nWords+=1
      _nPhons+=obs.size
    } else {
      if (obs==UB)
        _nUBS+=1
        else {
          _nWords+=1
          _nPhons+=obs.size
        }
    }
    res
  }
  def predProb(obs: WordType) = 
    if (UB==null)
      _pPhon*math.pow(_pPhon*(1-pStop),obs.size-1)*pStop
    else
      if (obs==UB)
        pUB
      else
        (1-pUB)*_pPhon*math.pow(_pPhon*(1-pStop),obs.size-1)*pStop
}
