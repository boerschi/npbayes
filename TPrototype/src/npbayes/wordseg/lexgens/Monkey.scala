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
class Monkey[T<:WordType](val nPhones: Int, val pStop: Double,val UB: T=null, val pUB: Double=0.5) extends PosteriorPredictive[T] {
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
    if (UB==null)
      math.pow(_pPhon*(1-pStop),obs.size)*_norm
    else
      if (obs==UB)
        pUB
      else
        (1-pUB)*math.pow(_pPhon*(1-pStop),obs.size)*_norm
}
