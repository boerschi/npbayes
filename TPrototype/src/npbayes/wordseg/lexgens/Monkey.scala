package npbayes.wordseg.lexgens

import npbayes.distributions.PosteriorPredictive
import npbayes.wordseg.data.Word

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
