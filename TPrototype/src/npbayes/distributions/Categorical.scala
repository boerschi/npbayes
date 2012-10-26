package npbayes.distributions

/**
 * a categorical distribution that is easy to sample from
 * 
 * add outcomes using add, you can pass unnormalized probabilities
 * 
 * use sample to produce an outcome of type T
 */
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
    inner(outcomes,0,Categorical.unif.nextDouble*partition)
  }
}

object Categorical {
  val unif = new scala.util.Random()
}
