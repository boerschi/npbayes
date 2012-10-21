package npbayes.wordseg.samplers

import npbayes.distributions._

object Gibbs extends App {
	val pypUni = new CRP[Word](1.0,0.0,new Monkey(50,0.5))
}