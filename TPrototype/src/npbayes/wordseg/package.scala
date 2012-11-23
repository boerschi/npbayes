package npbayes
package object wordseg {
	val DEBUG = false
	
	/**
	 * Sharon Goldwater's annealing scheme
	 */
	def annealTemperature(startTemp: Double, annealIters: Int, stopTemp: Double = 1) = {
	  def inner(iteration: Int) =
	    if (iteration >= annealIters)
	    	 stopTemp
	    else {
	    	val bin: Int = (9*iteration)/annealIters+1
      		(10.0/bin-1)*(startTemp-stopTemp)/9.0 + stopTemp
	    }
      inner _
	}
}