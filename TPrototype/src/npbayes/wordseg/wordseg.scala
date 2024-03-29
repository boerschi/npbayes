package npbayes.wordseg

import scala.collection.immutable.Map
import scala.io.Source
import npbayes.wordseg.models.Unigram
import npbayes.wordseg.models.Bigram
import npbayes.wordseg.models.WordsegModel
import npbayes.ArgParser
import npbayes.distributions.{EXACT,MAXPATH,MINPATH}

/**
 * vanilla-word segmentation
 * @author bborschi
 *
 */




class TaggerParams(args: Array[String]) extends ArgParser(args) {
	def NGRAM = getString("--ngram", "2")
	def ALPHA0 = getDouble("--alpha0", 100)
	def ALPHA1 = getDouble("--alpha1", 3000)
	def PSTOP = getDouble("--pstop", 0.5)
	def ITERS = getInt("--iters",1000)
	def ANNEALITERS = getInt("--annealIters",1000)
	def STARTTEMP = getDouble("--startTemp",10)
	def STOPTEMP = getDouble("--stopTemp",1)
	def DROPPROB = getDouble("--dropProb",0.0)
	def DROPSEG = getString("--dropSeg","KRLKR")
	def DROPIND = getString("--dropInd","KRLKR")
	def INPUT = getString("--input","")
	def OUTPUT = getString("--output","")
	def TRACE = getString("--trace","")
	def ASSUMPTION = getString("--assumption","EXACT")
	def GOLDINIT = getBoolean("--goldinit",false)
	def MODE = getString("--mode","WORDSEG")
	def BURNIN = getInt("--burnin",2000)
	def SAMPLES = getInt("--sampleEvery",10)
	def BOUNDINITPROB = getDouble("--binitProb",0.0)
}

object wordseg {
  
	def main(args: Array[String]) = {
	  val options = new TaggerParams(args)
	  val assumption = options.ASSUMPTION match {
	    case "EXACT" =>
	      EXACT
	    case "MINPATH" =>
	      MINPATH
	    case "MAXPATH" =>
	      MAXPATH
	  }
	  
	  val model: WordsegModel = options.NGRAM match {
	    case "1" =>
	      new Unigram(options.INPUT,options.ALPHA0,0,options.PSTOP,assumption,options.DROPSEG,options.DROPIND,options.DROPPROB)
	    case "2" =>
	      new Bigram(options.INPUT,options.ALPHA0,0,options.ALPHA1,0, options.PSTOP,assumption,options.DROPSEG,options.DROPIND,options.DROPPROB)	      
	  }
	  
	  def annealTemperature(x: Int) = 	    //npbayes.wordseg.annealTemperature(x)
		npbayes.wordseg.annealTemperature(options.STARTTEMP, options.ANNEALITERS, 1)(x)
	  
	  def sample = options.MODE match {
	    case "WORDSEG" =>
	      model.gibbsSweep(_)
	    case "LANGMODEL" =>
	      model.gibbsSweepWords(_)
	  }
	  val traceFile = new java.io.PrintStream(new java.io.File(options.OUTPUT+".trace"))
	  val sampleFile = new java.io.PrintStream(new java.io.File(options.OUTPUT+".samples"))
	  model.init(options.GOLDINIT,options.BOUNDINITPROB)
	  println(options)
	  traceFile.println(options)
	  println(0+" "+1+" "+model.logProb+" "+model._logProbTrack+" "+model.evaluate)
	  for (i <- 1 to options.ITERS) {
	    val temperature: Double = annealTemperature(i)
	    sample(1/temperature)
	    val log = i+" "+temperature+" "+model.logProb+" "+model._logProbTrack+" "+" "+model.evaluate
	    println(log); traceFile.println(log)
	    if (i>=options.BURNIN && i%options.SAMPLES==0) {
	      model.writeAnalysis(sampleFile)
	      sampleFile.println()
	    }
	  }
	  model.writeAnalysis(new java.io.PrintStream(new java.io.File(options.OUTPUT+".out")))
	  traceFile.close()
	  sampleFile.close()
	}
}