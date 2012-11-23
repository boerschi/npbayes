package npbayes.wordseg.models

import java.io.PrintStream
import npbayes.wordseg.data.VarData

abstract class WordsegModel {
	val data: VarData
	def sanity: Boolean
	def init(gold:Boolean = false)
	def logProb: Double
	def gibbsSweep(anneal: Double=1.0): Double
	def evaluate: String
	/**
	 * only resample words, but determine drops
	 */
	def gibbsSweepWords(anneal: Double=1.0): Double
	
	def writeAnalysis(s: PrintStream) =
	  data.printAnalysis(s)
}