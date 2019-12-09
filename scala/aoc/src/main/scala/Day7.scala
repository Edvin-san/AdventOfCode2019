object Day7 {
	import IntCodeComputer._
	import IntCodeComputationCoordinator._

	def runPart1(programString: String): BigInt = run((0 to 4).toSet)(programString, false)
	def runPart2(programString: String): BigInt = run((5 to 9).toSet)(programString, true)

	def run(phaseSettings: Set[Int])(programString: String, feedbackLoop: Boolean): BigInt = {
		val numAmplifiers = phaseSettings.size
		val program = programString.split(",").map(_.toInt).toArray
		val amplifiers = phaseSettings.toSeq.zip(1 to 5).map(t => new IntCodeComputer(t._2.toString, program))
		val combinedComputation = setupAmplifierComputation(amplifiers, feedbackLoop)
		def thrusterInput(phaseSettings: Seq[Int]): Option[BigInt] = amplifiersOutput(combinedComputation, amplifiers)(0)(phaseSettings).headOption

		phaseSettings.toSeq.permutations.map(thrusterInput).flatten.max
	}

	def runPart2WithPhase(programString: String, phaseSettings: Seq[Int]): List[BigInt] = {
		val program = programString.split(",").map(_.toInt).toArray
		val amplifiers = phaseSettings.zip(1 to 5).map(t => new IntCodeComputer(t._2.toString, program))
		val combinedComputation = setupAmplifierComputation(amplifiers, true)
		amplifiersOutput(combinedComputation, amplifiers)(0)(phaseSettings)
	}

	def setupAmplifierComputation(amplifiers: Seq[IntCodeComputer], feedbackLoop: Boolean): IntCodeCombinedComputation = {
		val pipeConfig = if (feedbackLoop) createFeedbackLoopPipe(amplifiers) else createSerialPipe(amplifiers)
		new IntCodeCombinedComputation("combined", amplifiers.head, pipeConfig)
	}

	def amplifiersOutput(computation: IntCodeCombinedComputation, amplifiers: Seq[IntCodeComputer])(initialInput: Int)(phaseSettings: Seq[Int]): List[BigInt] = {
		computation.reset()
		for ((amplifier, phaseSetting) <- amplifiers zip phaseSettings) amplifier.addInputs(List(phaseSetting))

		computation.addInputs(List(initialInput))
		computation.run()
		computation.output
	}
}