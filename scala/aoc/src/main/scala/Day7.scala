object Day7 {
	import IntCodeComputer._

	def runPart1(programString: String): Int = run((0 to 4).toSet)(programString, false)
	//def runPart2(programString: String): Int = run((5 to 9).toSet)

	def run(phaseSettings: Set[Int])(programString: String, feedbackLoop: Boolean): Int = {
		val numAmplifiers = phaseSettings.size
		val program = programString.split(",").map(_.toInt).toArray
		val amplifiers = phaseSettings.toSeq.map(_ => new IntCodeComputer(program))
		def thrusterInput(phaseSettings: Seq[Int]): Option[Int] = amplifiersOutput(List(0))(amplifiers)(phaseSettings).headOption

		phaseSettings.toSeq.permutations.map(thrusterInput).flatten.max
	}

	def feedbackLoopOutput()

	def amplifiersOutput(initialInput: List[Int])(amplifiers: Seq[IntCodeComputer])(phaseSettings: Seq[Int]): (List[Int], Seq[IntCodeComputer]) = {
		val configuredAmplifiers = amplifiers zip phaseSettings
		configuredAmplifiers.foldLeft((initialInput, Nil)) { 
			case ((inputSignals, updatedAmplifiers), (amplifier, phaseSetting)) => {
				val (output, updateAmplifier) = amplifier.run(phaseSetting :: inputSignals)
				(output, updateAmplifier :: updatedAmplifiers)
			}
		}
	}
}