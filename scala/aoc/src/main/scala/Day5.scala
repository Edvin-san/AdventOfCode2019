object Day5 {
	import IntCodeComputer._

	def run(programString: String, input: Int): String = {
		val program = programString.split(",").map(_.toInt).toArray
		val computer = new IntCodeComputer(program)
		computer.addInput(input)
		computer.run
		var diagnostics = computer.output
		diagnostics match {
			case Nil => "NO OUTPUT"
			case d => d.last.toString
		}
	}

}