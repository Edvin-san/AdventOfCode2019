object IntCodeComputerCoordinator {
	import IntCodeComputer._

	// Runs combined computations until some computer can't output because its target has terminated (or is None).
	/*
	class IntCodeCombinedComputation(
		val first: IntCodeComputation,
		val initialInput: Map[IntCodeComputation, List[Int]],
		val pipe: Map[IntCodeComputation, Option[IntCodeComputation])
	) extends IntCodeComputation {
		var terminated = false

		override def hasTerminated = terminated

		override def run(inputs: List[Int]): List[Int] = {
			var curr = first
			var out: List[Int] = Nil

			var continue = true
			var hasBeenInitialized: Set[IntCodeComputation] = Set()
			while (continue) {
				if (!hasBeenInitialized.contains(curr)) {
					out = curr.run(initialInput(curr))
				}


			}

			terminated = true
		}
	} 
	*/

}