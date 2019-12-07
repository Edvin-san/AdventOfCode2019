object IntCodeComputationCoordinator {
	import IntCodeComputer._

	def createSerialPipe(computations: Seq[IntCodeComputation]): Map[IntCodeComputation, IntCodeComputation] = {
		computations.sliding(2).map(t => (t(0), t(1))).toMap
	}

	def createFeedbackLoopPipe(computations: Seq[IntCodeComputation]): Map[IntCodeComputation, IntCodeComputation] = {
		createSerialPipe(computations) ++ Map(computations.last -> computations.head)
	}

	// Runs combined computations until some computer can't output because its target has terminated (or is None).
	class IntCodeCombinedComputation(
		val id: String,
		val first: IntCodeComputation,
		val pipe: Map[IntCodeComputation, IntCodeComputation]
	) extends IntCodeComputation with BasicIOIntCodeComputation {
		override def identifier = id

		override def reset() = {
			super.reset()
			val computations = Set(first) ++ pipe.keySet ++ pipe.values.toSet
			computations.foreach(_.reset())
		}

		override def run() = {
			var curr = first
			curr.addInputs(inputs.dequeueAll(_ => true).toList)

			var continue = true
			var loops = 0
			while (continue) {
				curr.run()
				val out = curr.output
				pipe.get(curr) match {
					case Some(computation) if !computation.hasTerminated => {
						computation.addInputs(out)
						curr = computation
					}
					case _ => {
						continue = false
						outputs = out
					}
				}
				loops = loops + 1
				if (loops > 500) throw new IllegalArgumentException("Too many loops")
			}

			terminated = true
		}
	}

}