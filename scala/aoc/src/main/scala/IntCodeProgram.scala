object IntCodeProgram {
	implicit def intArrayToMutableProgram(arr: Array[Int]): MutableProgram = 
		createMutableProgram(arr.map(a => BigInt(a)).toSeq)

	implicit def bigIntArrayToMutableProgram(arr: Array[BigInt]): MutableProgram =
		createMutableProgram(arr.toSeq)

	implicit def stringToMutableProgram(s: String): MutableProgram = createMutableProgram(s.split(",").map(BigInt(_)).toSeq)

	implicit def createMutableProgram(instructions: Seq[BigInt]): MutableProgram = {
		val program = instructions
			.map(i => i)
			.zipWithIndex
			.map { case (value, index) => (BigInt(index), value) }
			.toMap
		new MutableProgram(program)
	}

	class MutableProgram(val program: Map[BigInt, BigInt]) {
		var prog = program
		var rb = BigInt(0)

		def reset(): Unit = {
			prog = program
			rb = 0
		}

		def apply(index: BigInt): BigInt = {
			validateIndex(index)
			prog.get(index).getOrElse(0)
		}

		def update(index: BigInt, value: BigInt): Unit = {
			validateIndex(index)
			prog = prog + (index -> value)
		}

		def relativeBase: BigInt = rb

		def updateRelativeBase(newValue: BigInt): Unit = rb = newValue

		def print(index: BigInt): Unit = {
			val indices = prog.keySet.toList.sorted
			val edgeIndices = prog.keySet.filter(i => !prog.keySet.contains(i + 1))

			val valueString = indices.map(i => 
				(if (i == index) ("[" + prog(i) + "]") else prog(i)) + 
				(if (edgeIndices.contains(i)) s"($i)..." else if (i % 5 == 0) s"($i)" else "")).mkString(", ")

			println(s"relative base: $relativeBase")
			println(valueString)
		}

		private def validateIndex(index: BigInt): Unit = {
			if (index < 0) throw new IndexOutOfBoundsException("Negative index not allowed")
		}
	}
}