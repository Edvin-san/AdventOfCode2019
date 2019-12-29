object Day24 {
	case class Pos(x: Int, y: Int) {
		def +(that: Pos) = Pos(x + that.x, y + that.y)
	}

	case class BugState(state: Set[Pos]) {
		def isWithin(p: Pos) = p.x >= 0 && p.x <= 4 && p.y >= 0 && p.y <= 4

		def numNeighbors(p: Pos) = Seq(Pos(0, 1), Pos(0, -1), Pos(-1, 0), Pos(1, 0))
									.map(d => p + d)
									.filter(n => isWithin(n) && state.contains(n))
									.size

		def nextState: BugState = {
			val bugs = for (y <- 0 to 4; x <- 0 to 4) yield {
				val p = Pos(x, y)
				val n = numNeighbors(p)
				if (state.contains(p) && n == 1 || !state.contains(p) && n >= 1 && n <= 2) Some(p) else None
			}
			BugState(bugs.flatten.toSet)
		}

		def biodiversityRating = {
			var rating = 0
			var i = 0
			for (y <- 0 to 4; x <- 0 to 4) {
				if (state.contains(Pos(x, y))) {
					rating |= (1 << i)
				}
				i += 1
			}
			rating
		}
	}

	case class RecursiveBugState(state: Set[(Int, Pos)]) {
		val minLevel = state.map(_._1).min
		val maxLevel = state.map(_._1).max

		def neighborsInDirection(d: Int, p: Pos, dir: Pos): Seq[(Int, Pos)] = p + dir match {
			case Pos(2, 2) => dir match {
				case Pos(0, 1) => (0 to 4).map(i => (d + 1, Pos(i, 0))).toSeq // South
				case Pos(0, -1) => (0 to 4).map(i => (d + 1, Pos(i, 4))).toSeq // North
				case Pos(-1, 0) => (0 to 4).map(i => (d + 1, Pos(4, i))).toSeq // West
				case Pos(1, 0) => (0 to 4).map(i => (d + 1, Pos(0, i))).toSeq // East
				case _ => ???
			}
			case Pos(x, y) =>
				if (x > 4) Seq((d - 1, Pos(3, 2)))
				else if (y > 4) Seq((d - 1, Pos(2, 3)))
				else if (x < 0) Seq((d - 1, Pos(1, 2)))
				else if (y < 0) Seq((d - 1, Pos(2, 1)))
				else Seq((d, Pos(x, y)))
		}

		def numNeighbors(level: Int, p: Pos) = Seq(Pos(0, 1), Pos(0, -1), Pos(-1, 0), Pos(1, 0))
									.flatMap(d => neighborsInDirection(level, p, d))
									.filter(n => state.contains(n))
									.size

		def nextState: RecursiveBugState = {
			val bugs = for (level <- minLevel - 1 to maxLevel + 1; y <- 0 to 4; x <- 0 to 4) yield {
				val p = Pos(x, y)
				val n = numNeighbors(level, p)
				if (p != Pos(2, 2) &&
					(state.contains((level, p)) && n == 1
					|| !state.contains((level, p)) && n >= 1 && n <= 2)) Some((level, p))
				else None
			}
			RecursiveBugState(bugs.flatten.toSet)
		}

		def print(): Unit = {
			for (level <- minLevel - 1 to maxLevel + 1) {
				println(s"Depth $level:")
				for (y <- 0 to 4) {
					val line = for (x <- 0 to 4) yield if (state((level, Pos(x, y)))) '#' else '.'
					println(line.mkString)
				}
				println()
			}
		}
	}

	def recursiveBugStateStream(initialState: RecursiveBugState): Stream[RecursiveBugState] = initialState #:: recursiveBugStateStream(initialState.nextState)

	def bugStateStream(initialState: BugState): Stream[BugState] = initialState #:: bugStateStream(initialState.nextState)

	def part1(s: String = puzzleInput): Int = {
		def firstDupBiodiversityRating(stream: Stream[BugState], seenBioDiv: Set[Int] = Set.empty[Int]): Int = stream match {
			case head #:: tail if (seenBioDiv(head.biodiversityRating)) => head.biodiversityRating
			case head #:: tail => firstDupBiodiversityRating(tail, seenBioDiv + head.biodiversityRating)
		}

		firstDupBiodiversityRating(bugStateStream(parseState(s)))
	}

	def part2(s: String = puzzleInput, minutes: Int = 200): Int = {
		val bugState = parseState(s)
		val recursiveBugState = RecursiveBugState(bugState.state.map(p => (0, p)).toSet)
		val bugStates = recursiveBugStateStream(recursiveBugState)
		bugStates(minutes).state.size
	}

	def debugPart2(s: String = puzzleInput, minutes: Int = 10) = {
		val bugState = parseState(s)
		val recursiveBugState = RecursiveBugState(bugState.state.map(p => (0, p)).toSet)
		val bugStates = recursiveBugStateStream(recursiveBugState)
		for (i <- 0 to minutes) {
			val bs = bugStates(i)
			println(s"Minute $i -----------------------------")
			bs.print()
			println(s"Total bugs: ${bs.state.size}")
			println("----------------------------------------")
		}
	}

	def parseState(s: String): BugState = {
		val lines = s.trim.split("\n").map(_.trim)
		val bugs = for (y <- 0 to 4; x <- 0 to 4) yield lines(y)(x) match {
			case '#' => Some(Pos(x, y))
			case '.' => None
		}
		BugState(bugs.flatten.toSet)
	}

	val example =
		"""
		....#
		#..#.
		#..##
		..#..
		#....
		"""

	val puzzleInput = 
		"""
		#####
		.....
		....#
		#####
		.###.
		"""
}