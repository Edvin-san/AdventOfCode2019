object Day11 {
	import IntCodeComputer._

	// Turtle graphics. Keep track of direction. Can use polar coordinates.
	// Point(x1, y1) + Point(x2, y2) = Point(x1 + x2, y1 + y2)

	case class Vector(x: Int, y: Int) {
		def +(that: Vector) = Vector(x + that.x, y + that.y)
		def rotatedClockwise90: Vector = 
			(this.x, this.y) match {
				case (0, 1) => Vector(1, 0)
				case (1, 0) => Vector(0, -1)
				case (0, -1) => Vector(-1, 0)
				case (-1, 0) => Vector(0, 1)
				case _ => ???
			}

		def rotatedCounterClockwise90 = this.rotatedClockwise90.rotatedClockwise90.rotatedClockwise90
	}

	def part1(s: String = puzzleInput) = {
		var curr = Vector(0, 0)
		var painted: Set[Vector] = Set()
		var dir = Vector(0, 1)
		var color: Map[Vector, Int] = Map()  

		var prog = s.split(",").map(i => BigInt(i)).toArray
		var computer = new IntCodeComputer("", prog)

		while (!computer.hasTerminated) {
			computer.addInputs(List(BigInt(color.get(curr).getOrElse(0))))
			computer.run()
			computer.output match {
				case Nil => 
				case List(paint, rotation) => {
					painted = painted + curr
					color = color + (curr -> paint.toInt)
					dir = rotation.intValue match {
						case 1 => dir.rotatedClockwise90
						case 0 => dir.rotatedCounterClockwise90
						case _ => ???
					}
					curr = curr + dir
				}
			}
		}

		painted.size
	}

	def part1Draw() = {
		part2(puzzleInput, 0, 0)
	}

	def part2Draw() = {
		part2(puzzleInput, 1, 100)
	}

	def part2(s: String = puzzleInput, initialColor: Int = 1, drawTimeMs: Int = 100) = {
		var curr = Vector(0, 0)
		var painted: Set[Vector] = Set()
		var dir = Vector(0, 1)
		var color: Map[Vector, Int] = Map(curr -> initialColor)

		var prog = s.split(",").map(i => BigInt(i)).toArray
		var computer = new IntCodeComputer("", prog)

		def printMap() = {
			val minX = math.min(0, color.keySet.map(_.x).min)
			val maxX = math.max(44, color.keySet.map(_.x).max)
			val minY = math.min(-6, color.keySet.map(_.y).min)
			val maxY = math.max(2, color.keySet.map(_.y).max)

			for (y <- maxY to minY by -1) {
				val line = for (x <- minX to maxX) yield {
					if (curr == Vector(x, y)) (dir.x, dir.y) match {
						case (0, 1) => "^"
						case (0, -1) => "v"
						case (1, 0) => ">"
						case (-1, 0) => "<"
						case _ => ???
					} else color.get(Vector(x, y)).getOrElse(0) match {
						case 0 => "."
						case 1 => "#"
						case _ => ???
					}
				}
				println(line.mkString)
			}
		}

		while (!computer.hasTerminated) {
			computer.addInputs(List(BigInt(color.get(curr).getOrElse(0))))
			computer.run()
			computer.output match {
				case Nil => 
				case List(paint, rotation) => {
					painted = painted + curr
					color = color + (curr -> paint.toInt)
					dir = rotation.intValue match {
						case 1 => dir.rotatedClockwise90
						case 0 => dir.rotatedCounterClockwise90
						case _ => ???
					}
					curr = curr + dir
				}
			}
			printMap()
			println()
			Thread.sleep(drawTimeMs)
		}

		printMap()
	}

	val puzzleInput = "3,8,1005,8,327,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,28,1006,0,42,2,1104,11,10,1006,0,61,2,1005,19,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,65,1006,0,4,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,89,1,1108,10,10,1,1103,11,10,1,109,18,10,1006,0,82,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,126,2,109,7,10,1,104,3,10,1006,0,64,2,1109,20,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,163,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,185,2,1109,12,10,2,103,16,10,1,107,11,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,219,1,1005,19,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,245,2,1002,8,10,1,2,9,10,1006,0,27,1006,0,37,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,281,1006,0,21,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,306,101,1,9,9,1007,9,1075,10,1005,10,15,99,109,649,104,0,104,1,21102,1,847069852568,1,21101,344,0,0,1105,1,448,21101,0,386979963688,1,21101,355,0,0,1105,1,448,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,46346031251,1,1,21101,0,402,0,1105,1,448,21102,1,29195594775,1,21101,0,413,0,1105,1,448,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,868498428772,1,21101,0,436,0,1106,0,448,21102,718170641172,1,1,21102,1,447,0,1105,1,448,99,109,2,21202,-1,1,1,21102,40,1,2,21102,1,479,3,21102,1,469,0,1105,1,512,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,474,475,490,4,0,1001,474,1,474,108,4,474,10,1006,10,506,1101,0,0,474,109,-2,2106,0,0,0,109,4,2102,1,-1,511,1207,-3,0,10,1006,10,529,21101,0,0,-3,22101,0,-3,1,22101,0,-2,2,21101,0,1,3,21101,548,0,0,1106,0,553,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,576,2207,-4,-2,10,1006,10,576,21202,-4,1,-4,1106,0,644,22101,0,-4,1,21201,-3,-1,2,21202,-2,2,3,21102,1,595,0,1105,1,553,21201,1,0,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,614,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,636,22102,1,-1,1,21102,1,636,0,106,0,511,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0"
}