object Day12 {
	case class Vector(dims: List[Int]) {
		def +(that: Vector): Vector = Vector(dims.zip(that.dims).map{case (v1, v2) => v1 + v2}.toList)
	}

	case class Body(pos: Vector, vel: Vector)

	def gravity(bodies: Seq[Body]): Seq[Body] = {
		for (b <- bodies) yield {
			val allDimDiffs = for (b2 <- bodies) yield {
				val dimDiffs = b2.pos.dims.zip(b.pos.dims).map {
					case (b2pos, bpos) => if (b2pos > bpos) 1 else if (b2pos < bpos) -1 else 0
				}
				dimDiffs.toList
			}
			val aggrDimDiffs = Vector(allDimDiffs.transpose.map(_.sum).toList)
			Body(b.pos, b.vel + aggrDimDiffs)
		}
	}

	def velocity(bodies: Seq[Body]): Seq[Body] = bodies.map(b => Body(b.pos + b.vel, b.vel))

	def energy(body: Body): Int = {
		val potential = body.pos.dims.map(math.abs(_)).sum
		val kinetic = body.vel.dims.map(math.abs(_)).sum
		potential * kinetic
	}

	def systemEnergy(bodies: Seq[Body]): Int = bodies.map(energy).sum

	def tick(bodies: Seq[Body]): Seq[Body] = (velocity _ compose gravity)(bodies)

	def part1(s: String = puzzleInput, totalSteps: Int = 1000): Int = {
		var bodies = parseBodies(puzzleInput)
		for (_ <- 1 to totalSteps) {
			bodies = tick(bodies)
		}
		systemEnergy(bodies)
	}

	def parseVector(s: String) =  Vector(s.split(",").map(_.split("=").last).flatMap(_.split(">")).map(_.toInt).toList)

	def parseBodies(s: String) = s.trim.split("\r\n").map(parseVector).map(v => Body(v, Vector(v.dims.map(_ => 0)))).toSeq

	val exampleInput = 
		"""
		<x=-1, y=0, z=2>
		<x=2, y=-10, z=-7>
		<x=4, y=-8, z=8>
		<x=3, y=5, z=-1>
		"""

	val puzzleInput: String = 
		"""
		<x=12, y=0, z=-15>
		<x=-8, y=-5, z=-10>
		<x=7, y=-17, z=1>
		<x=2, y=-11, z=-6>
		"""
}