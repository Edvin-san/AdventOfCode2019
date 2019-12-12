object Day12 {
	case class Vector(dims: List[Int]) {
		def +(that: Vector): Vector = Vector(dims.zip(that.dims).map{case (v1, v2) => v1 + v2})
		def -(that: Vector): Vector = Vector(dims.zip(that.dims).map{case (v1, v2) => v1 - v2})
		def signum = Vector(dims.map(math.signum(_)))
		def length = dims.map(_.abs).sum
	}
	implicit def ListIntToVector(dims: List[Int]) = Vector(dims)

	case class Body(pos: Vector, vel: Vector)

	def gravity(bodies: Seq[Body]): Seq[Body] = 
		for (b <- bodies) yield {
			val bodyDiffs: Seq[List[Int]] = bodies.map(other => (other.pos - b.pos).signum.dims)
			val axisDiffs: List[Int] = bodyDiffs.transpose.map(_.sum).toList
			Body(b.pos, b.vel + axisDiffs)
		}
	def velocity(bodies: Seq[Body]): Seq[Body] = bodies.map(b => Body(b.pos + b.vel, b.vel))

	def energy(body: Body): Int = {
		val potential = body.pos.length
		val kinetic = body.vel.length
		potential * kinetic
	}
	def systemKineticEnergy(bodies: Seq[Body]): Int = bodies.map(_.vel.length).sum
	def systemEnergy(bodies: Seq[Body]): Int = bodies.map(energy).sum
	
	def simulationStep = (velocity _ compose gravity)

	def part1(s: String = puzzleInput, totalSteps: Int = 1000): Int = {
		var bodies = parseBodies(puzzleInput)
		for (_ <- 1 to totalSteps) {
			bodies = simulationStep(bodies)
		}
		systemEnergy(bodies)
	}

	// Here is a little natural deduction-ish proof that we will reach the initial state (if repeating at all?).
	// Fact: The simulation step is a bijective function. Think about it, we can invert it.
	// Assuming we reach a state we have visited before, it must be the initial state.
	// Proof: Assume the opposite, e.g. a sequence like a, b, c, [d, e, f], [d, e, f], ...
	// Then simulationStep(c) = d, and simulationStep(f) = d which means that simulationStep is not bijective. A contradiction.
	// Thus, assuming we reach a state we have visited before, it must be the initial state.

	// The second trick is that it is enough to look for the first state where the velocity is all zeroes.
	// Not 100% sure why but somehow the problem is symmetrical (makes sense with two bodies at least) and before we reach
	// the initial state again, we will reach a halfway state where kinetic energy is zero.
	def cycleLen(startState: Seq[Body]) = (kineticEnergySimulation(startState).tail.indexOf(0) + 1)*2

	def kineticEnergySimulation(startState: Seq[Body]): Stream[Int] = 
			systemKineticEnergy(startState) #:: kineticEnergySimulation(simulationStep(startState))

	def part2(s: String = puzzleInput): BigInt = {
		val bodies = parseBodies(s)
		def bodyToAxisBodies(body: Body): Seq[Body] = body.pos.dims.map(dim => Body(List(dim), List(0)))
		val axisBodies: Seq[Seq[Body]] = bodies.map(bodyToAxisBodies).transpose
		lcm(axisBodies.map(cycleLen))
	}

	def parseVector(s: String) =  Vector(s.split(",").map(_.split("=").last).map(_.replace(">", "")).map(_.toInt).toList)
	def parseBodies(s: String): Seq[Body] = s.trim.split("\r\n").map(parseVector).map(v => Body(v, Vector(v.dims.map(_ => 0)))).toSeq

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

	def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)
	def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(BigInt(1))((a, b) => (a / gcd(a, b)) * b)
}