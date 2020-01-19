object Day22 {
	import Util._
	
	class Shuffler(val m: Int) {
		var deck = (0 until m).toList

		def cut(n: BigInt) = {
			val posN = posMod(n, m).toInt
			deck = deck.drop(posN) ++ deck.take(posN)
		}
		def dealNewStack() = deck = deck.reverse
		def dealWithIncrement(inc: BigInt) = {
			// deck[j] = deck[i*inc % 10007] := deck[i]
			// j = i*inc % 10007
			// i = j*inc^-1 % 10007
			val inverse = modularInverse(inc, m)
			deck = (0 until m).map(j => deck((j*inverse % m).toInt)).toList
		}

		def apply(i: Int) = deck(i)
		def getDeck = deck
	}

	case class LazyShuffler(val m: BigInt, val ops: List[ShuffleOperation]) {
		def deck(i: BigInt): BigInt = ops match {
			case Nil => i
			case op :: previousOps => 
				val indexBeforeOp: BigInt = op match {
					case DealNewStack => 
						m - i - 1
					case Cut(n) => 
						val posN = posMod(n, m)
						(i + posN) % m
					case DealWithIncrement(inc) =>
						val inverse = modularInverse(inc, m)
						i*inverse % m
				}
				LazyShuffler(m, previousOps).deck(indexBeforeOp)
		}
	}

	sealed trait ShuffleOperation
	sealed trait IrreducibleShuffleOperation extends ShuffleOperation
	case object DealNewStack extends ShuffleOperation
	case class Cut(n: BigInt) extends IrreducibleShuffleOperation
	case class DealWithIncrement(inc: BigInt) extends IrreducibleShuffleOperation

	case class ShuffleReducer(m: BigInt) {
		/* Reduces to at most two irreducible shuffle operations. */
		def reduceShuffleOperations(ops: List[IrreducibleShuffleOperation]): List[IrreducibleShuffleOperation] = 
			/*
			Cut(n): i => i - n mod m
			DealWithIncrement(inc): i => inc*i mod m
			=>
			1. Reducing the number of operations
			Cut(x)(Cut(y)): i => i - x - y mod m = i - (x + y) mod m == Cut(x + y)
			DealWithIncrement(x)(DealWithIncrement(y)): i => x*y*i mod m == DealWithIncrement(x*y)

			2. Changing order of operations.
			DealWithIncrement(inc)(Cut(n)): i => inc*(i - n) mod m = inc*i - inc*n mod m == Cut(inc*n)(DealWithIncrement(inc))

			This means that we can reduce a big list of operations into at most 2 :)
			*/
			ops match {
				case Nil => Nil
				case List(op) => List(op)
				case head :: tail => head :: reduceShuffleOperations(tail) match {
					// Only 3, 4, and 6 seem to be used.
					case List(Cut(x), Cut(y)) => 
						//println("1")
						List(Cut(posMod(x + y, m)))
					case List(DealWithIncrement(x), DealWithIncrement(y)) => 
						//println("2")
						List(DealWithIncrement(posMod(x*y, m)))
					case List(o1, o2) => 
						//println("3")
						List(o1, o2)

					case List(Cut(x), Cut(y), deal) => 
								//println("4")
								List(Cut((posMod(x + y, m))), deal)

					case List(Cut(x), DealWithIncrement(y), Cut(z)) => 
								//println("5")
								List(Cut(posMod(x + x*y, m)), DealWithIncrement(y))

					case List(DealWithIncrement(x), Cut(y), DealWithIncrement(z)) => 
								//println("6")
								List(Cut(posMod(x*y, m)), DealWithIncrement(posMod(x*z, m)))

					case List(DealWithIncrement(x), DealWithIncrement(y), cut) => 
								//println("7")
								List(DealWithIncrement(posMod(x*y, m)), cut)

					case hmm => 
						println("dafuq is " + hmm)
						???
				}
			}

		/*
		DealNewStack can be translated to a Cut operation combined with a DealWithIncrement operation.
		*/
		def reduceOp(op: ShuffleOperation): List[IrreducibleShuffleOperation] = op match {
			case DealNewStack => List(DealWithIncrement(posMod(-1, m)), Cut(posMod(-1, m)))
			case o: IrreducibleShuffleOperation => List(o)
		}

		def reduceRepeatedOperations(ops: List[IrreducibleShuffleOperation], r: BigInt): List[IrreducibleShuffleOperation] = {
			if (r == 0) {
				Nil
			} else if (r % 2 == 0) {
				val reducedHalves = reduceRepeatedOperations(ops, r / 2)
				reduceShuffleOperations(reducedHalves ++ reducedHalves)
			} else {
				val reduced = reduceRepeatedOperations(ops, r - 1)
				reduceShuffleOperations(ops ++ reduced)
			}
		}
	}

	def part1(s: String = puzzleInput) = {
		val shuffleOperations = s.linesIterator.map(parseOperation)
		val shuffler = new Shuffler(10007)
		for (op <- shuffleOperations) op match {
			case DealNewStack => shuffler.dealNewStack()
			case DealWithIncrement(inc) => shuffler.dealWithIncrement(inc)
			case Cut(n) => shuffler.cut(n)
		}

		shuffler.getDeck
	}

	def part1Lazy(s: String = puzzleInput) = {
		val shuffleOperations = s.linesIterator.map(parseOperation).toList.reverse
		val shuffler = LazyShuffler(10007, shuffleOperations)
		for (i <- 0 to 10006) yield shuffler.deck(i)
	}

	def part1LazyReduce(s: String = puzzleInput) = {
		val reducer = ShuffleReducer(10007)
		val shuffleOperations: List[IrreducibleShuffleOperation] = 
								s.linesIterator.map(parseOperation).toList.reverse
								.flatMap(reducer.reduceOp _)

		val reducedOperations = reducer.reduceShuffleOperations(shuffleOperations)
		val shuffler = LazyShuffler(10007, reducedOperations)
		for (i <- 0 to 10006) yield shuffler.deck(i)
	}

	/*
	This does not work. The cycle length is too long.
	*/
	def part2FindCycleLen(s: String = puzzleInput, maxCycleLen: Int) = {
		val ops = s.linesIterator.map(parseOperation).toList.reverse
		val reducer = ShuffleReducer(BigInt("119315717514047"))
		val reducedOps = reducer.reduceShuffleOperations(ops.flatMap(reducer.reduceOp _))
		val shuffler = LazyShuffler(BigInt("119315717514047"), reducedOps)
		def valueAfterOneWholeShuffle(i: BigInt): BigInt = shuffler.deck(i)
		def originStream(position: BigInt): Stream[BigInt] = 
			position #:: originStream(valueAfterOneWholeShuffle(position))
		var i = 0
		var stream = originStream(2020)
		var foundCycleLen = -1
		var last = stream.head
		for (_ <- 0 to maxCycleLen if foundCycleLen == -1) {
			i += 1
			val st = stream.head
			stream = stream.tail
			if (st == 2020 && i > 1) {
				println("Found 2020! : " + st)
				foundCycleLen = i
			}
			last = st
		}
		(foundCycleLen, last)
	}

	/*
	Shuffle operations can be combined. We only need Cut and DealWithIncrement. 
	Use exponentiation to get (in logarithmic time) the reduced operations after the huge number of repetitions.
	*/
	def part2(s: String = puzzleInput) = {
		val m = BigInt("119315717514047")
		val repetitions = BigInt("101741582076661")
		val reducer = ShuffleReducer(m)
		val shuffleOperations: List[IrreducibleShuffleOperation] = 
								s.linesIterator.map(parseOperation).toList.reverse
								.flatMap(reducer.reduceOp _)

		val reducedOperations = reducer.reduceShuffleOperations(shuffleOperations)
		val reducedRepeatedOps = reducer.reduceRepeatedOperations(reducedOperations, repetitions)

		val shuffler = LazyShuffler(m, reducedRepeatedOps)
		shuffler.deck(2020)
	}

	def parseOperation(line: String): ShuffleOperation = {
		if (line.startsWith("deal into")) {
			DealNewStack
		} else if (line.startsWith("deal with")) {
			DealWithIncrement(line.split(" ").last.toInt)
		} else if (line.startsWith("cut")) {
			Cut(line.split(" ").last.toInt)
		} else ???
	}

	val puzzleInput = 
		"""deal with increment 10
cut -5908
deal with increment 75
cut 8705
deal with increment 49
cut -1609
deal with increment 69
cut 7797
deal into new stack
cut -6211
deal with increment 10
cut 6188
deal with increment 57
cut -1659
deal with increment 16
cut -5930
deal into new stack
cut -2758
deal with increment 33
cut -3275
deal with increment 39
cut -1301
deal with increment 50
cut 7837
deal with increment 74
cut 1200
deal with increment 23
deal into new stack
cut -9922
deal with increment 65
cut 4896
deal with increment 61
deal into new stack
cut 5945
deal with increment 9
deal into new stack
deal with increment 2
cut -8205
deal with increment 75
cut -4063
deal with increment 40
deal into new stack
cut -7366
deal with increment 51
cut 7213
deal into new stack
cut 4763
deal with increment 43
cut 3963
deal with increment 50
cut -8856
deal with increment 43
cut 8604
deal with increment 72
cut -7026
deal into new stack
deal with increment 25
cut 7843
deal with increment 71
cut -1272
deal with increment 64
cut 7770
deal with increment 18
cut -5278
deal with increment 67
deal into new stack
deal with increment 18
deal into new stack
cut 2216
deal with increment 42
cut 3206
deal with increment 14
deal into new stack
cut -6559
deal into new stack
deal with increment 12
deal into new stack
deal with increment 75
deal into new stack
deal with increment 41
cut 7378
deal with increment 44
cut 774
deal with increment 60
cut 7357
deal with increment 41
cut 479
deal with increment 40
cut 5146
deal with increment 13
cut 2017
deal into new stack
deal with increment 35
cut 9218
deal into new stack
deal with increment 22
cut -2462
deal with increment 23
cut -1820
deal with increment 69"""

}