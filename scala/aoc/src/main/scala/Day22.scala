object Day22 {
	import Util._
	
	class Shuffler(val m: Int) {
		var deck = (0 until m).toList

		def cut(n: Int) = {
			val posN = posMod(n, m)
			deck = deck.drop(posN) ++ deck.take(posN)
		}
		def dealNewStack() = deck = deck.reverse
		def dealWithIncrement(inc: Int) = {
			// deck[j] = deck[i*inc % 1007] := deck[i]
			// j = i*inc % 1007
			// i = j*inc^-1 % 1007
			val inverse = modularInverse(inc, m)
			deck = (0 until m).map(j => deck(j*inverse % m)).toList
		}

		def apply(i: Int) = deck(i)
		def getDeck = deck
	}

	sealed trait ShuffleOperation
	case object DealNewStack extends ShuffleOperation
	case class Cut(n: Int) extends ShuffleOperation
	case class DealWithIncrement(inc: Int) extends ShuffleOperation

	def part1(s: String = puzzleInput) = {
		val shuffleOperations = s.lines.map(parseOperation)
		val shuffler = new Shuffler(10007)
		for (op <- shuffleOperations) op match {
			case DealNewStack => shuffler.dealNewStack()
			case DealWithIncrement(inc) => shuffler.dealWithIncrement(inc)
			case Cut(n) => shuffler.cut(n)
		}

		shuffler.getDeck
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