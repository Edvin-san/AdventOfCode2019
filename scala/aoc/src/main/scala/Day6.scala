object Day6 {

	import scala.collection.mutable.Queue

	def runPart1(s: String): Int = {
		val graph = parseGraph1(s)

		var count: Map[String, Int] = Map()

		def orbits(s: String): Int = {
			if (!graph.contains(s)) 0
			else if (count.contains(s)) count(s)
			else {
				val c = graph(s).map(to => 1 + orbits(to)).sum
				//println("adding " + (s, c))
				count = count + (s -> c)
				//println(count)
				c
			}
		}

		val orbitCounts = graph.keySet.toList.map(orbits)

		orbitCounts.sum
	}

	def runPart2(s: String): Int = {
		val graph = parseGraph2(s)
		val start = graph("YOU").head
		val end = graph("SAN").head

		var visited = Set(start)

		var queue: Queue[(String, Int)] = Queue((start, 0))
		var found = false
		var requiredSteps = -1
		while (!queue.isEmpty && !found) {
			val (next, steps) = queue.dequeue
			visited = visited + next

			if (next == end) {
				found = true
				requiredSteps = steps
			} else {
				for (n <- graph(next).filter(n => !visited(n))) {
					val ne = (n, steps + 1)
					queue += ne
				}
			}
		}

		requiredSteps
	}

	def parseGraph1(s: String): Map[String, List[String]] = {
		val edges = for (pair <- s.split(",").map(_.split("\\)"))) yield pair match {
			case Array(to, from) => (from, to)
			case _ => ???
		}

		edges.groupBy(_._1).map { case (f, l) => (f, l.map(_._2).toList) }
	}

	def parseGraph2(s: String): Map[String, List[String]] = {
		val edges = for (pair <- s.split(",").map(_.split("\\)"))) yield pair match {
			case Array(to, from) => Seq((from, to), (to, from))
			case _ => ???
		}

		edges.flatten.groupBy(_._1).map { case (f, l) => (f, l.map(_._2).toList) }
	}

}