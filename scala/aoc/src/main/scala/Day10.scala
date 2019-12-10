object Day10 {
	
	case class Point(value: String, x: Int, y: Int)

	def dist(p1: Point, p2: Point): Double = {
		math.sqrt(math.pow(p1.x - p2.x, 2) + math.pow(p1.y - p2.y, 2))
	}
	
	case class PolarCoordinate(r: Double, theta: Double)
	case class Direction(theta: Double)

	case class MonitoringStation(asteroid: Point, otherAsteroids: Set[Point]) {
		val polarCoordinate: Map[Point, PolarCoordinate] = otherAsteroids.toSeq.map(o => {
				val dir = math.atan2(-(o.y - asteroid.y), o.x - asteroid.x)
				val pc = PolarCoordinate(dist(asteroid, o), dir)
				(o, pc)
			}).toMap
		val asteroidDirection: Map[Point, Direction] = polarCoordinate.map {
			case (point, pc) => (point, Direction(pc.theta))
		}.toMap

		def distinctDirections: Set[Direction] = asteroidDirection.values.toSet

		def view: Set[Point] = {
			val asteroidsByDir = asteroidDirection.keySet.toSeq.groupBy(asteroidDirection(_))

			val dirName = distinctDirections.zip("ABCDEFGHIJKLMNOPQRSTUVWXYZ").toMap
			val newView = asteroidsByDir.toSeq.flatMap { 
				case (dir, asteroids) => asteroids.toSeq.sortBy(p => dist(asteroid, p)) match {
					case first :: rest => Point(dirName(dir).toString, first.x, first.y) :: rest.map(a => 
						Point(dirName(dir).toString.toLowerCase(), a.x, a.y))
				}
			}
			newView.toSet + asteroid
		}

		def scanNorthClockwise: Seq[Point] = {
			def transposeAngle(theta: Double): Double = {
				val northCounterClockWise = theta - math.Pi / 2
				val northClockWise = -northCounterClockWise
				if (northClockWise < 0) northClockWise + math.Pi*2 else northClockWise
			}

			val transposedDirByPoint = polarCoordinate.map {
				case (point, PolarCoordinate(r, theta)) => (point, Direction(transposeAngle(theta)))
			}

			val pointByTransposedDir = transposedDirByPoint.keySet.toSeq.groupBy(transposedDirByPoint(_))

			pointByTransposedDir.toSeq.sortBy(_._1.theta).map {
				case (dir, asteroids) => asteroids.toSeq.sortBy(p => dist(asteroid, p)) match {
					case closest :: rest => closest
				}
			}
		}
	}

	def laserOrder(station: MonitoringStation): Seq[Point] = {
		station.scanNorthClockwise match {
			case Nil => Nil
			case points => points ++ laserOrder(MonitoringStation(station.asteroid, station.otherAsteroids diff points.toSet))
		}
	}

	def print(asteroids: Set[Point]): Unit = {
		val maxY = asteroids.map(_.y).max
		val maxX = asteroids.map(_.x).max
		for(y <- 0 to maxY) {
			val line = for (x <- 0 to maxX) yield {
				asteroids.find(p => p.x == x && p.y == y) match {
					case Some(Point(name, _, _)) => name
					case None => "."
				}
			}
			println(line.mkString)
		}
	}

	def testPrintPoints(s: String) = {
		print(parseAsteroids(s))
	}

	def testPrintScore(s: String) = {
		val asteroids = parseAsteroids(s)
		val scoredAsteroids = for (a <- asteroids) yield {
			val station = MonitoringStation(a, asteroids - a)
			Point(station.distinctDirections.size.toString, a.x, a.y) 
		}
		print(scoredAsteroids)
	}

	def parseAsteroids(s: String): Set[Point] = {
		val points = for {
			(row, rowIndex) <- s.trim.split("\n").zipWithIndex
			(value, colIndex) <- row.trim.zipWithIndex 
		} yield Point(value.toString, colIndex, rowIndex)
		
		points.filter(p => p.value == "#").toSet
	}

	def runPart1(s: String = sPuzzleInput) = {
		val asteroids = parseAsteroids(s)
		bestMonitoringStation(asteroids).distinctDirections.size
	}

	def runPart2(s: String = sPuzzleInput) = {
		val asteroids = parseAsteroids(s)
		val station = bestMonitoringStation(asteroids)
		val lasered = laserOrder(station)
		lasered(199) match {
			case Point(_, x, y) => x*100 + y
		}
	}

	def bestMonitoringStation(asteroids: Set[Point]): MonitoringStation =
		asteroids.map(a => MonitoringStation(a, asteroids - a)).maxBy(_.distinctDirections.size)

	def printAllViews(s: String): Unit = {
		val asteroids = parseAsteroids(s)
		val stations = asteroids.map(a => MonitoringStation(a, asteroids - a))
		for (station <- stations) {
			printStation(station)
		}
	}

	def printStation(station: MonitoringStation): Unit = {
		println()
		println(station.asteroid)
		print(station.view)
		println()
	}

	def testLaser(s: String = sLaser): Unit = {
		val asteroids = parseAsteroids(s)
		var station = bestMonitoringStation(asteroids)

		def removeLaserRound() = {
			station.scanNorthClockwise match {
				case Nil => false
				case points => {
					println(points) 
					station = MonitoringStation(station.asteroid, station.otherAsteroids diff points.toSet)
					true
				}
			}
		}

		while (removeLaserRound()) {
			printStation(station)
		}
	}

	val sLaser = 
		"""
		.#....#####...#..
		##...##.#####..##
		##...#...#.#####.
		..#.....X...###..
		..#.#.....#....##
		"""

	val s8 = """
		.#..#
		.....
		#####
		....#
		...##
		""";

	val s33 = """
		......#.#.
		#..#.#....
		..#######.
		.#.#.###..
		.#..#.....
		..#....#.#
		#..#....#.
		.##.#..###
		##...#..#.
		.#....####
		"""

	val s35 = """
		#.#...#.#.
		.###....#.
		.#....#...
		##.#.#.#.#
		....#.#.#.
		.##..###.#
		..#...##..
		..##....##
		......#...
		.####.###.
		"""

	val s41 = """
		.#..#..###
		####.###.#
		....###.#.
		..###.##.#
		##.##.#.#.
		....###..#
		..#.#..#.#
		#..#.#.###
		.##...##.#
		.....#.#..
		"""

	val s210 = """
		.#..##.###...#######
		##.############..##.
		.#.######.########.#
		.###.#######.####.#.
		#####.##.#.##.###.##
		..#####..#.#########
		####################
		#.####....###.#.#.##
		##.#################
		#####.##.###..####..
		..######..##.#######
		####.##.####...##..#
		.#####..#.######.###
		##...#.##########...
		#.##########.#######
		.####.#.###.###.#.##
		....##.##.###..#####
		.#.#.###########.###
		#.#.#.#####.####.###
		###.##.####.##.#..##
		"""

	val sPuzzleInput = """
	.#.####..#.#...#...##..#.#.##.
	..#####.##..#..##....#..#...#.
	......#.......##.##.#....##..#
	..#..##..#.###.....#.#..###.#.
	..#..#..##..#.#.##..###.......
	...##....#.##.#.#..##.##.#...#
	.##...#.#.##..#.#........#.#..
	.##...##.##..#.#.##.#.#.#.##.#
	#..##....#...###.#..##.#...##.
	.###.###..##......#..#...###.#
	.#..#.####.#..#....#.##..#.#.#
	..#...#..#.#######....###.....
	####..#.#.#...##...##....#..##
	##..#.##.#.#..##.###.#.##.##..
	..#.........#.#.#.#.......#..#
	...##.#.....#.#.##........#..#
	##..###.....#.............#.##
	.#...#....#..####.#.#......##.
	..#..##..###...#.....#...##..#
	...####..#.#.##..#....#.#.....
	####.#####.#.#....#.#....##.#.
	#.#..#......#.........##..#.#.
	#....##.....#........#..##.##.
	.###.##...##..#.##.#.#...#.#.#
	##.###....##....#.#.....#.###.
	..#...#......#........####..#.
	#....#.###.##.#...#.#.#.#.....
	.........##....#...#.....#..##
	###....#.........#..#..#.#.#..
	##...#...###.#..#.###....#.##.
	"""

}