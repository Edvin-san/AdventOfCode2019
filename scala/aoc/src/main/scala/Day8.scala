object Day8 {
	case class Layer(rows: List[List[Int]]) {
		def apply(i: Int, j: Int) = rows(i)(j)
		def numberOf(digit: Int): Int = rows.map(r => r.filter(n => n == digit).size).sum
		def asDigitList = rows.flatten
		override def toString(): String = rows.map(_.mkString("")).mkString("\n")
		
	}

	def composeToImageLayer(layers: Seq[Layer]): Layer = {
		val height = layers(0).rows.size
		val width = layers(0).rows(0).size

		def pixelColor(pixels: Seq[Int]): Int = pixels.find(_ != 2).getOrElse(2)

		val imageString = layers.map(_.asDigitList).transpose.map(pixelColor).mkString("")
		val imageLayer = parseLayer(width, height)(imageString)

		imageLayer
	}

	def part1(s: String): Int = {
		val layers = parsePart1(s)
		val minLayer = layers.minBy(_.numberOf(0))
		minLayer.numberOf(1) * minLayer.numberOf(2)
	}

	def part2(s: String): String = {
		val layers = parsePart1(s)
		val imageLayer = composeToImageLayer(layers)
		imageLayer.toString()
	}

	def parsePart1: String => List[Layer] = parseLayers(25, 6)

	def parseLayers(width: Int, height: Int)(s: String): List[Layer] = {
		val layers = s.grouped(width*height)

		layers.map(parseLayer(width, height)).toList
	}

	def parseLayer(width: Int, height: Int)(l: String): Layer = 
			Layer(l.grouped(width).map(r => r.map(_.asDigit).toList).toList)
}