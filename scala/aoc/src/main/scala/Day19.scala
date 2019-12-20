object Day19 {
    import IntCodeComputer._
    import Util._

    def part1(s: String = puzzleInput) = {
        val computer = new IntCodeComputer("", s)
        var n = 0
        for (y <- 10 to 100) {
            var seen1 = false
            var continue = true
            var x = 0
            val line = Iterator.continually { 
                computer.reset()
                computer.addInputs(List(BigInt(x), BigInt(y)))
                computer.run()
                x += 1
                computer.output.headOption.map(_.toInt) match {
                    case Some(1) => 
                        n += 1
                        seen1 = true
                        '#'
                    case _ => 
                        if (seen1) continue = false
                        '.'
                }
            }.takeWhile(_ => continue)
            println(line.mkString)
        }
        n
    }

    def part2(s: String = puzzleInput, maxY: Int = 2000): Option[(Int, Int)] = {
        import scala.collection.mutable.Queue

        val computer = new IntCodeComputer("", s)
        val yRange = 10 to maxY
        var minX = 6
        var maxX = 6
        val maxXHistory: Queue[Int] = Queue()
        maxXHistory.enqueue(maxX)

        def has1(x: Int, y: Int): Boolean = {
            computer.reset()
            computer.addInputs(List(BigInt(x), BigInt(y)))
            computer.run()
            computer.output.head == 1
        }

        for (y <- yRange) {
            if (!has1(minX, y)) {
                minX += 1
            }
            if (has1(maxX + 2, y)) maxX += 2
            else if (has1(maxX + 1, y)) maxX += 1

            maxXHistory.enqueue(maxX)
            if (maxXHistory.size == 100) {
                val maxX100back = maxXHistory.dequeue()
                if (maxX100back - 99 >= minX) {
                    return Some((maxX100back - 99, y - 99))
                }
            }

            val width = maxX - minX + 1
            if (y % 50 == 0) {
                println(s"y: $y, minX: $minX, maxX: $maxX, width: $width")
            }
        }

        None
    }

    val puzzleInput = "109,424,203,1,21101,0,11,0,1106,0,282,21102,1,18,0,1106,0,259,2101,0,1,221,203,1,21101,0,31,0,1106,0,282,21101,0,38,0,1106,0,259,21001,23,0,2,21202,1,1,3,21102,1,1,1,21102,1,57,0,1106,0,303,2102,1,1,222,20102,1,221,3,21001,221,0,2,21102,1,259,1,21102,80,1,0,1106,0,225,21102,106,1,2,21102,91,1,0,1105,1,303,1201,1,0,223,21001,222,0,4,21101,259,0,3,21102,1,225,2,21101,225,0,1,21101,0,118,0,1106,0,225,20101,0,222,3,21102,42,1,2,21101,133,0,0,1105,1,303,21202,1,-1,1,22001,223,1,1,21101,0,148,0,1106,0,259,1201,1,0,223,21001,221,0,4,20101,0,222,3,21101,10,0,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21101,195,0,0,106,0,108,20207,1,223,2,20102,1,23,1,21101,-1,0,3,21101,214,0,0,1105,1,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,1202,-4,1,249,22102,1,-3,1,22101,0,-2,2,21202,-1,1,3,21101,250,0,0,1105,1,225,21202,1,1,-4,109,-5,2106,0,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2105,1,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,22102,1,-2,-2,109,-3,2106,0,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,21202,-2,1,3,21101,343,0,0,1106,0,303,1105,1,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,22101,0,-4,1,21102,384,1,0,1106,0,303,1105,1,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,22102,1,1,-4,109,-5,2105,1,0"
}