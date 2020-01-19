object Day17 {
    import IntCodeComputer._
    // Part2: Find one simple path. Compress using text editor xD


    def part1(s: String = puzzleInput) = {
        val computer = new IntCodeComputer("", s)
        computer.run()
        val o = computer.output.map(_.toInt.toChar).mkString
        val (graph, _, _) = makeScaffoldGraph(o)
        
        graph.filter {
            case (pos, neighbors) => neighbors.size == 4
        }.keySet.map {
            case Pos(x, y) => x * y
        }.sum
    }

    def printMap(s: String = puzzleInput) = {
        val computer = new IntCodeComputer("", s)
        computer.run()
        println(computer.output.map(_.toInt.toChar).mkString)
    }

    case class Pos(x: Int, y: Int) {
        def +(that: Pos): Pos = Pos(x + that.x, y + that.y)
    }

    // Returns the Map of scaffolding, the position of the robot and its direction as a position (e.g. Pos(1, 0) for east)
    def makeScaffoldGraph(s: String): (Map[Pos, Seq[Pos]], Pos, Pos) = {
        val lines: Array[String] = s.split("\n")
        var map: Map[Pos, Seq[Pos]] = Map()
        var robotPos: Pos = Pos(0, 0)
        var dir: Pos = Pos(0, 0)

        def valueAt(pos: Pos): Char = lines(pos.y)(pos.x)

        for (y <- 0 until lines.size; x <- 0 until lines(y).size) {
            valueAt(Pos(x, y)) match {
                case '#' => 
                    val neighbors = Seq(Pos(x, y + 1), Pos(x, y - 1), Pos(x + 1, y), Pos(x - 1, y))
                                    .filter(p => p.x >= 0 && p.x < lines(y).size && p.y >= 0 && p.y < lines.size)
                                    .filter(p => valueAt(p) == '#' || valueAt(p) == 'v' || valueAt(p) == '<' || valueAt(p) == '>' || valueAt(p) == '^')
                    map = map + (Pos(x, y) -> neighbors)
                case 'v' =>
                    robotPos = Pos(x, y)
                    dir = Pos(0, 1)
                case '<' =>
                    robotPos = Pos(x, y)
                    dir = Pos(-1, 0)
                case '>' =>
                    robotPos = Pos(x, y)
                    dir = Pos(1, 0)
                case '^' =>
                    robotPos = Pos(x, y)
                    dir = Pos(0, -1)
                case x if x != '.' =>
                    println("Could not match " + x)
                case _ =>
            }
        }
        map = map + (robotPos -> map.filter(t => t._2.contains(robotPos)).map(_._1).toSeq)

        (map, robotPos, dir)
    }

    def part2(s: String = puzzleInput): BigInt = {
        val program = s.split(",").map(BigInt(_)).toArray
        program(0) = 2
        val computer = new IntCodeComputer("", program)
        def toAscii(m: String): List[BigInt] = (m.map(_.toInt) :+ 10).map(BigInt(_)).toList
        val input = 
        """
        A,B,A,C,B,A,C,A,C,B
        L,12,L,8,L,8
        L,12,R,4,L,12,R,6
        R,4,L,12,L,12,R,6
        n
        """
        val asciiInputs = input.trim.split("\n").map(t => toAscii(t.trim))
        for (ai <- asciiInputs) computer.addInputs(ai)
        computer.run()
        computer.output.last
    }

    def getPath(s: String = puzzleInput): Array[String] = {
        val computer = new IntCodeComputer("", s)
        computer.run()
        val o = computer.output.map(_.toInt.toChar).mkString
        var (graph, robotPos, dir) = makeScaffoldGraph(o)

        var result: Array[String] = Array()
        var steps = 0
        var visited = Set(robotPos)
        var continue = true
        while (continue) {
            val forwardPos = robotPos + dir
            if (graph.keySet.contains(forwardPos)) {
                visited = visited + forwardPos
                robotPos = forwardPos
                steps += 1
            }
            else {
                if (steps > 0) result = result :+ steps.toString
                steps = 0
                graph(robotPos).filter(!visited.contains(_)) match {
                    case Nil => 
                        continue = false
                    case Seq(neighbor) => 
                        if (neighbor == robotPos + rotateRight(dir)) {
                            dir = rotateRight(dir)
                            result = result :+ "R"
                        } else if (neighbor == robotPos + rotateLeft(dir)) {
                            dir = rotateLeft(dir)
                            result = result :+ "L"
                        } else {
                            ???
                        }
                    case _ => ???                    
                }
            }
        }
        
        result
    }

    def rotateRight(dir: Pos): Pos = dir match {
        case Pos(0, 1) => Pos(-1, 0)
        case Pos(-1, 0) => Pos(0, -1)
        case Pos(0, -1) => Pos(1, 0)
        case Pos(1, 0) => Pos(0, 1)
        case _ => ???
    }

    def rotateLeft(dir: Pos): Pos = Function.chain(List.fill(3)(rotateRight _))(dir)

    val puzzleInput = "1,330,331,332,109,4146,1102,1,1182,15,1101,1451,0,24,1002,0,1,570,1006,570,36,102,1,571,0,1001,570,-1,570,1001,24,1,24,1105,1,18,1008,571,0,571,1001,15,1,15,1008,15,1451,570,1006,570,14,21102,58,1,0,1106,0,786,1006,332,62,99,21102,333,1,1,21101,73,0,0,1105,1,579,1101,0,0,572,1102,1,0,573,3,574,101,1,573,573,1007,574,65,570,1005,570,151,107,67,574,570,1005,570,151,1001,574,-64,574,1002,574,-1,574,1001,572,1,572,1007,572,11,570,1006,570,165,101,1182,572,127,1002,574,1,0,3,574,101,1,573,573,1008,574,10,570,1005,570,189,1008,574,44,570,1006,570,158,1105,1,81,21102,1,340,1,1105,1,177,21102,477,1,1,1106,0,177,21102,1,514,1,21101,0,176,0,1105,1,579,99,21101,0,184,0,1106,0,579,4,574,104,10,99,1007,573,22,570,1006,570,165,102,1,572,1182,21102,1,375,1,21102,1,211,0,1106,0,579,21101,1182,11,1,21102,222,1,0,1106,0,979,21102,388,1,1,21101,0,233,0,1106,0,579,21101,1182,22,1,21102,244,1,0,1105,1,979,21102,1,401,1,21101,0,255,0,1106,0,579,21101,1182,33,1,21101,0,266,0,1105,1,979,21102,414,1,1,21101,277,0,0,1105,1,579,3,575,1008,575,89,570,1008,575,121,575,1,575,570,575,3,574,1008,574,10,570,1006,570,291,104,10,21101,1182,0,1,21101,0,313,0,1105,1,622,1005,575,327,1102,1,1,575,21102,327,1,0,1106,0,786,4,438,99,0,1,1,6,77,97,105,110,58,10,33,10,69,120,112,101,99,116,101,100,32,102,117,110,99,116,105,111,110,32,110,97,109,101,32,98,117,116,32,103,111,116,58,32,0,12,70,117,110,99,116,105,111,110,32,65,58,10,12,70,117,110,99,116,105,111,110,32,66,58,10,12,70,117,110,99,116,105,111,110,32,67,58,10,23,67,111,110,116,105,110,117,111,117,115,32,118,105,100,101,111,32,102,101,101,100,63,10,0,37,10,69,120,112,101,99,116,101,100,32,82,44,32,76,44,32,111,114,32,100,105,115,116,97,110,99,101,32,98,117,116,32,103,111,116,58,32,36,10,69,120,112,101,99,116,101,100,32,99,111,109,109,97,32,111,114,32,110,101,119,108,105,110,101,32,98,117,116,32,103,111,116,58,32,43,10,68,101,102,105,110,105,116,105,111,110,115,32,109,97,121,32,98,101,32,97,116,32,109,111,115,116,32,50,48,32,99,104,97,114,97,99,116,101,114,115,33,10,94,62,118,60,0,1,0,-1,-1,0,1,0,0,0,0,0,0,1,12,28,0,109,4,1201,-3,0,587,20101,0,0,-1,22101,1,-3,-3,21101,0,0,-2,2208,-2,-1,570,1005,570,617,2201,-3,-2,609,4,0,21201,-2,1,-2,1106,0,597,109,-4,2105,1,0,109,5,2102,1,-4,630,20101,0,0,-2,22101,1,-4,-4,21101,0,0,-3,2208,-3,-2,570,1005,570,781,2201,-4,-3,652,21002,0,1,-1,1208,-1,-4,570,1005,570,709,1208,-1,-5,570,1005,570,734,1207,-1,0,570,1005,570,759,1206,-1,774,1001,578,562,684,1,0,576,576,1001,578,566,692,1,0,577,577,21102,1,702,0,1105,1,786,21201,-1,-1,-1,1105,1,676,1001,578,1,578,1008,578,4,570,1006,570,724,1001,578,-4,578,21102,731,1,0,1106,0,786,1105,1,774,1001,578,-1,578,1008,578,-1,570,1006,570,749,1001,578,4,578,21101,0,756,0,1106,0,786,1106,0,774,21202,-1,-11,1,22101,1182,1,1,21101,0,774,0,1105,1,622,21201,-3,1,-3,1106,0,640,109,-5,2106,0,0,109,7,1005,575,802,21001,576,0,-6,21001,577,0,-5,1106,0,814,21101,0,0,-1,21101,0,0,-5,21101,0,0,-6,20208,-6,576,-2,208,-5,577,570,22002,570,-2,-2,21202,-5,55,-3,22201,-6,-3,-3,22101,1451,-3,-3,1201,-3,0,843,1005,0,863,21202,-2,42,-4,22101,46,-4,-4,1206,-2,924,21101,1,0,-1,1105,1,924,1205,-2,873,21101,0,35,-4,1105,1,924,1201,-3,0,878,1008,0,1,570,1006,570,916,1001,374,1,374,2102,1,-3,895,1102,1,2,0,2101,0,-3,902,1001,438,0,438,2202,-6,-5,570,1,570,374,570,1,570,438,438,1001,578,558,922,20102,1,0,-4,1006,575,959,204,-4,22101,1,-6,-6,1208,-6,55,570,1006,570,814,104,10,22101,1,-5,-5,1208,-5,49,570,1006,570,810,104,10,1206,-1,974,99,1206,-1,974,1102,1,1,575,21102,1,973,0,1106,0,786,99,109,-7,2106,0,0,109,6,21101,0,0,-4,21101,0,0,-3,203,-2,22101,1,-3,-3,21208,-2,82,-1,1205,-1,1030,21208,-2,76,-1,1205,-1,1037,21207,-2,48,-1,1205,-1,1124,22107,57,-2,-1,1205,-1,1124,21201,-2,-48,-2,1105,1,1041,21101,-4,0,-2,1106,0,1041,21102,1,-5,-2,21201,-4,1,-4,21207,-4,11,-1,1206,-1,1138,2201,-5,-4,1059,2102,1,-2,0,203,-2,22101,1,-3,-3,21207,-2,48,-1,1205,-1,1107,22107,57,-2,-1,1205,-1,1107,21201,-2,-48,-2,2201,-5,-4,1090,20102,10,0,-1,22201,-2,-1,-2,2201,-5,-4,1103,1202,-2,1,0,1105,1,1060,21208,-2,10,-1,1205,-1,1162,21208,-2,44,-1,1206,-1,1131,1106,0,989,21101,439,0,1,1105,1,1150,21102,1,477,1,1106,0,1150,21102,1,514,1,21102,1,1149,0,1106,0,579,99,21102,1,1157,0,1106,0,579,204,-2,104,10,99,21207,-3,22,-1,1206,-1,1138,1202,-5,1,1176,2102,1,-4,0,109,-6,2106,0,0,10,9,46,1,7,1,46,1,7,1,46,1,7,1,46,1,7,1,46,1,7,1,46,1,7,1,46,1,7,1,42,5,7,1,42,1,11,1,42,1,11,1,42,1,11,1,40,1,1,1,5,7,40,1,1,1,5,1,46,1,1,1,5,1,46,1,1,1,5,1,46,1,1,1,5,1,46,1,1,1,5,1,46,13,44,1,5,1,3,1,44,13,48,1,3,1,1,1,48,1,3,13,38,1,5,1,9,1,34,5,5,1,9,1,5,13,16,1,9,1,9,1,5,1,11,1,16,1,9,13,3,1,11,1,16,1,19,1,1,1,3,1,11,1,8,13,15,13,5,10,7,1,21,1,3,1,5,1,13,2,7,1,21,13,11,2,7,1,25,1,5,1,1,1,11,2,7,1,25,1,5,1,1,1,11,2,7,1,25,1,5,1,1,1,11,2,7,1,25,1,5,1,1,1,11,2,7,1,25,1,5,1,1,1,11,10,19,7,5,1,1,13,28,1,11,1,42,1,11,1,42,1,11,1,42,1,7,5,42,1,7,1,46,1,7,1,46,1,7,1,46,1,7,1,46,1,7,1,46,1,7,1,46,1,7,1,46,9,18"

    /*
    In my case I can get a path like this (just walk forward as long as possible, then turn).
    L12,L8,L8,L12,R4,L12,R6,L12,L8,L8,R4,L12,L12,R6,L12,R4,L12,R6,L12,L8,L8,R4,L12,L12,R6,L12,L8,L8,R4,L12,L12,R6,L12,R4,L12,R6

    I did the compression manually:

    L12,L8,L8, L12,R4,L12,R6, L12,L8,L8, R4,L12,L12,R6, L12,R4,L12,R6, L12,L8,L8, R4,L12,L12,R6, L12,L8,L8, R4,L12,L12,R6, L12,R4,L12,R6

    Main = A,B,A,C,B,A,C,A,C,B
    A = L12,L8,L8
    B = L12,R4,L12,R6
    C = R4,L12,L12,R6
    */

}