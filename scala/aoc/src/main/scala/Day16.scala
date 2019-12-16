object Day16 {

    def part1(s: String = puzzleInput, pattern: List[Int] = puzzlePattern): String = {
        val phases = phaseChain(pattern, s)
        phases(100)
    }

    def phaseChain(pattern: List[Int], input: String): Stream[String] = input #:: phaseChain(pattern, phase(pattern)(input))

    def phase(pattern: List[Int])(inputs: String): String = {
        val outputDigits = for (i <- 1 to inputs.size) yield {
            val parts = inputs.zip(repeated(pattern, i).drop(1).take(inputs.size)).map {
                case (in, p) => in.asDigit * p
            }
            parts.sum.toString.last
        }
        outputDigits.mkString
    }

    def repeated(pattern: List[Int], eachCharRepetition: Int): Stream[Int] = pattern.flatMap(List.fill(eachCharRepetition)(_)).toStream #::: repeated(pattern, eachCharRepetition)

    /*
    Okay... so not sure about the general case. There is a pattern between phases.
    In the example we had:
    12345678 initial
    48226158 after 1 phase
    34040438 after 2 phases
    03415518 after 3 phases
    01029498 after 4 phases

    Notice that the last digit is always the same. If we ignore the first half we get:
    ...5678
    ...6158
    ...0438
    ...5518
    ...9498

    The pattern here is a reversed cumulative sum % 10.
    E.g. 5678 is 
    8765 reversed
    The cumulative sum of this is
    8, 8 + 7, 8 + 7 + 6, 8 + 7 + 6 + 5 =
    8, 15, 21, 26    
    using %10 we get
    8, 5, 1, 6 which is 
    6158 reversed.

    For my input, the offset means that the message is in the second half of the digits. 
    This means that I can use the pattern above to compute the relevant digits after each phase.
    */
    def part2(s: String = puzzleInput): String = {
        val size = s.size*10000
        val offset = s.take(7).toInt
        if (offset < size / 2) ??? // Not sure about this case

        val numInterestingDigits = size - offset
        val numRepetitions = (numInterestingDigits + s.size - 1) / s.size // Ceil numInterestingDigit / s.size
        val s2 = (s*numRepetitions).drop(numRepetitions*s.size - numInterestingDigits)
        val arr = s2.map(_.asDigit).toArray
        for (_ <- 1 to 100) {
            for (i <- arr.size - 2 to 0 by -1) {
                arr(i) = (arr(i) + arr(i + 1)) % 10
            }
        }

        arr.take(8).mkString
    }

    val puzzlePattern = List(0, 1, 0, -1)
    val puzzleInput = "59790132880344516900093091154955597199863490073342910249565395038806135885706290664499164028251508292041959926849162473699550018653393834944216172810195882161876866188294352485183178740261279280213486011018791012560046012995409807741782162189252951939029564062935408459914894373210511494699108265315264830173403743547300700976944780004513514866386570658448247527151658945604790687693036691590606045331434271899594734825392560698221510565391059565109571638751133487824774572142934078485772422422132834305704887084146829228294925039109858598295988853017494057928948890390543290199918610303090142501490713145935617325806587528883833726972378426243439037"

}