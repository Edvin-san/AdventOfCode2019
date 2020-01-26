object Day22Mathy {
    import Day22._
    import Util._

    // Linear Congruential Generator.
    // f(x) = a*x + b mod m
    case class LCG(m: BigInt, a: BigInt, b: BigInt) {
        def apply(x: BigInt): BigInt = posMod(a*x + b, m)
        def unapply(y: BigInt): BigInt = posMod(modularInverse(a, m)*y - b, m)
    }

    // a1*(a2*x + b2) + b1 = (a1*a2)*x + a1*b2 + (b2 + b1).
    def compose(g1: LCG, g2: LCG): LCG = (g1, g2) match {
        case (LCG(m, a1, b1), LCG(m2, a2, b2)) if m == m2 =>
            LCG(m, posMod(a1*a2, m), posMod(a1*b2 + b1 + b2, m))
        case _ => ???
    }

    // Compose g with itself r times.
    // a*(a*x + b) + b = a^2*x + a*b + b
    // a*(a^2*x + a*b + b) + b = a^3*x + a^2*b + a*b + b
    // a*(a^3*x + a^2*b + a*b + b) + b = a^4*x + a^3*b + a^2*b + a*b + b
    // Geometric sum
    // S_{n} = b + b*a + b*a^2 + ... b*a^(n-1) = b*(a^n - 1)/(a - 1)
    // = b*(a^n - 1)*multiplicativeInverse(a - 1, m)
    // r*f(x) = S_{r} + a^r*x
    // 1*f(x) = b + a*x
    def repeat(g: LCG, r: BigInt): LCG = g match {
        case LCG(m, a, b) =>
            LCG(m,
            a.modPow(r, m), 
            posMod(b*(a.modPow(r, m) - 1) * modularInverse(a - 1, m), m))
    }

    def opToLCG(m: BigInt)(op: ShuffleOperation): LCG = op match {
        case DealNewStack => LCG(m, m - 1, m - 1)
        case Cut(n) => LCG(m, 1, -n)
        case DealWithIncrement(inc) => LCG(m, inc, 0)
    }

    def part1(s: String = Day22.puzzleInput) = {
        val cards = BigInt("10007")
        val ops = s.linesIterator.map(parseOperation).toList
        val toLCG = opToLCG(cards) _
        val op = ops.map(toLCG).reverse.reduce(compose _)
        op(2019)
    }

    def part2(s: String = Day22.puzzleInput) = {
        val cards = BigInt("119315717514047")
        val repetitions = BigInt("101741582076661")
        val ops = s.linesIterator.map(parseOperation).toList
        val toLCG = opToLCG(cards) _
        val op = ops.map(toLCG).reverse.reduce(compose _)
        val repOp = repeat(op, repetitions)
        repOp.unapply(2020)
    }
    
}