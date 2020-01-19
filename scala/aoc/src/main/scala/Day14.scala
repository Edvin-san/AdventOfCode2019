object Day14 {
	import Util.lowerBound
	// You can sometimes throw a binary search on another problem.

	case class Product(id: String, amount: BigInt)

	case class OreCalculator(val recipes: Map[Product, Seq[Product]]) {
		case class OreResult(requiredOre: BigInt, excessProduct: Map[String, BigInt])

		private def union(map1: Map[String, BigInt], map2: Map[String, BigInt]): Map[String, BigInt] = 
			dropEmpty((map1.toSeq ++ map2.toSeq).groupBy(_._1).view.mapValues(l => l.map(_._2).sum).toMap)

		private def dropEmpty(map: Map[String, BigInt]) = map.filter(_._2 != 0)

		def ore(p: Product, availableProduct: Map[String, BigInt], tabs: Int = 0): OreResult = p match {
			case Product(id, requestedAmount) => recipes.find(p2 => p2._1.id == id) match {
					case Some((Product(_, reactionAmount), requirements)) =>
						val availableAmount: BigInt = availableProduct.get(id).getOrElse(0)
						val consumed = availableAmount.min(requestedAmount)
						val consumedProduct = dropEmpty(Map(id -> -consumed))
						val actualRequestedAmount = requestedAmount - consumed

						val multiplier = (actualRequestedAmount + reactionAmount - 1) / reactionAmount
						val order = multiplier*reactionAmount

						var excess = union(availableProduct, consumedProduct)
						var requiredOre: BigInt = 0
						for (requirement <- requirements) {
							val result = ore(Product(requirement.id, requirement.amount*multiplier), excess, tabs + 1)
							requiredOre += result.requiredOre
							excess = result.excessProduct
						}
						excess = union(excess, Map(id -> (order - actualRequestedAmount)))
						OreResult(requiredOre, excess)
					case None => 
						// Should never be able to reuse ORE
						OreResult(requestedAmount, availableProduct)
				}
			}
	}

	def part1(s: String = puzzleInput) = {
		val recipes = parseRecipes(s)
		val calc = OreCalculator(recipes)
		calc.ore(Product("FUEL", 1), Map())
	}

	def part2(s: String = puzzleInput) = {
		val recipes = parseRecipes(s)
		val calc = OreCalculator(recipes)
		val oreLimit = BigInt("1000000000000")
		def valueOf(i: BigInt): Int = if (calc.ore(Product("FUEL", i), Map()).requiredOre <= oreLimit) 0 else 1
		lowerBound(0, oreLimit + 1, 1, valueOf) - 1
	}

	// Demo binary search.
	def oreRequiredToProduceXFuel(s: String, fuel: BigInt) = {
		val oreLimit = BigInt("1000000000000")
		val recipes = parseRecipes(s)
		val calc = OreCalculator(recipes)
		val reqOre = calc.ore(Product("FUEL", fuel), Map()).requiredOre
		println(s"You require $reqOre ore to produce $fuel fuel.")
		if (reqOre > oreLimit) println(s"TOO MUCH This is ${reqOre - oreLimit} more than the amount of ore you have available!!")
		else if (reqOre < oreLimit) println(s"TOO LITTLE You have ${oreLimit - reqOre} more ore available!")
		else println("You used up all ore!")
		println
	}

	def requiredOre(s: String = puzzleInput, requestedFuel: BigInt) = {
		val recipes = parseRecipes(s)
		val calc = OreCalculator(recipes)
		calc.ore(Product("FUEL", requestedFuel), Map())
	}

	def testPart1: Unit = {
		val testcases = Map("31" -> s31,
							"165" -> s165,
							"13312" -> s13312,
							"180697" -> s180697,
							"2210736" -> s2210736)
		val tested = testcases.map {
			case (id, input) => (BigInt(id), part1(input).requiredOre)
		}

		for ((expected, actual) <- tested) {
			if (expected == actual) println(expected.toString + " passed!")
			else println("Fail! Expected " + expected + " but got " + actual)
		}
	}

	def parseProduct(s: String): Product = s.trim.split(" ") match {
		case Array(amount, id) => Product(id, BigInt(amount))
	}

	def parseRecipes(s: String): Map[Product, Seq[Product]] = {
		val x = s.trim.split("\r\n").map(line => 
			line.split("=>") match {
				case Array(requirements, result) => (parseProduct(result), requirements.split(",").map(parseProduct).toSeq)
			})
		//println(x.groupBy(_._1).filter(_._2.size > 1))
		x.toMap
	}

	val s31 = 
	"""
	10 ORE => 10 A
	1 ORE => 1 B
	7 A, 1 B => 1 C
	7 A, 1 C => 1 D
	7 A, 1 D => 1 E
	7 A, 1 E => 1 FUEL
	"""

	val s165 = 
	"""
	9 ORE => 2 A
	8 ORE => 3 B
	7 ORE => 5 C
	3 A, 4 B => 1 AB
	5 B, 7 C => 1 BC
	4 C, 1 A => 1 CA
	2 AB, 3 BC, 4 CA => 1 FUEL
	"""

	val s13312 = 
	"""
	157 ORE => 5 NZVS
	165 ORE => 6 DCFZ
	44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
	12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
	179 ORE => 7 PSHF
	177 ORE => 5 HKGWZ
	7 DCFZ, 7 PSHF => 2 XJWVT
	165 ORE => 2 GPVTF
	3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
	"""

	val s180697 =
	"""
	2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
	17 NVRVD, 3 JNWZP => 8 VPVL
	53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
	22 VJHF, 37 MNCFX => 5 FWMGM
	139 ORE => 4 NVRVD
	144 ORE => 7 JNWZP
	5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
	5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
	145 ORE => 6 MNCFX
	1 NVRVD => 8 CXFTF
	1 VJHF, 6 MNCFX => 4 RFSQX
	176 ORE => 6 VJHF
	"""

	val s2210736 = 
	"""
	171 ORE => 8 CNZTR
	7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
	114 ORE => 4 BHXH
	14 VRPVC => 6 BMBT
	6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
	6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
	15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
	13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
	5 BMBT => 4 WPTQ
	189 ORE => 9 KTJDG
	1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
	12 VRPVC, 27 CNZTR => 2 XDBXC
	15 KTJDG, 12 BHXH => 5 XCVML
	3 BHXH, 2 VRPVC => 7 MZWV
	121 ORE => 7 VRPVC
	7 XCVML => 6 RJRHP
	5 BHXH, 4 VRPVC => 5 LTCX
	"""

	val puzzleInput = 
	"""
	1 HVXJL, 1 JHGQ => 2 ZQFQ
	6 GRQTX => 6 VZWRS
	128 ORE => 2 GRQTX
	1 MJPSW => 4 MGZBH
	3 HLQX => 8 KSMW
	4 QLNS => 9 LFRW
	10 HBCN => 3 CZWP
	1 CQRJP => 9 MJPSW
	1 SLXC => 6 SDTGP
	1 MTGVK => 4 NZWLQ
	4 PMJX => 3 CVKM
	2 LDKGL, 2 SFKF => 5 XZDV
	1 QLNS, 1 VZWRS => 5 RSBT
	1 NRQS, 22 LQFDM => 4 PMJX
	17 XZDV, 8 GSRKQ => 3 ZGDC
	11 BPJLM, 18 ZGDC, 1 JHGQ => 5 BXNJX
	2 GRQTX, 1 CQRJP => 7 NRQS
	1 LJTL => 7 DBHXK
	15 HPBQ, 5 PSPCF, 1 JHGQ, 25 ZMXWG, 1 JTZS, 1 SDTGP, 3 NLBM => 6 MQVLS
	9 KSMW => 2 GXTBV
	3 HVXJL => 5 JHGQ
	1 ZWXT, 13 MJPSW, 10 HVXJL => 5 LDKGL
	1 GRQTX => 2 LQFDM
	190 ORE => 5 FQPNW
	1 GTQB => 9 HVHN
	1 TNLN, 9 HVHN, 1 WLGT, 4 NZMZ, 2 QTPC, 1 LPTF => 7 WFCH
	3 PMJX => 5 SFKF
	1 ZGDC => 9 HTVR
	193 ORE => 1 CQRJP
	1 BPJLM, 1 HPBQ, 3 HVHN => 6 NLBM
	2 SFKF => 1 GSRKQ
	1 ZGDC => 8 GTQB
	1 LSPMR, 53 LDKGL, 24 WFCH, 32 GDLH, 2 HLQX, 14 NLBM, 18 BDZK, 7 MDSRW, 9 MQVLS => 1 FUEL
	12 SFKF => 7 NZMZ
	13 PVJM => 3 XBTH
	7 GSRKQ, 7 LPTF, 1 HLQX, 1 FJHK, 1 DHVM, 3 SFKF, 15 NLBM, 2 SDTGP => 3 LSPMR
	4 LFRW, 28 MJPSW => 4 GDLH
	6 VZWRS, 8 MJPSW => 8 HVXJL
	13 LFRW => 4 ZWQW
	1 LQFDM, 7 NZWLQ, 2 HVXJL => 4 HLQX
	2 KSMW, 1 WDGN, 4 ZQFQ => 1 ZMXWG
	3 MGZBH => 2 LPTF
	1 LFRW, 1 CVKM, 3 LDKGL => 4 LJTL
	3 LJTL, 20 CZWP, 1 HPBQ => 9 WLGT
	3 FQPNW => 8 MTGVK
	1 MTDWJ, 1 CVKM => 9 WDGN
	5 ZWQW => 3 MTDWJ
	2 CVKM => 8 QTPC
	2 PVJM, 9 ZWQW, 1 MTDWJ => 4 HBCN
	5 RSBT, 2 WDGN, 6 GSRKQ => 1 BPJLM
	34 JHGQ, 6 ZGDC => 8 DHVM
	3 QTPC, 1 RSBT, 1 GXTBV => 9 JTZS
	1 BXNJX, 2 JTZS => 5 SLXC
	23 LPTF, 2 NZMZ => 4 TNLN
	24 HTVR, 5 DBHXK => 2 FJHK
	5 LPTF, 5 QTPC => 4 PSPCF
	17 MTGVK, 27 LQFDM => 4 QLNS
	1 CVKM, 5 HTVR => 8 HPBQ
	6 ZQFQ, 28 XBTH => 7 MDSRW
	13 WDGN => 5 BDZK
	1 MJPSW, 2 VZWRS => 4 ZWXT
	1 MGZBH, 1 GRQTX => 8 PVJM
	"""
}