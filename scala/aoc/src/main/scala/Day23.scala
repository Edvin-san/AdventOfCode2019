object Day23 {
	import IntCodeComputer._

	case class Packet(addressTo: Int, x: BigInt, y: BigInt)

	trait PacketHandler {
		val packetArgs = 2
		var packetParts: List[BigInt] = Nil
		var address: Option[Int] = None

		def addPacketPart(part: BigInt): Unit = {
			if (!address.isDefined) 
				address = Some(part.toInt)
			else
				packetParts = packetParts :+ part
		}

		def consumePacket(): Option[Packet] = 
			address match {
				case Some(a) if packetParts.size == packetArgs =>
					val x = packetParts.head
					val y = packetParts.last
					address = None
					packetParts = Nil
					Some(Packet(a, x, y))
				case _ => None
			}
	}

	trait PacketIOCounter {
		var emptyReceives = 0
		def conseqEmptyReceives: Int = emptyReceives
		def sendPacket(): Unit = emptyReceives = 0
		def receivePacket(): Unit = emptyReceives = 0
		def receiveEmpty(): Unit = emptyReceives += 1
	}

	class Network(val numComputers: Int, val networkCode: String, val natAddress: Int) {
		var computers = (0 until numComputers)
			.map(i => {
				val computer = new IntCodeComputer(i.toString, networkCode) with PacketHandler with PacketIOCounter
				computer.addInputs(List(BigInt(i)))
				computer
			}).toArray

		def simulateOneTick(): List[Packet] = {
			for (computer <- computers) {
				computer.runOneInstruction() match {
					case Some(output) =>
						computer.addPacketPart(output)
					case None => None
				}

				if (computer.status == WaitingForInput) {
					computer.addInputs(List(BigInt(-1)))
					computer.runOneInstruction()
					computer.receiveEmpty()
				}
			}

			var packets: List[Packet] = Nil
			for (computer <- computers) {
				computer.consumePacket() match {
					case Some(Packet(address, x, y)) =>
						packets = packets :+ Packet(address, x, y)
						computer.sendPacket()
						if (address < numComputers) {
							computers(address).addInputs(List(x, y))
							computers(address).receivePacket()
						}
						println(s"Sending from ${computer.id} value $x $y to $address")
					case _ =>
				}
			}
			packets
		}

		def natCycle(initPacketForAddress0: Option[Packet]): Option[Packet] = {
			initPacketForAddress0 match {
				case None =>
				case Some(packetFor0) => 
					computers(0).addInputs(List(packetFor0.x, packetFor0.y))
					computers(0).receivePacket()
			}

			var natPacket: Option[Packet] = None

			def isNetworkIdle = computers.forall(c => c.conseqEmptyReceives >= 2)

			while (!isNetworkIdle) {
				simulateOneTick().filter(p => p.addressTo == natAddress).headOption match {
					case None => 
					case packetToNat => natPacket = packetToNat 
				}
			}
			println(s"NAT sends $natPacket to address 0")
			natPacket
		}

		def natPacketStream(initPacket: Option[Packet]): Stream[Option[Packet]] = initPacket #:: natPacketStream(natCycle(initPacket))
	}

	def part1(s: String = puzzleInput) = {
		val network = new Network(50, s, 255)
		var packetsTo255: List[Packet] = Nil
		while (packetsTo255.size < 1) {
			packetsTo255 ++= network.simulateOneTick().filter(packet => packet.addressTo == 255)
		}
		packetsTo255
	}

	def part2(s: String = puzzleInput) = {
		val network = new Network(50, s, 255)
		def firstDupY(packetStream: Stream[Option[Packet]], seenY: Set[BigInt] = Set.empty[BigInt]): Option[BigInt] = packetStream match {
			case Some(packet) #:: tail if seenY(packet.y) => Some(packet.y)
			case Some(packet) #:: tail => firstDupY(tail, seenY + packet.y)
			case None #:: tail => firstDupY(tail, seenY)
			case _ => None
		}

		val y = firstDupY(network.natPacketStream(None))
		y
	}
	
	val puzzleInput = "3,62,1001,62,11,10,109,2219,105,1,0,1916,2159,891,1246,1623,794,1724,1380,1312,965,1025,1056,1279,1691,631,1759,2027,730,1792,1415,1510,1151,1555,1211,1089,1823,932,1481,1588,763,1658,1957,1120,829,1994,699,994,666,1182,1452,2188,2056,602,1852,860,571,1347,2097,1883,2128,0,0,0,0,0,0,0,0,0,0,0,0,3,64,1008,64,-1,62,1006,62,88,1006,61,170,1105,1,73,3,65,20102,1,64,1,21001,66,0,2,21101,0,105,0,1105,1,436,1201,1,-1,64,1007,64,0,62,1005,62,73,7,64,67,62,1006,62,73,1002,64,2,133,1,133,68,133,101,0,0,62,1001,133,1,140,8,0,65,63,2,63,62,62,1005,62,73,1002,64,2,161,1,161,68,161,1102,1,1,0,1001,161,1,169,1002,65,1,0,1101,1,0,61,1101,0,0,63,7,63,67,62,1006,62,203,1002,63,2,194,1,68,194,194,1006,0,73,1001,63,1,63,1105,1,178,21101,0,210,0,106,0,69,1201,1,0,70,1102,1,0,63,7,63,71,62,1006,62,250,1002,63,2,234,1,72,234,234,4,0,101,1,234,240,4,0,4,70,1001,63,1,63,1105,1,218,1105,1,73,109,4,21102,0,1,-3,21101,0,0,-2,20207,-2,67,-1,1206,-1,293,1202,-2,2,283,101,1,283,283,1,68,283,283,22001,0,-3,-3,21201,-2,1,-2,1106,0,263,22101,0,-3,-3,109,-4,2105,1,0,109,4,21101,1,0,-3,21102,0,1,-2,20207,-2,67,-1,1206,-1,342,1202,-2,2,332,101,1,332,332,1,68,332,332,22002,0,-3,-3,21201,-2,1,-2,1105,1,312,21201,-3,0,-3,109,-4,2105,1,0,109,1,101,1,68,359,20102,1,0,1,101,3,68,366,21002,0,1,2,21101,0,376,0,1105,1,436,22101,0,1,0,109,-1,2105,1,0,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,8388608,16777216,33554432,67108864,134217728,268435456,536870912,1073741824,2147483648,4294967296,8589934592,17179869184,34359738368,68719476736,137438953472,274877906944,549755813888,1099511627776,2199023255552,4398046511104,8796093022208,17592186044416,35184372088832,70368744177664,140737488355328,281474976710656,562949953421312,1125899906842624,109,8,21202,-6,10,-5,22207,-7,-5,-5,1205,-5,521,21102,0,1,-4,21101,0,0,-3,21101,0,51,-2,21201,-2,-1,-2,1201,-2,385,470,21002,0,1,-1,21202,-3,2,-3,22207,-7,-1,-5,1205,-5,496,21201,-3,1,-3,22102,-1,-1,-5,22201,-7,-5,-7,22207,-3,-6,-5,1205,-5,515,22102,-1,-6,-5,22201,-3,-5,-3,22201,-1,-4,-4,1205,-2,461,1105,1,547,21102,1,-1,-4,21202,-6,-1,-6,21207,-7,0,-5,1205,-5,547,22201,-7,-6,-7,21201,-4,1,-4,1106,0,529,22102,1,-4,-7,109,-8,2106,0,0,109,1,101,1,68,564,20101,0,0,0,109,-1,2105,1,0,1102,1,151,66,1101,1,0,67,1101,0,598,68,1101,0,556,69,1102,1,1,71,1101,600,0,72,1106,0,73,1,3,17,29671,1101,0,52981,66,1101,1,0,67,1101,629,0,68,1102,556,1,69,1102,1,0,71,1102,1,631,72,1106,0,73,1,1639,1101,0,97771,66,1101,0,3,67,1101,658,0,68,1101,253,0,69,1101,0,1,71,1102,1,664,72,1105,1,73,0,0,0,0,0,0,46,54377,1101,0,27091,66,1101,1,0,67,1101,693,0,68,1102,1,556,69,1101,0,2,71,1101,695,0,72,1106,0,73,1,2,2,93305,2,111966,1102,64879,1,66,1101,0,1,67,1101,0,726,68,1102,1,556,69,1102,1,1,71,1101,0,728,72,1105,1,73,1,157651,41,1877,1102,1,29671,66,1102,2,1,67,1101,757,0,68,1101,302,0,69,1102,1,1,71,1102,1,761,72,1106,0,73,0,0,0,0,14,97771,1101,0,25951,66,1102,1,1,67,1101,0,790,68,1101,0,556,69,1101,1,0,71,1101,0,792,72,1106,0,73,1,35246,41,5631,1102,1,50087,66,1102,3,1,67,1102,1,821,68,1102,1,302,69,1101,0,1,71,1101,827,0,72,1105,1,73,0,0,0,0,0,0,23,200986,1102,1,39079,66,1101,1,0,67,1101,0,856,68,1102,556,1,69,1101,0,1,71,1102,1,858,72,1106,0,73,1,-17,4,10253,1102,7951,1,66,1101,0,1,67,1102,887,1,68,1101,556,0,69,1101,1,0,71,1101,0,889,72,1105,1,73,1,-42282,41,7508,1102,1,18661,66,1101,0,6,67,1101,918,0,68,1102,1,302,69,1102,1,1,71,1102,1,930,72,1106,0,73,0,0,0,0,0,0,0,0,0,0,0,0,34,35318,1102,1,85091,66,1102,2,1,67,1102,1,959,68,1102,302,1,69,1101,1,0,71,1101,963,0,72,1105,1,73,0,0,0,0,12,9187,1101,6151,0,66,1102,1,1,67,1102,992,1,68,1101,0,556,69,1102,1,0,71,1102,1,994,72,1105,1,73,1,1375,1102,1,88379,66,1101,1,0,67,1101,0,1021,68,1101,556,0,69,1101,1,0,71,1101,0,1023,72,1105,1,73,1,4397,4,30759,1102,101653,1,66,1101,0,1,67,1102,1,1052,68,1102,556,1,69,1101,0,1,71,1101,1054,0,72,1105,1,73,1,63072,41,11262,1101,0,95989,66,1101,2,0,67,1102,1,1083,68,1102,302,1,69,1102,1,1,71,1102,1,1087,72,1105,1,73,0,0,0,0,31,289516,1102,1,61871,66,1102,1,1,67,1101,1116,0,68,1101,556,0,69,1101,0,1,71,1102,1118,1,72,1106,0,73,1,-75,5,150261,1101,0,77773,66,1102,1,1,67,1102,1147,1,68,1101,0,556,69,1102,1,1,71,1101,1149,0,72,1106,0,73,1,8669,7,7879,1102,62873,1,66,1102,1,1,67,1101,0,1178,68,1102,1,556,69,1101,1,0,71,1101,0,1180,72,1105,1,73,1,160,2,74644,1101,0,29077,66,1101,0,1,67,1102,1209,1,68,1102,1,556,69,1102,1,0,71,1101,0,1211,72,1106,0,73,1,1443,1101,100493,0,66,1102,3,1,67,1101,0,1238,68,1102,302,1,69,1101,1,0,71,1102,1244,1,72,1106,0,73,0,0,0,0,0,0,31,217137,1101,3067,0,66,1102,1,1,67,1101,1273,0,68,1101,0,556,69,1101,0,2,71,1102,1,1275,72,1105,1,73,1,10,19,213116,2,18661,1102,9187,1,66,1101,0,2,67,1101,1306,0,68,1102,302,1,69,1101,0,1,71,1101,0,1310,72,1106,0,73,0,0,0,0,13,163342,1101,17551,0,66,1101,0,3,67,1102,1339,1,68,1101,0,302,69,1101,0,1,71,1101,1345,0,72,1105,1,73,0,0,0,0,0,0,31,72379,1102,1,54377,66,1102,2,1,67,1102,1,1374,68,1101,0,302,69,1102,1,1,71,1101,1378,0,72,1105,1,73,0,0,0,0,48,11483,1102,1,7879,66,1102,1,3,67,1101,1407,0,68,1101,0,302,69,1101,0,1,71,1101,1413,0,72,1106,0,73,0,0,0,0,0,0,14,293313,1102,53279,1,66,1102,1,4,67,1102,1442,1,68,1102,302,1,69,1102,1,1,71,1101,0,1450,72,1105,1,73,0,0,0,0,0,0,0,0,2,55983,1101,24571,0,66,1102,1,1,67,1102,1,1479,68,1102,1,556,69,1102,1,0,71,1102,1481,1,72,1105,1,73,1,1215,1102,1,101797,66,1102,1,1,67,1101,1508,0,68,1101,556,0,69,1101,0,0,71,1102,1510,1,72,1106,0,73,1,1934,1102,1,4079,66,1102,1,1,67,1101,0,1537,68,1102,556,1,69,1101,0,8,71,1101,0,1539,72,1106,0,73,1,5,12,18374,13,81671,8,52653,48,22966,15,87793,19,53279,19,106558,2,37322,1101,0,50957,66,1102,1,1,67,1101,0,1582,68,1102,1,556,69,1101,2,0,71,1102,1,1584,72,1105,1,73,1,499,8,35102,5,100174,1102,57383,1,66,1101,1,0,67,1102,1615,1,68,1102,556,1,69,1102,1,3,71,1102,1617,1,72,1106,0,73,1,1,7,15758,4,20506,5,50087,1101,10253,0,66,1102,1,3,67,1101,0,1650,68,1101,0,302,69,1102,1,1,71,1101,1656,0,72,1105,1,73,0,0,0,0,0,0,14,195542,1101,61051,0,66,1102,1,1,67,1101,0,1685,68,1101,0,556,69,1102,1,2,71,1101,1687,0,72,1105,1,73,1,125,26,170182,19,159837,1102,1,81671,66,1101,2,0,67,1102,1718,1,68,1101,302,0,69,1102,1,1,71,1102,1722,1,72,1105,1,73,0,0,0,0,8,17551,1101,26489,0,66,1102,3,1,67,1102,1,1751,68,1101,302,0,69,1102,1,1,71,1101,0,1757,72,1105,1,73,0,0,0,0,0,0,31,144758,1102,1,87793,66,1102,1,2,67,1101,0,1786,68,1101,0,302,69,1101,0,1,71,1101,1790,0,72,1106,0,73,0,0,0,0,11,95989,1102,1,84263,66,1102,1,1,67,1101,1819,0,68,1101,0,556,69,1101,0,1,71,1102,1,1821,72,1106,0,73,1,25,46,108754,1101,73679,0,66,1102,1,1,67,1102,1,1850,68,1102,1,556,69,1101,0,0,71,1101,1852,0,72,1106,0,73,1,1410,1101,65071,0,66,1102,1,1,67,1102,1879,1,68,1101,556,0,69,1102,1,1,71,1102,1881,1,72,1105,1,73,1,453643,17,59342,1101,0,11483,66,1101,2,0,67,1102,1,1910,68,1102,1,302,69,1101,1,0,71,1102,1,1914,72,1106,0,73,0,0,0,0,15,175586,1101,0,77687,66,1102,1,1,67,1101,0,1943,68,1102,1,556,69,1102,6,1,71,1101,0,1945,72,1106,0,73,1,21443,11,191978,23,100493,23,301479,6,26489,6,52978,6,79467,1101,72379,0,66,1101,4,0,67,1102,1984,1,68,1102,253,1,69,1101,0,1,71,1102,1,1992,72,1106,0,73,0,0,0,0,0,0,0,0,34,17659,1102,17659,1,66,1101,0,2,67,1102,1,2021,68,1101,351,0,69,1101,0,1,71,1101,2025,0,72,1105,1,73,0,0,0,0,255,77687,1102,1,48883,66,1102,1,1,67,1102,2054,1,68,1102,556,1,69,1101,0,0,71,1101,2056,0,72,1105,1,73,1,1591,1102,1,1877,66,1102,1,6,67,1101,0,2083,68,1101,0,253,69,1102,1,1,71,1101,0,2095,72,1106,0,73,0,0,0,0,0,0,0,0,0,0,0,0,26,85091,1102,599,1,66,1102,1,1,67,1101,0,2124,68,1101,556,0,69,1101,1,0,71,1102,2126,1,72,1106,0,73,1,51674,41,9385,1102,1,76159,66,1101,1,0,67,1101,2155,0,68,1101,556,0,69,1102,1,1,71,1102,2157,1,72,1105,1,73,1,-34362,41,3754,1102,23789,1,66,1101,1,0,67,1101,2186,0,68,1102,556,1,69,1101,0,0,71,1102,2188,1,72,1105,1,73,1,1927,1102,1,58151,66,1102,1,1,67,1101,2215,0,68,1101,0,556,69,1102,1,1,71,1101,2217,0,72,1106,0,73,1,67,7,23637"	
}