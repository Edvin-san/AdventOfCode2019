object IntCodeComputer {
	import IntCodeProgram._

	sealed trait Param {
		def getValue(program: MutableProgram): BigInt
	}
	sealed trait PositionBasedParam extends Param {
		def index(program: MutableProgram): BigInt
	}
	case class ImmediateParam(value: BigInt) extends Param {
		override def getValue(program: MutableProgram) = value
	}
	case class PositionParam(i: BigInt) extends PositionBasedParam{
		override def getValue(program: MutableProgram) = program(i)
		override def index(program: MutableProgram) = i
	}
	case class RelativeParam(i: BigInt) extends PositionBasedParam {
		override def getValue(program: MutableProgram) = program(index(program))
		override def index(program: MutableProgram) = i + program.relativeBase
	}

	sealed trait Instruction
	case object RelativeBaseOffset extends Instruction
	case object Add extends Instruction
	case object Multiply extends Instruction
	case object StoreInput extends Instruction
	case object Output extends Instruction
	case object JumpIfTrue extends Instruction
	case object JumpIfFalse extends Instruction
	case object LessThan extends Instruction
	case object Equals extends Instruction
	case object Halt extends Instruction

	sealed trait IntCodeComputationStatus
	case object Terminated extends IntCodeComputationStatus
	case object ReadyToRun extends IntCodeComputationStatus
	case object WaitingForInput extends IntCodeComputationStatus
	
	trait IntCodeComputation {
		def identifier: String
		def run(): Unit
		def hasTerminated: Boolean
		def addInputs(in: List[BigInt]): Unit
		def output: List[BigInt]
		def reset()
	}

	trait BasicIOIntCodeComputation extends IntCodeComputation {
		import scala.collection.mutable.Queue
		var terminated = false
		var inputs: Queue[BigInt] = Queue.empty
		var outputs: List[BigInt] = Nil
		override def addInputs(in: List[BigInt]) = {
			for (input <- in) {
				inputs += input
			}
		}
		override def output = outputs
		override def hasTerminated = terminated
		override def reset(): Unit = {
			inputs = Queue.empty
			outputs = Nil
			terminated = false
		}
	}

	class IntCodeComputer(val id: String, val prog: MutableProgram) 
	extends IntCodeComputation with BasicIOIntCodeComputation {
		var i = BigInt(0)
		var computationStatus: IntCodeComputationStatus = ReadyToRun

		/*
		override def addInputs(in: List[BigInt]) = {
			super.addInputs(in)
			println("computer " + id + " now has input " + inputs)
		}
		*/

		override def identifier = id

		private def getVal(pa: Param) = pa.getValue(prog)

		def status: IntCodeComputationStatus = computationStatus

		override def reset() = {
			super.reset()
			i = 0
			prog.reset()
		}

		def runOneInstruction(): Option[BigInt] = {
			if (status == Terminated || (status == WaitingForInput && inputs.isEmpty)) return None

			computationStatus = ReadyToRun
			var out: Option[BigInt] = None
			val code = prog(i)
			val op = parseInstruction(code)
			val np = numParams(op)
			val intparams = for (j <- i + 1 to i + np) yield prog(j)
			val params = parseParams(code, intparams.toList)
			var doModifyInstrPointer = true
			(op, params) match {
				case (RelativeBaseOffset, Seq(p)) => {
					prog.updateRelativeBase(prog.relativeBase + getVal(p))
				}
				case (Add, Seq(p1, p2, p: PositionBasedParam)) => {
					prog(p.index(prog)) = getVal(p1) + getVal(p2)
				}
				case (Multiply, Seq(p1, p2, p: PositionBasedParam)) => {
					prog(p.index(prog)) = getVal(p1) * getVal(p2)
				}
				case (StoreInput, Seq(p: PositionBasedParam)) => {
					if (inputs.isEmpty) {
						doModifyInstrPointer = false
						computationStatus = WaitingForInput
					} else {
						prog(p.index(prog)) = inputs.dequeue
					}
				}
				case (Output, Seq(p)) => {
					out = Some(getVal(p))
				}
				case (JumpIfTrue, Seq(p1, p2)) => {
					if (getVal(p1) != 0) {
						i = getVal(p2)
						doModifyInstrPointer = false
					}
				}
				case (JumpIfFalse, Seq(p1, p2)) => {
					if (getVal(p1) == 0) {
						i = getVal(p2)
						doModifyInstrPointer = false
					}
				}
				case (LessThan, Seq(p1, p2, p: PositionBasedParam)) => {
					prog(p.index(prog)) = if (getVal(p1) < getVal(p2)) 1 else 0
				}
				case (Equals, Seq(p1, p2, p: PositionBasedParam)) => {
					prog(p.index(prog)) = if (getVal(p1) == getVal(p2)) 1 else 0
				}
				case (Halt, _) => {
					terminated = true
					computationStatus = Terminated
				}
				case unrecognized => {
					print("i: " + i)
					println(unrecognized)
					prog.print(i)
					???
				}
			}
			if (doModifyInstrPointer) {
				i = i + np + 1	
			}
			out
		}

		// Will run until reaching halt instruction (and terminating) or waiting for input.
		override def run() = {
			var limit = 10000000
			var out: List[BigInt] = Nil
			while (status == ReadyToRun || status == WaitingForInput && !inputs.isEmpty) {
				runOneInstruction() match {
					case Some(o) => out = out :+ o
					case _ => 
				}
				limit = limit - 1
				if (limit == 0) {
					throw new IllegalArgumentException("Too many iterations, infinite loop?")
				}
			}
			outputs = out
		}

		def parseParams(code: BigInt, params: List[BigInt]): Seq[Param] = {
			val pairs = "%03d".format(("0"+code.toString.dropRight(2)).toInt).reverse zip params 
			for (p <- pairs) yield p match {
				case ('0', i) => PositionParam(i)
				case ('1', i) => ImmediateParam(i)
				case ('2', i) => RelativeParam(i)
				case _ => ???
			}
		}

		def numParams(op: Instruction) = op match {
			case RelativeBaseOffset => 1
			case Add => 3
			case Multiply => 3
			case StoreInput => 1
			case Output => 1
			case JumpIfTrue => 2
			case JumpIfFalse => 2
			case LessThan => 3
			case Equals => 3
			case Halt => 0
		}

		def parseInstruction(code: BigInt): Instruction = code.toString.takeRight(2).toInt match {
			case 1 => Add
			case 2 => Multiply
			case 3 => StoreInput
			case 4 => Output
			case 5 => JumpIfTrue
			case 6 => JumpIfFalse
			case 7 => LessThan
			case 8 => Equals
			case 9 => RelativeBaseOffset
			case 99 => Halt
			case _ => ???
		}

	}

}