package translator

import scala.io.Source
import java.io.{File, PrintWriter, FilenameFilter}
import java.util.UUID

sealed trait Direction
case object Push extends Direction
case object Pop extends Direction

sealed trait Segment
sealed trait GlobalSegment extends Segment
sealed trait FunctionSegment extends Segment

case object Local extends FunctionSegment
case object Argument extends FunctionSegment
case object This extends FunctionSegment
case object That extends FunctionSegment
case object Constant extends GlobalSegment
case object Temp extends GlobalSegment
case object Pointer extends GlobalSegment
case object Static extends GlobalSegment

sealed trait VMStatement
case class MemoryAccessCommand(direction: Direction, segment: Segment, index: Int) extends VMStatement
sealed trait StackArithmeticCommand extends VMStatement
case object Add extends StackArithmeticCommand
case object Sub extends StackArithmeticCommand
case object Neg extends StackArithmeticCommand
case object Eq extends StackArithmeticCommand
case object Gt extends StackArithmeticCommand
case object Lt extends StackArithmeticCommand
case object And extends StackArithmeticCommand
case object Or extends StackArithmeticCommand
case object Not extends StackArithmeticCommand
sealed trait BranchingCommand extends VMStatement
case class Label(label: String) extends BranchingCommand
case class Goto(label: String) extends BranchingCommand
case class IfGoto(label: String) extends BranchingCommand
sealed trait FunctionCommand extends VMStatement
case class Declaration(label: String, locals: Int) extends FunctionCommand
case class Call(label: String, arity: Int) extends FunctionCommand
case object Return extends FunctionCommand

object Translator {
  def main(args: Array[String]): Unit = {
    val Array(path) = args
    val file = new File(path)
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = {
        name.matches("^.+\\.vm")
      }
    }

    val files: Array[File] = if (file.isDirectory) {
      file.listFiles(filter)
    } else {
      Array(file)
    }

    val writer = new PrintWriter(s"${file.getPath}/${file.getName}.asm")

    val initializeStackPointer =
      s"""| // boot
          |@256
          |@SP
          |M=A""".stripMargin

    val callSysInit = toAsm("")(Call("Sys.init", 0))

    val boot = List(
      initializeStackPointer,
      callSysInit
    ).mkString("\n")

    try {
      writer.write(boot)
      writer.write('\n')

      files.foreach { source =>
        Source
          .fromFile(source)
          .getLines()
          .collect(toVMStatement)
          .map(toAsm(source.getName))
          .foreach(writer.println)
      }
    } finally  {
      writer.close()
    }
  }

  val MemoryAccessPattern = "(push|pop) (argument|this|that|temp|local|static|constant|pointer) (\\d+).*".r
  val StackArithmeticPattern = "(add|sub|neg|eq|gt|lt|and|or|not).*".r
  val BranchingPattern = "(label|goto|if-goto) (\\w+).*".r
  val FunctionCommandPattern = "(function|call) (.+) (\\d+).*".r

  val toVMStatement: PartialFunction[String, VMStatement] = {
    case FunctionCommandPattern(command, label, arity) => command match {
      case "function" => Declaration(label, arity.toInt)
      case "call" => Call(label, arity.toInt)
    }
    case "return" => Return
    case BranchingPattern(command, label) => command match {
      case "label" => Label(label)
      case "goto" => Goto(label)
      case "if-goto" => IfGoto(label)
    }
    case MemoryAccessPattern(direction, segment, index) =>
      MemoryAccessCommand(toDirection(direction), toSegment(segment), index.toInt)
    case StackArithmeticPattern(command) => command match {
      case "add" => Add
      case "sub" => Sub
      case "neg" => Neg
      case "eq" => Eq
      case "gt" => Gt
      case "lt" => Lt
      case "and" => And
      case "or" => Or
      case "not" => Not
    }
  }

  val toDirection: PartialFunction[String, Direction] = {
    case "push" => Push
    case "pop" => Pop
  }

  val toSegment: PartialFunction[String, Segment] = {
    case "argument" => Argument
    case "this" => This
    case "that" => That
    case "temp" => Temp
    case "local" => Local
    case "static" => Static
    case "constant" => Constant
    case "pointer" => Pointer
  }

  def toAsm(segment: Segment): String = segment match {
    // SP?
    case Local => "LCL"
    case This => "THIS"
    case That => "THAT"
    case Argument => "ARG"
    case _: GlobalSegment =>
      throw new MatchError("Global memory segments do not have pointers in RAM. They are either fixed mappings, or virtual.")
  }

  def calculateTempIndex(accessIndex: Int): Int = {
    // the temp memory segment is implemented on RAM as registers R5 to R12
    val tempRange = (5 to 12).toSet

    require(tempRange.contains(accessIndex),
      "out of bounds of the temp memory segment")

    val tempBase = 5
    val ramIndex = tempBase + accessIndex

    return ramIndex
  }

  def resolvePointer(index: Int): String = index match {
    case 0 => "THIS"
    case 1 => "THAT"
    case _ => throw new MatchError("Pointer segment index out of range.")
  }

  def toVM(segment: Segment): String = segment.toString.toLowerCase

  def toAsm(fileName: String)(statement: VMStatement): String = statement match {
    case Call(name, arity) =>
      val uuid = UUID.randomUUID.toString
      val RET_ADDRESS = s"RET_ADDRESS.$uuid"

      val pushReturnAddress = s"""|// call $name $arity
                                  |@$RET_ADDRESS
                                  |D=A
                                  |@SP
                                  |A=M
                                  |M=D""".stripMargin

      val saveFrame = List("LCL", "ARG", "THIS", "THAT").map { pointer =>
        s"""|@$pointer // push $pointer
            |D=M
            |@SP
            |AM=M+1
            |M=D""".stripMargin
      }.mkString("\n")

      val setArg = s"""|@$arity
                       |D=A
                       |@SP
                       |D=M-D
                       |@5
                       |D=D-A
                       |@ARG
                       |M=D""".stripMargin

      val setLcl = s"""|@SP
                       |D=M
                       |@LCL
                       |M=D""".stripMargin

      val goto = toAsm(fileName)(Goto(name))

      val returnLabel = s"($RET_ADDRESS)"

      List(
        pushReturnAddress,
        saveFrame,
        setArg,
        setLcl,
        goto,
        returnLabel
      ).mkString("\n")
    case Return =>
      s"""|// return
          |@LCL // frame = LCL
          |D=M
          |@R13
          |M=D
          |@5 // retAddr = *(frame - 5)
          |A=D-A
          |D=M
          |@R14
          |M=D
          |@SP // *ARG = pop
          |A=M-1
          |D=M
          |@ARG
          |A=M
          |M=D
          |@ARG // SP = ARG + 1
          |D=M
          |@SP
          |M=D+1
          |@R13 // THAT = *(frame - 1)
          |A=M-1
          |D=M
          |@THAT
          |M=D
          |@2 // THIS = *(frame - 2)
          |D=A
          |@R13
          |A=M-D
          |D=M
          |@THIS
          |M=D
          |@3 // ARG = *(frame - 3)
          |D=A
          |@R13
          |A=M-D
          |D=M
          |@ARG
          |M=D
          |@4 // LCL = *(frame - 4)
          |D=A
          |@R13
          |A=M-D
          |D=M
          |@LCL
          |M=D
          |@R14
          |A=M
          |0;JMP""".stripMargin
    case Declaration(name, locals) =>
      val label = s"""|// function $name $locals
                      |($name)""".stripMargin
      val pushConstant0 = toAsm(fileName)(MemoryAccessCommand(Push, Constant, 0))

      label + '\n' + pushConstant0 + '\n' + pushConstant0
    case IfGoto(name) =>
      s"""|// if-goto $name
          |@SP
          |AM=M-1
          |D=M
          |@$name
          |D;JNE""".stripMargin
    case Goto(name) =>
      s"""|// goto $name
          |@$name
          |0;JMP""".stripMargin
    case Label(name) =>
      s"""|// label $name
          |($name)""".stripMargin
    case Add =>
      s"""|// add
          |@SP
          |AM=M-1
          |D=M
          |@SP
          |AM=M-1
          |M=D+M
          |@SP
          |M=M+1""".stripMargin
    case Sub =>
      s"""|// sub
          |@SP
          |AM=M-1
          |D=M
          |@SP
          |AM=M-1
          |M=M-D
          |@SP
          |M=M+1""".stripMargin
    case Neg =>
      s"""|// neg
          |@SP
          |AM=M-1
          |M=-M
          |@SP
          |M=M+1""".stripMargin
    case Eq =>
      val uuid = UUID.randomUUID.toString
      s"""|// eq
          |@SP
          |AM=M-1
          |D=M
          |@SP
          |AM=M-1
          |D=M-D
          |M=-1
          |@EQ.is_true.$uuid
          |D;JEQ
          |@SP
          |A=M
          |M=0
          |(EQ.is_true.$uuid)
          |@SP
          |M=M+1""".stripMargin
    case Gt =>
      val uuid = UUID.randomUUID.toString
      s"""|// gt
          |@SP
          |AM=M-1
          |D=M
          |@SP
          |AM=M-1
          |D=M-D
          |@GT.is_true.$uuid
          |D;JGT
          |D=0
          |@GT.end.$uuid
          |0;JMP
          |(GT.is_true.$uuid)
          |D=-1
          |(GT.end.$uuid)
          |@SP
          |A=M
          |M=D
          |@SP
          |M=M+1""".stripMargin
    case Lt =>
      val uuid = UUID.randomUUID.toString
      s"""|// lt
          |@SP
          |AM=M-1
          |D=M
          |@SP
          |AM=M-1
          |D=M-D
          |@LT.is_true.$uuid
          |D;JLT
          |D=0
          |@LT.end.$uuid
          |0;JMP
          |(LT.is_true.$uuid)
          |D=-1
          |(LT.end.$uuid)
          |@SP
          |A=M
          |M=D
          |@SP
          |M=M+1""".stripMargin
    case And =>
      s"""|// and
          |@SP
          |AM=M-1
          |D=M
          |@SP
          |AM=M-1
          |M=D&M
          |@SP
          |M=M+1""".stripMargin
    case Or =>
      s"""|// or
          |@SP
          |AM=M-1
          |D=M
          |@SP
          |AM=M-1
          |M=D|M
          |@SP
          |M=M+1""".stripMargin
    case Not =>
      s"""|// not
          |@SP
          |A=M-1
          |M=!M""".stripMargin
    case MemoryAccessCommand(Push, segment: FunctionSegment, i) =>
      s"""|// push ${toVM(segment)} $i
          |@$i
          |D=A
          |@${toAsm(segment)}
          |A=M+D
          |D=M
          |@SP
          |A=M
          |M=D
          |@SP
          |M=M+1""".stripMargin
    case MemoryAccessCommand(Push, segment: GlobalSegment, i) => segment match {
      case Constant =>
        s"""|// push ${toVM(segment)} $i
            |@$i
            |D=A
            |@SP
            |A=M
            |M=D
            |@SP
            |M=M+1""".stripMargin
      case Static =>
        s"""|// push ${toVM(segment)} $i
            |@$fileName.$i
            |D=M
            |@SP
            |A=M
            |M=D
            |@SP
            |M=M+1""".stripMargin
      case Temp =>
        val tempIndex = calculateTempIndex(i)
        s"""|// push ${toVM(segment)} $i
            |@R$tempIndex
            |D=M
            |@SP
            |A=M
            |M=D
            |@SP
            |M=M+1""".stripMargin
      case Pointer =>
        s"""|// push ${toVM(segment)} $i
            |@${resolvePointer(i)}
            |D=M
            |@SP
            |A=M
            |M=D
            |@SP
            |M=M+1""".stripMargin
    }
    case MemoryAccessCommand(Pop, segment: FunctionSegment, i) =>
      s"""|// pop ${toVM(segment)} $i
          |@$i
          |D=A
          |@${toAsm(segment)}
          |D=M+D
          |@R13
          |M=D
          |@SP
          |AM=M-1
          |D=M
          |@R13
          |A=M
          |M=D""".stripMargin
    case MemoryAccessCommand(Pop, segment: GlobalSegment, i) => segment match {
      case Constant => throw new MatchError("Cannot pop from Constant memory segment")
      case Static =>
        s"""|// pop ${toVM(segment)} $i
            |@SP
            |AM=M-1
            |D=M
            |@$fileName.$i
            |M=D""".stripMargin
      case Temp =>
        val tempIndex = calculateTempIndex(i)
        s"""|// pop ${toVM(segment)} $i
            |@SP
            |AM=M-1
            |D=M
            |@R$tempIndex
            |M=D""".stripMargin
      case Pointer =>
        s"""|// pop ${toVM(segment)} $i
            |@SP
            |AM=M-1
            |D=M
            |@${resolvePointer(i)}
            |M=D""".stripMargin
    }
  }
}
