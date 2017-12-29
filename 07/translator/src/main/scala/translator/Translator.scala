package translator

import scala.io.Source
import java.io.{File, PrintWriter}
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

object Translator {
  def main(args: Array[String]): Unit = {
    val Array(fileName) = args

    val file = new File(fileName)

    val writer = new PrintWriter(fileName.replaceFirst("\\.vm", ".asm"))

    try {
      Source
        .fromFile(file)
        .getLines()
        .collect(toVMStatement)
        .map(toAsm(file.getParentFile.toPath.getFileName.toString))
        .foreach(writer.println)
    } finally {
      writer.flush()
      writer.close()
    }
  }

  val MemoryAccessPattern = "(push|pop) (argument|this|that|temp|local|static|constant|pointer) (.+)".r
  val StackArithmeticPattern = "(add|sub|neg|eq|gt|lt|and)".r

  val toVMStatement: PartialFunction[String, VMStatement] = {
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
          |MD=M-D
          |@EQ.is_true.$uuid
          |D;JEQ
          |@SP
          |A=M
          |M=-1
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
          |D=-1
          |@GT.end.$uuid
          |0;JMP
          |(GT.is_true.$uuid)
          |D=0
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
          |D=-1
          |@LT.end.$uuid
          |0;JMP
          |(LT.is_true.$uuid)
          |D=0
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
