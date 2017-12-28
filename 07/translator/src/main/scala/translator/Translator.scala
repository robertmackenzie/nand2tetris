package translator

import scala.io.Source
import java.io.File

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

object Translator {
  def main(args: Array[String]): Unit = {
    val Array(fileName) = args

    val file = new File(fileName)

    Source
      .fromFile(file)
      .getLines()
      .collect(toVMStatement)
      .map(toAsm(file.getParentFile.toPath.getFileName.toString))
      .foreach(println)
  }

  val MemoryAccessPattern = "(push|pop) (argument|this|that|temp|local|static|constant|pointer) (.+)".r
  val StackArithmeticPattern = "(add|sub)".r

  val toVMStatement: PartialFunction[String, VMStatement] = {
    case MemoryAccessPattern(direction, segment, index) =>
      MemoryAccessCommand(toDirection(direction), toSegment(segment), index.toInt)
    case StackArithmeticPattern(command) => command match {
      case "add" => Add
      case "sub" => Sub
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
    // ... GlobalSegment
  }

  def toVM(segment: Segment): String = segment.toString.toLowerCase

  def toAsm(fileName: String)(statement: VMStatement): String = statement match {
    case Add => "// add"
    case Sub => "// sub"
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
      case Temp => s"// push temp $i"
      case Pointer => s"// push pointer $i"
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
            |M=D
            """.stripMargin
      case Temp => s"// pop temp $i"
      case Pointer => s"// pop pointer $i"
    }
  }
}

