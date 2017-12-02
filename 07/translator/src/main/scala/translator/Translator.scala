package translator

import scala.io.Source

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

    Source
      .fromFile(fileName)
      .getLines()
      .collect(parse)
      .map(toAsm)
      .foreach(println)
  }

  val MemoryAccessPattern = "(push|pop) (argument|this|that|temp|local|static|constant|pointer) (.+)".r
  val StackArithmeticPattern = "(add|sub)".r

  val parse: PartialFunction[String, VMStatement] = {
    case MemoryAccessPattern(direction, segment, index) => direction match {
      case "push" => MemoryAccessCommand(Push, Local, index.toInt)
      case "pop" => MemoryAccessCommand(Pop, Local, index.toInt)
    }
    case StackArithmeticPattern(command) => command match {
      case "add" => Add
      case "sub" => Sub
    }
  }

  def toAsm(segment: Segment): String = segment match {
    // SP?
    case Local => "LCL"
    case This => "THIS"
    case That => "THAT"
    case Argument => "ARG"
    // ... GlobalSegment
  }

  def toVM(segment: Segment): String = segment match {
    case Local => "local"
    case This => "this"
    case That => "that"
    case Argument => "argument"
    // ... GlobalSegment
  }

  def toAsm(statement: VMStatement): String = statement match {
    case Add => ""
    case Sub => ""
    case MemoryAccessCommand(Push, segment: FunctionSegment, i) =>
      s"""
        |// push ${toVM(segment)} $i
        |@$i
        |D=A
        |@${toAsm(segment)}
        |A=M+D
        |D=M
        |@SP
        |A=M
        |M=D
        |@SP
        |M=M+1
      """.stripMargin
    case MemoryAccessCommand(Push, segment: GlobalSegment, i) => segment match {
      case Constant =>
        s"""
          |// push ${toVM(segment)} $i
          |@$i
          |D=A
          |@SP
          |A=M
          |M=D
          |@SP
          |M=M+1
        """.stripMargin
      case Pointer => ""
      case Temp => ""
      case Static => ""
    }
    case MemoryAccessCommand(Pop, segment: FunctionSegment, i) =>
      s"""
        |// pop ${toVM(segment)} $i
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
        |M=D
      """.stripMargin
    case MemoryAccessCommand(Pop, segment: GlobalSegment, i) => segment match {
      case Constant => throw new MatchError("Cannot pop from Constant memory segment")
      case Pointer => ""
      case Temp => ""
      case Static => ""
    }
  }
}

