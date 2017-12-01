package translator

import scala.io.Source

sealed trait Direction
case object Push extends Direction
case object Pop extends Direction
sealed trait Segment
case object Constant extends Segment
case object Local extends Segment
case object Argument extends Segment
case object This extends Segment
case object That extends Segment
case object Temp extends Segment
case object Pointer extends Segment
case object Static extends Segment
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

  def toAsm(statement: VMStatement): String = statement match {
    case Add => ""
    case Sub => ""
    case MemoryAccessCommand(Push, segment, i) => segment match {
      case Argument => ""
      case This => ""
      case That => ""
      case Temp => ""
      case Local => ""
      case Static => ""
      case Constant => {
        s"""
          |// *SP = i
          |@${i}
          |D=A
          |@SP
          |A=M
          |M=D
          |// SP++
          |@SP
          |M=M+1
        """.stripMargin
      }
      case Pointer => ""
    }
    case MemoryAccessCommand(Pop, segment, i) => segment match {
      case Argument => ""
      case This => ""
      case That => ""
      case Temp => ""
      case Local => ""
      case Static => ""
      case Constant => throw new MatchError("Cannot pop from Constant memory segment")
      case Pointer => ""
    }
  }
}

