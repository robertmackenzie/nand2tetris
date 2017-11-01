package assembler

import scala.io.Source

sealed trait Instruction
class A(index: Int) extends Instruction
object A {
  val a = "@(.+)".r

  def apply(index: Int): Instruction = {
    new A(index)
  }

  def unapply(asm: String): Option[Int] = {
    asm match {
      case a(i) => Some(i.toInt)
      case _ => None
    }
  }
}
case class C(dst: Dst, comp: Comp, jmp: Jmp) extends Instruction

sealed trait CToken
case class Dst(a: Register, b: Register, c: Register) extends CToken
case class Jmp(one: Conditional, two: Conditional) extends CToken
case class Comp(operation: Operation, a: Operand, b: Operand) extends CToken

sealed trait Operation
case object Negate extends Operation
case object Zero extends Operation
case object Minus extends Operation
case object Plus extends Operation
case object And extends Operation
case object Or extends Operation

sealed trait Register
sealed trait Operand
case object AReg extends Register with Operand
case object DReg extends Register with Operand
case object MReg extends Register with Operand
case object One extends Operand

sealed trait Conditional
case object Equal
case object Greater
case object Less
case object Not

case object Assembler {
  def main(args: Array[String]): Unit = {
    val Array(fileName) = args

    val statements = Source
      .fromFile(fileName)
      .getLines()
      .collect(parse)
      .foreach(println)
  }

  // DST=COMP;JMP
  val c = "(?:(.+)=)?(.+?)(?:;(.+))?".r

  val parse: PartialFunction[AnyRef, Any] = {
    case A(index) => A(index)
    //case C(dst, comp, jmp) => {
      //val (a, b, c) = parse(dst)
      //C(a, b, c)
    //}
  }
}

  //def toBinaryString(statement: String): Seq[String] = {
    //case Whitespace => Seq()
    //case Comment => Seq()
    //case Instruction(text) => {
      //text match {
        //case A(Location(location)) => Seq(s"0${toBinaryString(location)}")
        //case C(dst, comp, jmp) => Seq(s"111${toBinaryString(comp)}${toBinaryString(dst)}${toBinaryString(jmp)}")
      //}
    //}
  //}

  //def toBinaryString(token: Option[String]): String = {
    //case None => "000"
    //case Dst(M) => "001"
    //case Dst(D) => "010"
    //case Dst(M, D) => "011"
    //case Dst(A) => "100"
    //case Dst(A, M) => "101"
    //case Dst(A, D) => "110"
    //case Dst(A, M, D) => "111"
    //case Comp(Zero) => "0101010"
    //case Comp(Empty, One) =>
    //case Comp(Minus, One) =>
    //case Comp(Empty, D) =>
    //case Comp(Negate, D) =>
    //case Comp(Negate, A) =>
    //case Comp(Negate, M) =>
    //case Comp(Minus, D) =>
    //case Comp(Minus, A) =>
    //case Comp(Minus, M) =>
    //case Comp(Plus, D, One) =>
    //case Comp(Plus, D, One) =>
    //case Comp(Plus, M, One) =>
    //case Comp(Minus, D, One) =>
    //case Comp(Minus, A, One) =>
    //case Comp(Minus, M, One) =>
    //case Comp(Plus, D, A) =>
    //case Comp(Plus, D, M) =>
    //case Comp(Minus D, A) =>
    //case Comp(Minus, D, M) =>
    //case Comp(Minus, A, D) =>
    //case Comp(Minus, M, D) =>
    //case Comp(And, D, A) =>
    //case Comp(And, D, M) =>
    //case Comp(Or, D, A) =>
    //case Comp(Or, D, M) =>
    //case Jmp(Greater) => "001"
    //case Jmp(Equal) => "010"
    //case Jmp(Greater, Equal) => "011"
    //case Jmp(Less) => "100"
    //case Jmp(Not, Equal) => "101"
    //case Jmp(Less, Equal) => "110"
    //case Jmp => "111"
  //}
//}
