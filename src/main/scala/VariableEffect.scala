import scala.lms.common._

trait PowerSource extends Base with PrimitiveOps with NumericOps with OrderingOps with LiftPrimitives with BooleanOps with Variables with LiftVariables with While with IfThenElse {
  def prog(n: Rep[Int]) : Rep[Double] = {
    var x = unit(0.0)
    if ((n < unit(1)) : Rep[Boolean]) {
      x = x + 3.0
    } else {
      x = x - 3.0
    }
    x
  }
}

class PowerExp extends PowerSource with BaseExp with PrimitiveOpsExp with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with VariablesExp with WhileExp with EffectExp with IfThenElseExp {
  val prog_reified = reifyEffects(prog(fresh[Int]))
}

object VEMain extends App {
  val p = new PowerExp()
  import p._
  val g = new ExportGraph { val IR: p.type = p }
  g.emitDepGraph(p.prog_reified.res, "hoge.dot", false)
  println("hello")
}