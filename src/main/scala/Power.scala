package examples

import java.io.PrintWriter
import org.scalatest.FunSuite
import scala.lms.common._

/**
  * Created by asakura on 17/06/05.
  */
trait PowerSource extends Base with PrimitiveOps with NumericOps with LiftPrimitives with Variables {
  def power(b: Rep[Double], n: Int): Rep[Double] = {
    if (n == 0) 1.0
    else b * power(b, n - 1)
  }
  def power10(b: Rep[Double]) : Rep[Double] = power(b, 10)
}

trait PowerExp extends PowerSource with BaseExp with PrimitiveOpsExp with NumericOpsExp with LiftPrimitives with
  VariablesExp {
}

trait PowerGenScala extends ScalaGenBase with ScalaGenPrimitiveOps with ScalaGenNumericOps with ScalaGenVariables {
  val IR : PowerExp
}

trait PowerGenC extends CGenBase with CGenPrimitiveOps with CGenNumericOps with CGenVariables {
  val IR : PowerExp
}

class PowerScala extends PowerSource with PowerExp with CompileScala { self =>
  override val codegen = new PowerGenScala { val IR : self.type = self }
  lazy val f = compile(power10)(manifestTyp[Double], manifestTyp[Double])
  def eval(x: Double): Double = f(x)
}

class PowerC extends PowerSource with PowerExp { self =>
  val codegen = new PowerGenC { val IR : self.type = self }
}

object PowerMain extends App {
  val powScala = new PowerScala()
  val powC = new PowerC()
  // assert(p.f(2) === 1024)
  import powScala._
  {
    lazy val code: String = {
      val source = new java.io.StringWriter()
      powScala.codegen.emitSource(powScala.power10, "power10_scala", new PrintWriter(source))
      source.toString
    }
    println(code)
  }

  {
    import powC._
    lazy val code: String = {
      val source = new java.io.StringWriter()
      powC.codegen.emitSource(powC.power10, "power10_C", new PrintWriter(source))
      source.toString
    }
    println(code)
  }
}