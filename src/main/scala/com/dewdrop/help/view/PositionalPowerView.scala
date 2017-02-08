package com.dewdrop.help.view

import com.dewdrop.help.view.ElementOps._
import org.scalajs.dom.Event
import org.scalajs.dom.html.{Element, Heading}

import scala.language.implicitConversions
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

trait PositionalPowerView extends View {
  private lazy val formInput = RangeControlView("Форма", 76, 110, 100, calcAndRenderPowers)
  private lazy val fizaInput = RangeControlView("Физа", 75, 100, 100, calcAndRenderPowers)
  private lazy val ageInput = RangeControlView("Возраст", 17, 35, 26, calcAndRenderPowers)
  private lazy val leaderLevelSelector = select(`class` := "form-control input-sm", width := 70,
    for (i ← 0 to 3) yield option(value := i, s"Л$i")
  ).render
  leaderLevelSelector.onchange = (e: Event) ⇒ {
    calcAndRenderPowers()
  }
  protected def leaderLevel: Int = leaderLevelSelector.value.toInt
  import PositionalPower._
  protected def posPower(position: String, posPowerFn: => Double): PositionalPower = {
    val power = withFormFiza(posPowerFn, formInput.currentValue, fizaInput.currentValue, ageInput.currentValue)
    val cb = capBonus(power, ageInput.currentValue, leaderLevel)
    PositionalPower(position, power, cb)
  }
  protected def allPosPowers: Seq[PositionalPower]
  def header: Heading
  private lazy val posPowersOutput = div(`class` := "list-group").render
  private def calcAndRenderPowers(): Unit = {
    val newPosPowers = allPosPowers
    val maxPosPower = newPosPowers.map(_.power).max
    val newPosPowerChildren = newPosPowers.map {
      case PositionalPower(position, power, capBonus) =>
        val topPosPower = power == maxPosPower
        div(`class` := (if (topPosPower) "list-group-item list-group-item-info" else "list-group-item"),
          div(`class` := "row",
            div(`class` := "col-md-1 col-xs-2", if (topPosPower) strong(position) else position),
            div(`class` := "col-md-2 col-xs-3", if (topPosPower) strong(power) else power),
            div(`class` := "col-md-2 col-xs-3", if (topPosPower) strong(capBonus) else capBonus)
          )
        ).render
    }
    posPowersOutput.removeAllChildren()
    posPowersOutput.appendChild(div(`class` := "list-group-item",
      div(`class` := "row",
        div(`class` := "col-md-1 col-xs-2", ""),
        div(`class` := "col-md-2 col-xs-3", ""),
        div(`class` := "col-md-2 col-xs-3", leaderLevelSelector)
      )
    ).render)
    newPosPowerChildren.foreach(posPowersOutput.appendChild)
  }
  calcAndRenderPowers()
  override def view(): TypedTag[Element] = {
    div(
      div(`class` := "form-inline", style := "padding: 5px",
        div(`class` := "form-group", style := "padding: 5px", width := "300px",
          formInput.rangeLabel, formInput.rangeInput
        ),
        div(`class` := "form-group", style := "padding: 5px", width := "300px",
          fizaInput.rangeLabel, fizaInput.rangeInput
        ),
        div(`class` := "form-group", style := "padding: 5px", width := "300px",
          ageInput.rangeLabel, ageInput.rangeInput
        )
      ),
      posPowersOutput
    )
  }
}
case class PositionalPower(position: String, power: Double, capBonus: Double)

case class OutfieldPositionalPowerView(defPower: Int, midPower: Int, attPower: Int) extends PositionalPowerView {
  private lazy val sum = defPower + midPower + attPower
  lazy val header: Heading = h3(defPower, " + ", midPower, " + ", attPower, " = ", defPower + midPower + attPower).render
  import PositionalPower._
  override protected def allPosPowers: Seq[PositionalPower] = Seq(
    posPower("xD", XD(defPower, midPower, attPower, sum)),
    posPower("xB", XB(defPower, midPower, attPower, sum)),
    posPower("DM", DM(defPower, midPower, attPower, sum)),
    posPower("xM", XM(defPower, midPower, attPower, sum)),
    posPower("AM", AM(defPower, midPower, attPower, sum)),
    posPower("xW", XW(defPower, midPower, attPower, sum)),
    posPower("xF", XF(defPower, midPower, attPower, sum)),
    posPower("ST", ST(defPower, midPower, attPower, sum)),
    posPower("F**", FNew(defPower, midPower, attPower, sum))
  )
}
case class KeeperPositionalPowerView(gkPower: Int) extends PositionalPowerView {
  lazy val header: Heading = h3(gkPower).render
  import PositionalPower._
  override protected def allPosPowers: Seq[PositionalPower] = Seq(
    posPower("GK", GK(gkPower))
  )
}

object PositionalPower {
  def roundUp(d: Double): Double = BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  def GK(g: Int): Double = roundUp(if (g > 30) Math.pow(1.0178, g - 30.0) * 35.0 else g.toDouble)
  def XD(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(d, sum * 0.80) + math.min(m, sum * 0.20)) * 1.4)
  def DM(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(d, sum * 0.40) + math.min(m, sum * 0.45) + math.min(a, sum * 0.15)) * 1.15)
  def XB(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(d, sum * 0.45) + math.min(m, sum * 0.40) + math.min(a, sum * 0.15)) * 1.15)
  def XM(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(d, sum * 0.15) + math.min(m, sum * 0.70) + math.min(a, sum * 0.15)) * 1.3)
  def AM(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(d, sum * 0.10) + math.min(m, sum * 0.50) + math.min(a, sum * 0.40)) * 1.18)
  def XW(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(d, sum * 0.10) + math.min(m, sum * 0.45) + math.min(a, sum * 0.45)) * 1.15)
  def XF(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(m, sum * 0.20) + math.min(a, sum * 0.80)) * 1.22)
  def ST(d: Int, m: Int, a: Int, sum: Int): Double = roundUp(math.min(a, sum * 1.00) * 1.6)
  def FNew(d: Int, m: Int, a: Int, sum: Int): Double = roundUp(math.min(m, sum * 0.16) + math.min(a, sum * 0.84) * 1.42)
  def withFormFiza(base: Double, form: Int, fiza: Int, age: Int): Double = {
    val x = 0.284 * age * age - 8.47 * age + 49
    val formNorm = Math.max(83, if (form < 100) form.toDouble * (100 - x) / 100 + x else form.toDouble)
    val fizaKoef = formNorm * fiza / 100
    roundUp((base - (100 - fizaKoef) / 2.5) * (fizaKoef / 50 - (fizaKoef * fizaKoef) / 10000))
  }
  def capBonus(base: Double, age: Int, leaderLevel: Int): Double = {
    roundUp(base * (1.0 + 0.2 * leaderLevel)* Math.max(0, age - 20) * 0.005)
  }
}