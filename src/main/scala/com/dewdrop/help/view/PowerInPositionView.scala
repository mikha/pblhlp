package com.dewdrop.help.view

import com.dewdrop.help.view.PowerInPositionView._
import org.scalajs.dom.html.Element
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

import scala.language.implicitConversions

case class PowerInPositionView(position: String,
                               power: BigDecimal,
                               topPower: Boolean,
                               capBonus: BigDecimal,
                               maybeBestSplit: Option[PowerSplit])
    extends View {
  override def view(): TypedTag[Element] = {
    val powerShow = power.show()
    val capBonusShow = capBonus.show()
    val bestSplit = maybeBestSplit.map(_.view()).getOrElse(span(""))
    div(
      `class` := (if (topPower) "list-group-item list-group-item-info"
                  else "list-group-item"),
      div(
        `class` := "row",
        div(
          `class` := "col-md-1 col-xs-2",
          if (topPower) strong(position) else position
        ),
        div(
          `class` := "col-md-2 col-xs-3",
          if (topPower) strong(powerShow) else powerShow
        ),
        div(
          `class` := "col-md-2 col-xs-3",
          if (topPower) strong(capBonusShow) else capBonusShow
        ),
        div(`class` := "col-md-2 col-xs-3", bestSplit)
      )
    )
  }
}

object PowerInPositionView {
  def header(rangesView: PowerRangesView): TypedTag[Element] = div(
    `class` := "list-group-item",
    div(
      `class` := "row",
      div(`class` := "col-md-1 col-xs-2", ""),
      div(`class` := "col-md-2 col-xs-3", ""),
      div(`class` := "col-md-2 col-xs-3", rangesView.leaderLevelSelector)
    )
  )

  def apply(position: String,
            basePower: BigDecimal,
            topPower: Boolean,
            rangesView: PowerRangesView,
            maybeBestSplit: Option[PowerSplit]): PowerInPositionView = {
    val power =
      withFormFiza(
        basePower = basePower,
        form = rangesView.form,
        fiza = rangesView.fiza,
        age = rangesView.age
      )
    val cb = capBonus(
      power = power,
      age = rangesView.age,
      leaderLevel = rangesView.leaderLevel
    )
    PowerInPositionView(
      position = position,
      power = power,
      topPower = topPower,
      capBonus = cb,
      maybeBestSplit = maybeBestSplit
    )
  }

  implicit class RichBigDecimal(val bd: BigDecimal) extends AnyVal {
    def show(scale: Int = 2): Double =
      bd.setScale(scale, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def GK(g: Int): BigDecimal =
    if (g > 30) Math.pow(1.0178, g - 30.0) * 35.0 else g

  sealed abstract class PowerCalc(val d: BigDecimal,
                                  val m: BigDecimal,
                                  val a: BigDecimal,
                                  k: BigDecimal) {
    def calc(p: PowerSplit): BigDecimal =
      (p.d.min(p.sum * d) + p.m.min(p.sum * m) + p.a.min(p.sum * a)) * k
  }

  case object XD extends PowerCalc(d = 0.80, m = 0.20, a = 0.00, k = 1.4)
  case object XB extends PowerCalc(d = 0.45, m = 0.40, a = 0.15, k = 1.15)
  case object DM extends PowerCalc(d = 0.40, m = 0.45, a = 0.15, k = 1.15)
  case object XM extends PowerCalc(d = 0.15, m = 0.70, a = 0.15, k = 1.3)
  case object AM extends PowerCalc(d = 0.10, m = 0.50, a = 0.40, k = 1.18)
  case object XW extends PowerCalc(d = 0.10, m = 0.45, a = 0.45, k = 1.15)
  case object XF extends PowerCalc(d = 0.00, m = 0.16, a = 0.84, k = 1.42)

  def withFormFiza(basePower: BigDecimal,
                   form: Int,
                   fiza: Int,
                   age: Int): BigDecimal = {
    val x = 0.284 * age * age - 8.47 * age + 49
    val formNorm = Math.max(
      83,
      if (form < 100) form.toDouble * (100 - x) / 100 + x else form.toDouble
    )
    val fizaKoef = formNorm * fiza / 100
    (basePower - (100 - fizaKoef) / 2.5) * (fizaKoef / 50 - (fizaKoef * fizaKoef) / 10000)
  }

  def capBonus(power: BigDecimal, age: Int, leaderLevel: Int): BigDecimal = {
    power * (1.0 + 0.2 * leaderLevel) * Math.max(0, age - 20) * 0.005
  }

  private def bestSplit(calc: PowerCalc)(original: PowerSplit): PowerSplit = {
    val d = (calc.d * original.sum).setScale(1, BigDecimal.RoundingMode.HALF_UP)
    val m = (calc.m * original.sum).setScale(1, BigDecimal.RoundingMode.HALF_UP)
    val a = original.sum - (d + m)
    original.copy(d = d, m = m, a = a)
  }
  def bestSplitXB: PowerSplit => PowerSplit = bestSplit(XB)
  def bestSplitDM: PowerSplit => PowerSplit = bestSplit(DM)
  def bestSplitAM: PowerSplit => PowerSplit = bestSplit(AM)
  def bestSplitXW: PowerSplit => PowerSplit = bestSplit(XW)
}
