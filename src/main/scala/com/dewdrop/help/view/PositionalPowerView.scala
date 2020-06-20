package com.dewdrop.help.view

import com.dewdrop.help.view.ElementOps._
import com.dewdrop.help.view.PowerInPositionView._
import org.scalajs.dom.html.{Element, Heading}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

import scala.language.implicitConversions

sealed abstract class PositionalPowerView(rangesView: PowerRangesView)
    extends View {
  private lazy val powerLinesContainer = div(`class` := "list-group").render
  override def view(): TypedTag[Element] = {
    div(rangesView.view(), powerLinesContainer)
  }
  protected def powerLines: Seq[TypedTag[Element]]
  def header: Heading
  def calcAndRenderPowers(): Unit = {
    val lines = powerLines
    powerLinesContainer.removeAllChildren()
    lines.foreach { p =>
      powerLinesContainer.appendChild(p.render)
    }
  }
}

case class OutfieldPositionalPowerView(split: PowerSplit,
                                       rangesView: PowerRangesView)
    extends PositionalPowerView(rangesView) {
  lazy val header: Heading = h3(split.view(), " = ", split.sum.show()).render
  override protected def powerLines: Seq[TypedTag[Element]] = {
    val powerCalculated = Seq(
      ("xD", XD.calc(split), None),
      ("xB", XB.calc(split), Some(bestSplitXB)),
      ("DM", DM.calc(split), Some(bestSplitDM)),
      ("xM", XM.calc(split), None),
      ("AM", AM.calc(split), Some(bestSplitAM)),
      ("xW", XW.calc(split), Some(bestSplitXW)),
      ("xF", XF.calc(split), None)
    )
    val maxPower = powerCalculated.map(_._2).max

    PowerInPositionView.header(rangesView) +:
      powerCalculated.map {
      case (position, power, maybeBestSplitFn) =>
        val topPower = power == maxPower
        val showBestSplit = power >= maxPower * 0.85
        PowerInPositionView(
          position = position,
          basePower = power,
          topPower = topPower,
          rangesView = rangesView,
          maybeBestSplit =
            if (showBestSplit) maybeBestSplitFn.map(_.apply(split)) else None
        ).view()
    }
  }
}
case class KeeperPositionalPowerView(gkPower: Int, rangesView: PowerRangesView)
    extends PositionalPowerView(rangesView) {
  lazy val header: Heading = h3(gkPower).render
  import PowerInPositionView._
  override protected def powerLines: Seq[TypedTag[Element]] =
    Seq(
      PowerInPositionView.header(rangesView),
      PowerInPositionView(
        position = "GK",
        basePower = GK(gkPower),
        topPower = true,
        rangesView = rangesView,
        maybeBestSplit = None
      ).view()
    )
}

case class PowerSplit(d: BigDecimal, m: BigDecimal, a: BigDecimal)
    extends View {
  lazy val sum: BigDecimal = d + m + a

  override def view(): TypedTag[Element] =
    span(d.show(), " + ", m.show(), " + ", a.show())
}
