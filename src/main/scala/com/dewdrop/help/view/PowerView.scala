package com.dewdrop.help.view

import com.dewdrop.help.view.ElementOps._
import org.scalajs.dom.html.Element
import org.scalajs.dom.{DragEvent, Event}

import scala.scalajs.js.timers._
import scala.util.Try
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

object PowerView extends View {
  private val powerThresholdInput = input(`type` := "text", `class` := "form-control", placeholder := "вставляйте цифры сюда").render
  powerThresholdInput.onpaste = (e: DragEvent) ⇒ {
    setTimeout(200) {
      handleThresholdInput(powerThresholdInput.value)
    }
  }
  powerThresholdInput.onkeypress = (e: Event) ⇒ {
    setTimeout(200) {
      handleThresholdInput(powerThresholdInput.value)
    }
  }
  private val powerOutput = div().render
  override def view(): TypedTag[Element] = {
    div(
      h4("расчет расклада силы игрока по границам уровня"),
      p("обладатели платного пакета и 4-го уровня скаута видят следующие данные для любого игрока:"),
      img(src := "scout-exp-power.png", `class` := "img-responsive"),
      p(style := "margin-top: 10px;",
        "выделяйте цифры для ", strong("нижней границы уровня"),
        " и копируйте в поле ниже",
        i(" (одно число считается как вратарь)")),
      p("кроме того можно просто вписать значения составляющих через ",
        i(title := "любые нечисловые символы", "разделитель"),
        ", например 11-31-25", ":"),
      div(`class` := "input-group",
        span(`class` := "input-group-btn",
          button(`type` := "button", `class` := "btn btn-primary", onclick := clearAll _, "Очистить")
        ),
        powerThresholdInput
      ),
      powerOutput
    )
  }
  private def clearAll(e: Event): Unit = {
    powerThresholdInput.value = ""
    handleThresholdInput("")
    powerThresholdInput.focus()
  }
  private def handleThresholdInput(in: String): Unit = {
    val powerView = in match {
      case OutfieldExp(defExp, midExp, attExp) =>
        val defPower = level(defExp, identity)
        val midPower = level(midExp, identity)
        val attPower = level(attExp, identity)
        Some(OutfieldPositionalPowerView(defPower, midPower, attPower))
      case KeeperExp(gkExp) =>
        val gkPower = level(gkExp, _ + 30)
        Some(KeeperPositionalPowerView(gkPower))
      case _ => None
    }
    powerOutput.removeAllChildren()
    powerView.foreach { view =>
      powerOutput.appendChild(view.header)
      powerOutput.appendChild(view.view().render)
    }
  }
  private val levels = 1 to 75 map { i => (10700 * math.pow(1.0427, i) - 10698).toInt }
  def level(exp: Int, levelMod: Int ⇒ Int): Int = {
    val level = levels.indexWhere(_ > exp)
    if (level < 1) exp else levelMod(level)
  }
}

object OutfieldExp {
  def unapply(arg: String): Option[(Int, Int, Int)] = {
    extract(arg.trim).orElse(extract(arg.replace(" ", "")))
  }
  private def extract(arg: String): Option[(Int, Int, Int)] = {
    arg.split("\\D").map(_.trim).filterNot(_.isEmpty) match {
      case Array(one, two, three) ⇒ Try((one.toInt, two.toInt, three.toInt)).toOption
      case _ ⇒ None
    }
  }
}
object KeeperExp {
  def unapply(arg: String): Option[Int] = {
    arg.replace(" ", "").trim.split("\t") match {
      case Array(one) => Try(one.toInt).toOption
      case _ => None
    }
  }
}