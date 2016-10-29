package com.dewdrop.help.view
import org.scalajs.dom.html.Element

import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag

object YourIdeasView extends View {
  override def view(): TypedTag[Element] = h4(
    "присылайте ваши идеи для пбл-утилит через ",
    a(href := "http://pbliga.com/mng_send_message.php?act=direct&toid=38", "личные сообщения")
  )
}
