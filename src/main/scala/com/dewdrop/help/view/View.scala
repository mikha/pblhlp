package com.dewdrop.help.view

import org.scalajs.dom.html.Element

import scalatags.JsDom

trait View {
  def view(): JsDom.TypedTag[Element]
}
