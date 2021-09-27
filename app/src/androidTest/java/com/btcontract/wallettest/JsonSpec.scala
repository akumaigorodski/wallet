package com.btcontract.wallettest

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import androidx.test.ext.junit.runners.AndroidJUnit4
import org.junit.runner.RunWith
import org.junit.Test


@RunWith(classOf[AndroidJUnit4])
class JsonSpec {

  @Test
  def json4sDecodesFine: Unit = {
    val json = """{"jsonClass":"MessageAction","domain":"https://www.website.com","message":"hello there"}"""
    val parsedMessage = (parse(json) \ "message").asInstanceOf[JString]
    assert(parsedMessage.values == "hello there")
  }

  @Test
  def json4SEncodesFine: Unit = {
    val json = ("method" -> "execute") ~ ("param" -> 1)
    assert(compact(render(json)) == """{"method":"execute","param":1}""")
  }
}
