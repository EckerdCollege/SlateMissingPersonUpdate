package edu.eckerd.integrations.slate.missingpersoncontact.request

import edu.eckerd.integrations.slate.core.DefaultJsonProtocol
import edu.eckerd.integrations.slate.missingpersoncontact.model.{MissingPersonContact, MissingPersonResponse, OptOut}
import spray.json.{JsArray, JsObject, JsString, JsValue, RootJsonFormat}
import spray.json.deserializationError

import scala.util.Try

/**
  * Created by davenpcm on 7/27/16.
  */
object jsonProtocol extends DefaultJsonProtocol{
  implicit val MissingPersonResponseFormat = jsonFormat7(MissingPersonResponse)
  implicit val OptOutResponseFormat = jsonFormat1(OptOut)

  implicit object MissingPersonContactFormat extends RootJsonFormat[MissingPersonContact] {
    def write(c: MissingPersonContact) = c match {
      case optOut : OptOut => OptOutResponseFormat.write(optOut)
      case missingPersonResponse: MissingPersonResponse => MissingPersonResponseFormat.write(missingPersonResponse)
    }
    def read(value: JsValue) = value match {
      case mpr if Try(MissingPersonResponseFormat.read(value)).isSuccess =>
        MissingPersonContact(MissingPersonResponseFormat.read(value))
      case opt if Try(OptOutResponseFormat.read(value)).isSuccess => OptOutResponseFormat.read(value)
      case _ => deserializationError("MissingPersonContact Expected")
    }
  }
}
