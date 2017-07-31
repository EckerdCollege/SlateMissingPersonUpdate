package edu.eckerd.integrations.slate.missingpersoncontact.request

import edu.eckerd.integrations.slate.missingpersoncontact.model.{MissingPersonResponse, OptOut}
import org.scalatest.{FlatSpec, Matchers}
import spray.json.{DeserializationException, JsObject, JsString, deserializationError}
/**
  * Created by davenpcm on 7/28/16.
  */
class  jsonProtocolTests extends FlatSpec with Matchers {
  import jsonProtocol._

  "OptOutFormat" should "write an OptOut to json" in {
    val o = OptOut("1")
    val j = JsObject("BannerID" -> JsString("1"))
    OptOutResponseFormat.write(o) should be (j)
  }

  it should "read and OptOut to a case class" in {
    val o = OptOut("1")
    val j = JsObject("BannerID" -> JsString("1"))
    OptOutResponseFormat.read(j) should be (o)
  }

  "MissingPersonResponseFormat" should "write a MissingPersonResponse to json" in {
    val m = MissingPersonResponse("1", "8", "First Last", "Cell", "Street", "City", "State", "Zip")
    val j = JsObject(
      "BannerID" -> JsString("1"),
      "Relationship" -> JsString("8"),
      "Name" -> JsString("First Last"),
      "Cell" -> JsString("Cell"),
      "AddressStreet" -> JsString("Street"),
      "AddressCity" -> JsString("City"),
      "AddressState" -> JsString("State"),
      "AddressPostal" -> JsString("Zip")
      )
    MissingPersonResponseFormat.write(m) should be (j)
  }

  it should "read a MissingPersonResponse from json" in {
    val m = MissingPersonResponse("1", "8", "First Last", "Cell", "Street", "City", "State", "Zip")
    val j = JsObject(
      "BannerID" -> JsString("1"),
      "Relationship" -> JsString("8"),
      "Name" -> JsString("First Last"),
      "Cell" -> JsString("Cell"),
      "AddressStreet" -> JsString("Street"),
      "AddressCity" -> JsString("City"),
      "AddressState" -> JsString("State"),
      "AddressPostal" -> JsString("Zip")
    )
    MissingPersonResponseFormat.read(j) should be (m)
  }

  "MissingPersonContactFormat" should "be able to write an OptOut" in {
    val o = OptOut("1")
    val j = JsObject("BannerID" -> JsString("1"))
    MissingPersonContactFormat.write(o) should be (j)
  }

  it should "be able to write a MissingPersonResponse" in {
    val m = MissingPersonResponse("1", "8", "First Last", "Cell", "Street", "City", "State", "Zip")
    val j = JsObject(
      "BannerID" -> JsString("1"),
      "Relationship" -> JsString("8"),
      "Name" -> JsString("First Last"),
      "Cell" -> JsString("Cell"),
      "AddressStreet" -> JsString("Street"),
      "AddressCity" -> JsString("City"),
      "AddressState" -> JsString("State"),
      "AddressPostal" -> JsString("Zip")
    )
    MissingPersonContactFormat.write(m) should be (j)
  }

  it should "be able to read an OptOut" in {
    val o = OptOut("1")
    val j = JsObject("BannerID" -> JsString("1"))
    MissingPersonContactFormat.read(j) should be (o)
  }

  it should "be able to read a MissingPersonResponse with values" in {
    val m = MissingPersonResponse("1", "8", "First Last", "Cell", "Street", "City", "State", "Zip")
    val j = JsObject(
      "BannerID" -> JsString("1"),
      "Relationship" -> JsString("8"),
      "Name" -> JsString("First Last"),
      "Cell" -> JsString("Cell"),
      "AddressStreet" -> JsString("Street"),
      "AddressCity" -> JsString("City"),
      "AddressState" -> JsString("State"),
      "AddressPostal" -> JsString("Zip")
    )
    MissingPersonContactFormat.read(j) should be (m)
  }

  it should "be able to return an optOut if they have opted Out in a Response" in {
    val m = OptOut("1")
    val j = JsObject(
      "BannerID" -> JsString("1"),
      "Relationship" -> JsString("8"),
      "Name" -> JsString("OPTION DECLINED"),
      "Cell" -> JsString("Cell"),
      "AddressStreet" -> JsString("Street"),
      "AddressCity" -> JsString("City"),
      "AddressState" -> JsString("State"),
      "AddressPostal" -> JsString("Zip")
    )
    MissingPersonContactFormat.read(j) should be (m)
  }

  it should "Fail to Parse Nonsense that is not correct" in {
    val j =  JsObject(
      "Nonsense" -> JsString("Monkeys"),
      "Perfect" -> JsString("Eat Bananas")
    )
    intercept[DeserializationException]{
      MissingPersonContactFormat.read(j)
    }
  }


}
