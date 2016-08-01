package edu.eckerd.integrations.slate.missingpersoncontact.methods

import java.sql.Timestamp

import cats.data.Xor
import com.typesafe.scalalogging.LazyLogging
import edu.eckerd.integrations.slate.missingpersoncontact.model._
import edu.eckerd.integrations.slate.missingpersoncontact.persistence.DBImpl
import org.scalatest.{FlatSpec, Matchers}
import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
/**
  * Created by davenpcm on 7/28/16.
  */
class MissingPersonContactMethodsTest
  extends FlatSpec
  with Matchers
  with MissingPersonMethods
  with DBImpl {

  class testException extends Exception
  override implicit val dbConfig: DatabaseConfig[JdbcProfile] = DatabaseConfig.forConfig("oracle")
  def pidmResponder : PidmResponder =  {
    case fail if fail == "-1" => Future.failed(new testException())
    case passAsNone if passAsNone == "0" => Future.successful(None)
    case _ =>  Future.successful(Some(1))
  }
  def dbUpdateResponder : UpdateResponder = _ => Future.successful(1)
  def emailResponder : EmailResponder = _ => Future.successful(())
  def timeResponder : java.sql.Timestamp = Timestamp.valueOf("2016-07-28 10:10:10.0")

  val goodID = "1"
  val noneID = "0"
  val failID = "-1"

  val relationship = "8"
  val name = "First Last"

  val street = "Street"
  val city = "City"

  val zip = "Zip"
  val invalidZip = "thisHasToBeLongerThanThirtyCharactersLongToGenerateTheInvalidation"

  val usPhone = "+1 360 555 0234"
  val intlPhone = "+23 23-4237-1332"
  val invalidUSPhone = "+1 5604839450234523"

  val failPidmResponse = MissingPersonResponse( failID, relationship, name, usPhone, street, city, zip )
  val nonePidmResponse = MissingPersonResponse(noneID, relationship, name, usPhone, street, city, zip)
  val goodPidmResponse = MissingPersonResponse(goodID, relationship, name, usPhone, street, city, zip)

  val goodIntlNumberResponse = MissingPersonResponse(goodID, relationship, name, intlPhone, street, city, zip)
  val invalidUSNumberResponse = MissingPersonResponse(goodID, relationship, name, invalidUSPhone, street, city, zip)
  val invalidIntlNumberResponse = MissingPersonResponse(goodID, relationship, name, invalidUSPhone, street, city, zip)

  val invalidZipResponse = MissingPersonResponse(goodID, relationship, name, invalidUSPhone, street, city, invalidZip)



  val failPidmOptOut = OptOut(failID)
  val nonePidmOptOut = OptOut(noneID)
  val goodPidmOptOut = OptOut(goodID)

  val failSeqContact =
    Seq(failPidmResponse, nonePidmResponse, goodPidmResponse, failPidmOptOut, nonePidmOptOut, goodPidmOptOut)
  val goodSeqContact = Seq(nonePidmResponse, goodPidmResponse, nonePidmOptOut, goodPidmOptOut,
    goodIntlNumberResponse, invalidUSNumberResponse, invalidIntlNumberResponse, invalidZipResponse)

  "ProcessRequests" should "be able to deal with a sequence of  EmergencyContactRequests" in {
    val awaitable = ProcessResponses(goodSeqContact)
    Await.result(awaitable, 60.seconds) should be (())
  }

  it should "fail when futures fail - as in database not reachable" in {
    val awaitable = ProcessResponses(failSeqContact)
    intercept[testException]{
      Await.result(awaitable, 60.seconds)
    }
  }

  def phoneFactory(string: String): MissingPersonResponse =
    MissingPersonResponse(goodID, relationship, name, string, street, city, zip)

  "parsePhone" should "parse a US Number" in {
    val phone: String = "+1 908-789-9691"
    val missingPersonResponse = phoneFactory(phone)
    parsePhone(missingPersonResponse) should be (Xor.Right(Some(PhoneNumber("1", Some("908"), "7899691" ))))
  }

  it should "parse a UsNumber with spaces" in {
    val phone : String = "+1 360 903 2985"
    val missingPersonResponse = phoneFactory(phone)
    parsePhone(missingPersonResponse) should be (Xor.Right(Some(PhoneNumber("1", Some("360"), "9032985" ))))
  }

  it should "parse an International Phone Number" in {
    val phone: String = "+94 77 272 6598"
    val missingPersonResponse = phoneFactory(phone)
    parsePhone(missingPersonResponse) should be (Xor.Right(Some(PhoneNumber("94", None, "772726598" ))))
  }

  it should "parse an International Number without spacing assistance" in {
    val phone: String = "+62 812-3810-668"
    val missingPersonResponse = phoneFactory(phone)
    parsePhone(missingPersonResponse) should be (Xor.Right(Some(PhoneNumber("62", None, "8123810668" ))))
  }

  it should "parse a Us number without the country code" in {
    val phone: String = "7272370076"
    val missingPersonResponse = phoneFactory(phone)
    parsePhone(missingPersonResponse) should be (Xor.Right(Some(PhoneNumber("1", Some("727"), "2370076" ))))
  }

  it should "parse a usNumber that is irregularly formatted" in {
    val phone: String = "+1.603-651-8289"
    val missingPersonResponse = phoneFactory(phone)
    parsePhone(missingPersonResponse) should be (Xor.Right(Some(PhoneNumber("1", Some("603"), "6518289" ))))
  }

  it should "fail an invalid number" in {
    val mpr = phoneFactory(invalidUSPhone)
    parsePhone(mpr) should be (Xor.Left(mpr))
  }

  it should "parse any opt out to a None" in {
    val o = OptOut("1")
    parsePhone(o) should be (Xor.Right(None))
  }

  "parseName" should "generate a name from a string" in {
    val string = "Chris Davenport"
    parseName(string) should be (Name("Chris", "Davenport"))
  }

  it should "generate a first name and a period for the last name if passed a string with no spaces" in {
    val string = "Nathanael"
    parseName(string) should be (Name("Nathanael", "."))
  }

  it should "ensure that any appellations are added with the last name" in {
    val string = "Christopher Davenport Jr. III"
   parseName(string) should be (Name("Christopher", "Davenport Jr. III"))
  }

  def zipFactory(zip: String): MissingPersonResponse =
    MissingPersonResponse(goodID, relationship, name, usPhone, street, city, zip)

  "parseZip" should "accept any zip less than 30 characters" in {
    val zip: String = "98662"
    val mpr = zipFactory(zip)
    parseZip(mpr) should be (Xor.Right(Some(zip)))
  }

  it should "push to a left any zip longer than 30" in {
    val zip : String = "10 Downing St, London SW1A 2AA, United Kingdom"
    val mpr = zipFactory(zip)
    parseZip(mpr) should be (Xor.Left(mpr))
  }

  it should "parse an Opt Out as a None" in {
    val o = OptOut("1")
    parseZip(o) should be (Xor.Right(None))
  }

  "TransformToRowOrEmail" should "parse a valid record to a Row" in {
    val mpr = MissingPersonResponse("1", "8", "First Last", "+1 360 903 2985", "street", "city", "zip")
    val r = SpremrgRow(
      1,
      "8".charAt(0),
      "Last",
      "First",
      Some("street"),
      Some("city"),
      Some("zip"),
      Some("1"),
      Some("360"),
      Some("9032985"),
      Some("8".charAt(0)),
      timeResponder,
      Some("Slate Transfer"),
      Some("ECBATCH")
    )

    Await.result(TransformToRowOrEmail(mpr), 1.second) should be (Xor.Right(r))
  }

  it should "parse a valid Opt Out" in {
    val o = OptOut("1")
    val r = SpremrgRow(
      1,
      "8".charAt(0),
      "DECLINED",
      "OPTION",
      None,
      None,
      None,
      None,
      None,
      None,
      Some("8".charAt(0)),
      timeResponder,
      Some("Slate Transfer"),
      Some("ECBATCH")
    )

    Await.result(TransformToRowOrEmail(o), 1.second) should be (Xor.right(r))
  }

  it should "go Left if the zip is too Long" in {
    val mpr = MissingPersonResponse("1", "8", "First Last", "+1 360 903 2985", "street", "city",
      "10 Downing St, London SW1A 2AA, United Kingdom")

    Await.result(TransformToRowOrEmail(mpr), 1.second) should be (Xor.Left(mpr))
  }

  "partitionResponses" should "seperate Bad Response to the Left of a Tuple" in {
    val bad = List(
      nonePidmResponse,
      nonePidmOptOut,
      invalidUSNumberResponse,
      invalidIntlNumberResponse,
      invalidZipResponse
    )
    Await.result(partitionResponses(goodSeqContact).map(_._1), 1.second) should be (bad)
  }

  it should "seperate good Responses to the Right as SpremrgRows" in {
    val good = List(
      SpremrgRow(1, "8".charAt(0), "Last", "First", Some("Street"), Some("City"), Some("Zip"),
        Some("1"), Some("360"), Some("5550234"), Some("8".charAt(0)), timeResponder, Some("Slate Transfer"), Some("ECBATCH")
      ),
      SpremrgRow(1, "8".charAt(0), "DECLINED", "OPTION", None, None, None,
        None, None, None, Some("8".charAt(0)), timeResponder, Some("Slate Transfer"), Some("ECBATCH")
      ),
      SpremrgRow(1, "8".charAt(0), "Last", "First", Some("Street"), Some("City"), Some("Zip"),
        Some("232"), None, Some("342371332"), Some("8".charAt(0)), timeResponder, Some("Slate Transfer"), Some("ECBATCH")
      )
    )

      Await.result(partitionResponses(goodSeqContact).map(_._2), 1.second) should be (good)
  }


}
