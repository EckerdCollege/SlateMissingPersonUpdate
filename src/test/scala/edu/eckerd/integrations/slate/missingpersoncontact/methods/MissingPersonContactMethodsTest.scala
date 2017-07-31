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
  val state = "State"

  val zip = "Zip"
  val invalidZip = "thisHasToBeLongerThanThirtyCharactersLongToGenerateTheInvalidation"

  val usPhone = "+1 360 555 0234"
  val intlPhone = "+23 23-4237-1332"
  val invalidUSPhone = "+1 5604839450234523"

  val failPidmResponse = MissingPersonResponse( failID, relationship, name, usPhone, street, city, state, zip )
  val nonePidmResponse = MissingPersonResponse(noneID, relationship, name, usPhone, street, city, state, zip)
  val goodPidmResponse = MissingPersonResponse(goodID, relationship, name, usPhone, street, city, state, zip)

  val goodIntlNumberResponse = MissingPersonResponse(goodID, relationship, name, intlPhone, street, city, state, zip)
  val invalidUSNumberResponse = MissingPersonResponse(goodID, relationship, name, invalidUSPhone, street, city, state, zip)
  val invalidIntlNumberResponse = MissingPersonResponse(goodID, relationship, name, invalidUSPhone, street, city, state, zip)

  val invalidZipResponse = MissingPersonResponse(goodID, relationship, name, invalidUSPhone, street, city, state, invalidZip)



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
    MissingPersonResponse(goodID, relationship, name, string, street, city, state, zip)

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
    MissingPersonResponse(goodID, relationship, name, usPhone, street, city, state, zip)

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
    val mpr = MissingPersonResponse("1", "8", "First Last", "+1 360 903 2985", "street", "city", "state", "zip")
    val r = SpremrgRow(
      1,
      "8".charAt(0),
      "Last",
      "First",
      Some("street"),
      Some("city"),
      Some("state"),
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
      None,
      Some("8".charAt(0)),
      timeResponder,
      Some("Slate Transfer"),
      Some("ECBATCH")
    )

    Await.result(TransformToRowOrEmail(o), 1.second) should be (Xor.right(r))
  }

  it should "go Left if the zip is too Long" in {
    val mpr = MissingPersonResponse("1", "8", "First Last", "+1 360 903 2985", "street", "city", "state",
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
      SpremrgRow(1, "8".charAt(0), "Last", "First", Some("Street"), Some("City"), Some("State"), Some("Zip"),
        Some("1"), Some("360"), Some("5550234"), Some("8".charAt(0)), timeResponder, Some("Slate Transfer"), Some("ECBATCH")
      ),
      SpremrgRow(1, "8".charAt(0), "DECLINED", "OPTION", None, None, None, None,
        None, None, None, Some("8".charAt(0)), timeResponder, Some("Slate Transfer"), Some("ECBATCH")
      ),
      SpremrgRow(1, "8".charAt(0), "Last", "First", Some("Street"), Some("City"), Some("State"), Some("Zip"),
        Some("232"), None, Some("342371332"), Some("8".charAt(0)), timeResponder, Some("Slate Transfer"), Some("ECBATCH")
      )
    )

      Await.result(partitionResponses(goodSeqContact).map(_._2), 1.second) should be (good)
  }

  "generateHtmlString" should "write a MissingPersonResponse" in {
    generateHtmlString(goodPidmResponse) should be (
      """<tr>
        |  <td>1</td>
        |  <td>8</td>
        |  <td>First Last</td>
        |  <td>+1 360 555 0234</td>
        |  <td>Street</td>
        |  <td>City</td>
        |  <td>State</td>
        |  <td>Zip</td>
        |</tr>
        |""".stripMargin)
  }

  it should "write an OptOut" in {
    generateHtmlString(goodPidmOptOut) should be(
      """<tr>
        |  <td>1</td>
        |  <td>8</td>
        |  <td>OPTION DECLINED</td>
        |  <td></td>
        |  <td></td>
        |  <td></td>
        |  <td></td>
        |  <td></td>
        |</tr>
        |""".stripMargin)
  }

  "generateCompleteHtmlString" should "create a String with a valid sequence" in {
    val l = List(goodPidmOptOut, goodPidmResponse)
    generateCompleteHtmlString(l) should be(
      Some(
        """<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
          |<html xmlns="http://www.w3.org/1999/xhtml">
          |    <head>
          |        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
          |        <title></title>
          |        <style></style>
          |    </head>
          |    <body>
          |        <table border="0" cellpadding="0" cellspacing="0" height="100%" width="100%" id="bodyTable">
          |            <tr>
          |                <td align="center" valign="top">
          |                    <table border="0" cellpadding="0" cellspacing="0" width="800" id="emailContainer">
          |                        <tr>
          |                            <td align="center" valign="top">
          |                                <h2>Unparsed Missing Person Contacts</h2>
          |                            </td>
          |                        </tr>
          |                        <table border="0" cellpadding="2" cellspacing="2" height="100%" width=100% id="MissingPersonContact">
          |                        <tr>
          |                        <th>Banner ID</th>
          |                        <th>Relationship</th>
          |                        <th>Name</th>
          |                        <th>Phone Number</th>
          |                        <th>Street Address</th>
          |                        <th>City</th>
          |                        <th>State</th>
          |                        <th>Zip Code</th>
          |                        </tr>""".stripMargin +
          """<tr>
            |  <td>1</td>
            |  <td>8</td>
            |  <td>OPTION DECLINED</td>
            |  <td></td>
            |  <td></td>
            |  <td></td>
            |  <td></td>
            |  <td></td>
            |</tr>
            |""".stripMargin +
          """<tr>
            |  <td>1</td>
            |  <td>8</td>
            |  <td>First Last</td>
            |  <td>+1 360 555 0234</td>
            |  <td>Street</td>
            |  <td>City</td>
            |  <td>State</td>
            |  <td>Zip</td>
            |</tr>
            |""".stripMargin +
          """
            |                        </table>
            |                    </table>
            |                </td>
            |            </tr>
            |        </table>
            |    </body>
            |</html>""".stripMargin )
    )
  }

  it should "return a None if it is given an empty list" in {
    val l = List[MissingPersonContact]()
    generateCompleteHtmlString(l) should be (None)
  }

  "SendEmail" should "utilize the emailResponder if there is a value to be sent" in {
    class cool extends MissingPersonMethods with DBImpl{
      override implicit val dbConfig: DatabaseConfig[JdbcProfile] = DatabaseConfig.forConfig("oracle")
      def pidmResponder : PidmResponder =  {
        case fail if fail == "-1" => Future.failed(new testException())
        case passAsNone if passAsNone == "0" => Future.successful(None)
        case _ =>  Future.successful(Some(1))
      }
      def dbUpdateResponder : UpdateResponder = _ => Future.successful(1)
      def emailResponder : EmailResponder = _ => Future.failed(new testException)
      def timeResponder : java.sql.Timestamp = Timestamp.valueOf("2016-07-28 10:10:10.0")
    }
    val myobj = new cool
    val l = List(goodPidmOptOut, goodPidmResponse)

    intercept[testException]{
      Await.result(myobj.SendEmail(l), 1.second)
    }
  }

  it should "not Use the responder if the List is empty" in {
    class cool extends MissingPersonMethods with DBImpl{
      override implicit val dbConfig: DatabaseConfig[JdbcProfile] = DatabaseConfig.forConfig("oracle")
      def pidmResponder : PidmResponder =  {
        case fail if fail == "-1" => Future.failed(new testException())
        case passAsNone if passAsNone == "0" => Future.successful(None)
        case _ =>  Future.successful(Some(1))
      }
      def dbUpdateResponder : UpdateResponder = _ => Future.successful(1)
      def emailResponder : EmailResponder = _ => Future.failed(new testException)
      def timeResponder : java.sql.Timestamp = Timestamp.valueOf("2016-07-28 10:10:10.0")
    }
    val myobj = new cool
    val l = List[MissingPersonContact]()

    Await.result(myobj.SendEmail(l), 1.second) should be (())
  }


}
