package edu.eckerd.integrations.slate.missingpersoncontact.methods

import java.sql.Timestamp

import edu.eckerd.integrations.slate.missingpersoncontact.model.{MissingPersonResponse, OptOut}
import edu.eckerd.integrations.slate.missingpersoncontact.persistence.DBImpl
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
/**
  * Created by davenpcm on 7/28/16.
  */
class MissingPersonContactMethodsTest extends FlatSpec with Matchers with MissingPersonMethods with DBImpl {

  class testException extends Exception
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


}
