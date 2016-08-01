package edu.eckerd.integrations.slate.missingpersoncontact.methods

import cats.data.Xor
import cats.implicits._
import com.google.i18n.phonenumbers.PhoneNumberUtil
import com.typesafe.scalalogging.LazyLogging
import edu.eckerd.integrations.slate.missingpersoncontact.model._
import edu.eckerd.integrations.slate.missingpersoncontact.persistence.SPREMRG

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try


/**
  * Created by davenpcm on 7/27/16.
  */
trait MissingPersonMethods extends LazyLogging {
  this : SPREMRG =>

  type PidmResponder = String => Future[Option[Int]]
  def pidmResponder : PidmResponder
  type UpdateResponder = SpremrgRow => Future[Int]
  def dbUpdateResponder : UpdateResponder
  type EmailResponder = String => Future[Unit]
  def emailResponder : EmailResponder
  def timeResponder : java.sql.Timestamp

  /**
    * This is the SideEffect King. It executes the code using the responders and returns a Future of Unit
    * so other than ensuring it does not fail there is not much to this other than to see it as the execution step
    * @param seq The sequence of responses to process
    * @param ec The execution context to fork from
    * @return A Future of Unit
    */
  def ProcessResponses(seq: Seq[MissingPersonContact])
                      (implicit ec: ExecutionContext): Future[Unit] = partitionResponses(seq).flatMap{
    partitionedTuple =>
      for {
        _ <- SendEmail(partitionedTuple._1)
        _ <- UpdateDatabase(partitionedTuple._2)
      } yield ()
  }

  /**
    * This is the function that updates all values in the database as a result this endpoint is the most likely
    * for runtime failures as this is where we start to see the side effects
    * @param list The list of rows to be added/updated in the database
    * @param ec The execution context to fork from
    * @return A Future of Unit
    */
  def UpdateDatabase(list: List[SpremrgRow])
                    (implicit ec: ExecutionContext): Future[List[Int]] =
    Future.traverse(list)(dbUpdateResponder)

  /**
    * This function sends the email out
    * @param list The list of Contacts to Send
    * @param ec The execution Context
    * @return A Future of Unit
    */
  def SendEmail(list: List[MissingPersonContact])(implicit ec: ExecutionContext): Future[Unit] =
    emailResponder(generateCompleteHtmlString(list))

  /**
    * This function takes the initial sequence and transforms it into a sequence of Xor and then partitions that
    * into a tuple with the two types of data seperated
    * @param seq The sequence of responses
    * @param ec The execution context to fork from
    * @return  A Future Tuple of a List of MissingPersonContact(to be emailed) and a List of SpremrgRow(to be upserted
    *          in the database)
    */
  def partitionResponses(seq: Seq[MissingPersonContact])
                        (implicit ec: ExecutionContext): Future[(List[MissingPersonContact], List[SpremrgRow])] =
    Future.traverse(seq)(TransformToRowOrEmail)
      .map(partitionXor)

  /**
    * This is the XorConverter Method. It takes a Response and parses it to Either a Row to be updated or a value
    * to be sent in the email. It does this by first attempting to parse the record, then it generates additional
    * Xors for phone, zip, and pidm which can all fail under the right circumstance. Finally it extracts the values
    * from all of those possible outcomes to generate the final Row otherwise it returns the original response.
    *
    * @param missingPersonContact The response to validate
    * @param ec the execution context to fork from
    * @return A future of Xor of a MissingPersonContact, and a SpremrgRow with the right being the Option
    *         we desire.
    */
  def TransformToRowOrEmail(missingPersonContact: MissingPersonContact)
                           (implicit ec: ExecutionContext): Future[Xor[MissingPersonContact, SpremrgRow]] = for {
    pidmXor <- pidmResponder(missingPersonContact.BannerID).map(Xor.fromOption(_, missingPersonContact))
  } yield for {
    pidm <- pidmXor
    phoneOpt <- parsePhone(missingPersonContact)
    zipOpt <- parseZip(missingPersonContact)
  } yield createRow(missingPersonContact, zipOpt, pidm, phoneOpt)


  /**
    * This function transforms a sequence of Xors into a Tuple of two lists of the two types. We fold copying the
    * current value and adding the next value to the list. So we utilized the inherent duality of the Xor class
    * to seperate it into two seperate sequences
    * @param s The Sequence of Xors
    * @return A Tuple of A List of MissingPersonContact and SpremrgRow
    */
  def partitionXor(s: Seq[Xor[MissingPersonContact, SpremrgRow]]): (List[MissingPersonContact], List[SpremrgRow]) = {
    val leftAcc = List[MissingPersonContact]()
    val rightAcc = List[SpremrgRow]()
    def fold(next: Xor[MissingPersonContact, SpremrgRow], acc: (List[MissingPersonContact], List[SpremrgRow]))
    : (List[MissingPersonContact], List[SpremrgRow]) = next match {
      case Xor.Left(missingPersonResponse) =>
        acc.copy( _1 = missingPersonResponse :: acc._1)
      case Xor.Right(row) =>
        acc.copy( _2 = row :: acc._2 )
    }
    s.foldRight((leftAcc, rightAcc))(fold)
  }

  def createRow(
                 contact: MissingPersonContact,
                 zip: Option[String],
                 pidm: Int,
                 phoneOpt: Option[PhoneNumber],
                 time : java.sql.Timestamp = timeResponder
               ): SpremrgRow = {
    val dataOrigin = Some("Slate Transfer")
    val userId = Some("ECBATCH")
    val priority = '8'
    val relationship = Some('8')
    val nationCode = phoneOpt.map(_.natnCode)
    val areaCode = phoneOpt.flatMap(_.areaCode)
    val phoneNumber = phoneOpt.map(_.phoneNumber)
    contact match {
      case MissingPersonResponse(_, _, name, _, street, city,_) =>

        val pName: Name = parseName(name)
        val firstName = pName.first
        val lastName = pName.last
        val row = SpremrgRow(
          pidm, priority, lastName, firstName, Some(street), Some(city), zip,
          nationCode, areaCode, phoneNumber, relationship, time, dataOrigin, userId
        )
        row
      case OptOut(_) =>
        val firstName = "OPTION"
        val lastName = "DECLINED"
        SpremrgRow(
          pidm, priority, lastName, firstName, None, None, None,
          nationCode, areaCode, phoneNumber, relationship, time, dataOrigin, userId
        )
    }
  }

  def parseZip(missingPersonContact: MissingPersonContact): Xor[MissingPersonResponse, Option[String]] =
    missingPersonContact match {
      case OptOut(_) => Xor.Right(None)
      case MissingPersonResponse(_, _ , _, _ , _ , _ , zip) if zip.length <= 30 => Xor.Right(Some(zip))
      case responseUnmatched : MissingPersonResponse => Xor.Left(responseUnmatched)
    }

  def parseName(string: String): Name = {
    if (string.contains(' ')){
      Name(string.takeWhile(_ != ' '), string.dropWhile(_ != ' ').drop(1))
    } else {
      Name(string, ".")
    }

  }

  def parsePhone(missingPersonContact: MissingPersonContact)
  : Xor[MissingPersonResponse, Option[PhoneNumber]] = missingPersonContact match {
    case OptOut(_) =>
      Xor.Right(None)
    case missingPersonResponse: MissingPersonResponse =>
      val number = missingPersonResponse.Cell
      val util = PhoneNumberUtil.getInstance()
      val phoneTry = Try(util.parse(number, "US")).toOption

      Xor.fromOption(phoneTry, missingPersonResponse)
       .flatMap{ phoneParsed =>
        val countryCode = phoneParsed.getCountryCode.toString
        val nationalNumber = phoneParsed.getNationalNumber.toString

        countryCode match {
          case "1" =>
            val areaCode = nationalNumber.take(3)
            val number = nationalNumber.drop(3)
            if (number.length == 7) Xor.Right(Some(PhoneNumber(countryCode, Some(areaCode), number)))
            else Xor.Left(missingPersonResponse)
          case _ =>
            Xor.Right(Some(PhoneNumber(countryCode, None, nationalNumber)))
        }

      }
  }

  def generateHtmlString(missingPersonContact: MissingPersonContact): String = {
    val baseTable: (String, String, String, String, String, String, String) => String =
      (BannerID, Relationship, Name, Cell, AddressStreet, AddressCity, AddressPostal) =>
      s"""<tr>
         |  <td>$BannerID</td>
         |  <td>$Relationship</td>
         |  <td>$Name</td>
         |  <td>$Cell</td>
         |  <td>$AddressStreet</td>
         |  <td>$AddressCity</td>
         |  <td>$AddressPostal</td>
         |</tr>
       """.stripMargin
    missingPersonContact match {
      case OptOut(id) => baseTable(id,"8","OPTION DECLINED","","","","")
      case MissingPersonResponse(s1, s2, s3, s4, s5, s6, s7) => baseTable(s1,s2, s3, s4, s5, s6, s7)
    }
  }

  def generateCompleteHtmlString(list: List[MissingPersonContact]): String = {
    val content = list.foldLeft("")(_ + generateHtmlString(_))
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
      |                        <th>Zip Code</th>
      |                        </tr>""".stripMargin +
      content +
      """
        |                        </table>
        |                    </table>
        |                </td>
        |            </tr>
        |        </table>
        |    </body>
        |</html>
      """.stripMargin
  }

}
