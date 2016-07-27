package edu.eckerd.integrations.slate.missingpersoncontact.methods

import cats.data.Xor
import cats.implicits._
import edu.eckerd.integrations.slate.missingpersoncontact.model.MissingPersonContact
import edu.eckerd.integrations.slate.missingpersoncontact.model.Name
import edu.eckerd.integrations.slate.missingpersoncontact.model.PhoneNumber
import edu.eckerd.integrations.slate.missingpersoncontact.persistence.{DBFunctions, DBImpl}
import edu.eckerd.integrations.slate.missingpersoncontact.persistence.SPREMRG.SpremrgRow

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by davenpcm on 7/27/16.
  */
trait MissingPersonMethods extends DBImpl {

  type PidmResponder = String => Future[Option[BigDecimal]]
  def pidmResponder : PidmResponder
//  type DBUpdater = SpremrgRow => Future[Unit]
//  def dbUpdater: DBUpdater
//  type EmailResponder = Seq[MissingPersonContact] => Future[Unit]
//  def emailResponder : EmailResponder

  def TranformToRowOrEmail(missingPersonContact: MissingPersonContact)(implicit ec: ExecutionContext): Future[Xor[MissingPersonContact, SpremrgRow]] = {
    val convertedStringContact = convertEmptyStringsOnMissingPerson(missingPersonContact)


    val phoneXor: Xor[MissingPersonContact, Option[PhoneNumber]] = parsePhone(convertedStringContact)
    val zipXor : Xor[MissingPersonContact, Option[String]] = parseZip(convertedStringContact)
    val name: Name = parseName(convertedStringContact.Name)



    val dataOrigin = "Slate Transfer"
    val userId = "ECBATCH"
    val priority = '8'
    val lastName = name.last
    val firstName = name.first

    val futurePidmXor: Future[Xor[MissingPersonContact , BigDecimal]] = pidmResponder(convertedStringContact.BannerID)
      .map( opt =>
        Xor.fromOption(opt, convertedStringContact)
      )

    for {
      pidmXor <- futurePidmXor
    } yield for {
      zip <- zipXor
      pidm <- pidmXor
      phone <- phoneXor
    } yield phone match {
      case Some(phoneNumber) =>
        SpremrgRow(
          pidm,
          priority,
          lastName,
          firstName,
          convertedStringContact.AddressStreet,
          convertedStringContact.AddressCity,
          zip,
          Some(phoneNumber.natnCode),
          phoneNumber.areaCode,
          Some(phoneNumber.phoneNumber),
          Some(convertedStringContact.Relationship.charAt(0)),
          new java.sql.Timestamp(new java.util.Date().getTime),
          Some(dataOrigin),
          Some(userId)
        )
      case None =>
        SpremrgRow(
          pidm,
          priority,
          lastName,
          firstName,
          convertedStringContact.AddressStreet,
          convertedStringContact.AddressCity,
          convertedStringContact.AddressPostal,
          None,
          None,
          None,
          Some(convertedStringContact.Relationship.charAt(0)),
          new java.sql.Timestamp(new java.util.Date().getTime),
          Some(dataOrigin),
          Some(userId)
        )

    }
  }

  def parseZip(missingPersonContact: MissingPersonContact): Xor[MissingPersonContact, Option[String]] = {
    missingPersonContact.AddressPostal match {
      case Some(zip) => if (zip.length <= 30) Xor.Right(Some(zip)) else Xor.Left(missingPersonContact)
      case None => Xor.Right(None)
    }
  }

  def parseName(string: String): Name = {
    Name(string.takeWhile(_ != ' '), string.dropWhile(_ != ' ').drop(1))
  }

  def convertEmptyStringsOnMissingPerson(missingPersonContact: MissingPersonContact): MissingPersonContact = {
    def ifBlankNone(s: String) : Option[String] = if (s == "") None else Some(s)
    missingPersonContact match {
      case MissingPersonContact(bi, r, n, Some(cell), Some(as), Some(ac), Some(ap)) =>
        MissingPersonContact(bi, r, n, ifBlankNone(cell), ifBlankNone(as), ifBlankNone(ac), ifBlankNone(ap))
      case _ => missingPersonContact
    }
  }

  def parsePhone(missingPersonContact: MissingPersonContact): Xor[MissingPersonContact, Option[PhoneNumber]] =
    missingPersonContact.Cell match {

    case Some(number) =>
      val parse = number.replace("+", "").replace(".", "-").replace(" ", "-")
      parse match {
        case usNumber if usNumber.startsWith("1-") && usNumber.length == 14 =>
          val areaCode = usNumber.dropWhile(_ != '-').drop(1).takeWhile(_ != '-')
          val phoneNumber = usNumber.dropWhile(_ != '-').drop(1).dropWhile(_ != '-').drop(1).replace("-", "")
          Xor.Right(Some(PhoneNumber("1", Some(areaCode), phoneNumber)))
        case intlParsed if intlParsed.dropWhile(_ != "-").drop(1).length <= 12 && !intlParsed.startsWith("1-") =>
          val natnCode = intlParsed.takeWhile(_ != "-")
          val phoneNumber = intlParsed.dropWhile(_ != "-").drop(1).replace("-", "")
          Xor.Right(Some(PhoneNumber(natnCode, None, phoneNumber)))
        case _ => Xor.Left(missingPersonContact)
      }

    case None =>
      Xor.Right(None)
    }

}
