package edu.eckerd.integrations.slate.missingpersoncontact.methods

import cats.data.Xor

import cats.implicits._
import edu.eckerd.integrations.slate.missingpersoncontact.model._
import edu.eckerd.integrations.slate.missingpersoncontact.persistence.SPREMRG.SpremrgRow

import scala.concurrent.{ExecutionContext, Future}


/**
  * Created by davenpcm on 7/27/16.
  */
trait MissingPersonMethods {

  type PidmResponder = String => Future[Option[BigDecimal]]
  def pidmResponder : PidmResponder
  type UpdateResponder = SpremrgRow => Future[Unit]
  def updateResponder : UpdateResponder
  type EmailResponder = List[MissingPersonContact] => Future[Unit]
  def emailResponder : EmailResponder

  /**
    * This is the SideEffect King. It executes the code using the responders and returns a Future of Unit
    * so other than ensuring it does not fail there is not much to this other than to see it as the execution step
    * @param seq The sequence of responses to process
    * @param ec The execution context to fork from
    * @return A Future of Unit
    */
  def ProcessResponses(seq: Seq[MissingPersonResponse])
                      (implicit ec: ExecutionContext): Future[Unit] = partitionResponses(seq).map{
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
                    (implicit ec: ExecutionContext): Future[Unit] =
    Future.traverse(list)(updateResponder).map(_ => ())

  /**
    * This function sends the email out
    * @param list The list of Contacts to Send
    * @param ec The execution Context
    * @return A Future of Unit
    */
  def SendEmail(list: List[MissingPersonContact])(implicit ec: ExecutionContext): Future[Unit] = emailResponder(list)

  /**
    * This function takes the initial sequence and transforms it into a sequence of Xor and then partitions that
    * into a tuple with the two types of data seperated
    * @param seq The sequence of responses
    * @param ec The execution context to fork from
    * @return  A Future Tuple of a List of MissingPersonContact(to be emailed) and a List of SpremrgRow(to be upserted
    *          in the database)
    */
  def partitionResponses(seq: Seq[MissingPersonResponse])
                        (implicit ec: ExecutionContext): Future[(List[MissingPersonContact], List[SpremrgRow])] =
    Future.traverse(seq)(TransformToRowOrEmail)
      .map(partitionXor)

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
      case Xor.Left(missingPersonContact) =>
        acc.copy( _1 = missingPersonContact :: acc._1)
      case Xor.Right(row) =>
        acc.copy( _2 = row :: acc._2 )
    }

    s.foldRight((leftAcc, rightAcc))(fold)
  }

  def convertToTypes(
                      missingPersonResponse: MissingPersonResponse
                    ): Xor[MissingPersonResponse, FinishedMissingPersonContact] = missingPersonResponse match {
    case MissingPersonResponse(id, _, "Monkey", _, _, _, _) =>
      Xor.Right(OptOut(id))
    case MissingPersonResponse(id, relationship, name, Some(cell), Some(street), Some(city), Some(state)) =>
      Xor.Right(CompleteMissingPersonContact(id, relationship, name, cell, street, city, state))
    case _ => Xor.Left(missingPersonResponse)
  }


  def TransformToRowOrEmail(missingPersonContact: MissingPersonResponse)
                          (implicit ec: ExecutionContext): Future[Xor[MissingPersonContact, SpremrgRow]] = {

    val convertedContact: Xor[MissingPersonContact, FinishedMissingPersonContact] = convertToTypes(missingPersonContact)
    val phoneXor: Xor[MissingPersonContact, Option[PhoneNumber]] = convertedContact.flatMap(parsePhone)
    val zipXor : Xor[MissingPersonContact, String] = convertedContact.flatMap(parseZip)
    val futurePidmXor: Future[Xor[MissingPersonContact, BigDecimal]] = convertedContact
      .bimap(Future.successful, x => pidmResponder(x.BannerID))
      .bisequence
      .map(_.flatMap(Xor.fromOption(_, missingPersonContact)))

    for {
      pidmXor <- futurePidmXor
    } yield for {
      contact <- convertedContact
      zip <- zipXor
      pidm <- pidmXor
      phoneOpt <- phoneXor
    } yield createRow(contact, zip, pidm, phoneOpt)
  }

  def createRow(
                 contact: FinishedMissingPersonContact,
                 zip: String,
                 pidm: BigDecimal,
                 phoneOpt: Option[PhoneNumber],
                  time : java.sql.Timestamp = new java.sql.Timestamp(new java.util.Date().getTime)
               ): SpremrgRow = {
    val dataOrigin = Some("Slate Transfer")
    val userId = Some("ECBATCH")
    val priority = '8'
    val relationship = Some('8')

    val nationCode = phoneOpt.map(_.natnCode)
    val areaCode = phoneOpt.flatMap(_.areaCode)
    val phoneNumber = phoneOpt.map(_.phoneNumber)

    contact match {

      case CompleteMissingPersonContact(_, _, name, _, street, city,_) =>
        val pName: Name = parseName(name)
        val firstName = pName.first
        val lastName = pName.last

        SpremrgRow(
          pidm,
          priority,
          lastName, firstName,
          Some(street), Some(city), Some(zip),
          nationCode, areaCode, phoneNumber,
          relationship,
          time, dataOrigin, userId
        )

      case OptOut(_) =>
        val firstName = "OPTED"
        val lastName = "OUT"

        SpremrgRow(
          pidm, priority,
          lastName, firstName,
          None, None, None,
          nationCode, areaCode, phoneNumber,
          relationship,
          time, dataOrigin, userId
        )
    }
  }

  def parseZip(missingPersonContact: FinishedMissingPersonContact): Xor[MissingPersonContact, String] =
    missingPersonContact match {
      case OptOut(_) => Xor.Right("")
      case CompleteMissingPersonContact(_, _ , _, _ , _ , _ , zip) =>
        if ( zip.length <= 30) Xor.Right(zip)
        else Xor.Left(missingPersonContact)
    }


  def parseName(string: String): Name = {
    Name(string.takeWhile(_ != ' '), string.dropWhile(_ != ' ').drop(1))
  }

  def parsePhone(
                  missingPersonContact: FinishedMissingPersonContact
                ): Xor[FinishedMissingPersonContact, Option[PhoneNumber]] =
    missingPersonContact match {
      case CompleteMissingPersonContact(_, _ , _, number, _, _, _) =>
        number.replace("+", "").replace(".", "-").replace(" ", "-") match {
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
    case OptOut(_) =>
      Xor.Right(None)
    }

}
