package edu.eckerd.integrations.slate.missingpersoncontact

import java.sql.Timestamp

import scala.concurrent.ExecutionContext.Implicits.global
import edu.eckerd.integrations.slate.missingpersoncontact.request.jsonProtocol._
import edu.eckerd.integrations.slate.core.Request
import edu.eckerd.integrations.slate.missingpersoncontact.mail.CourierFunctions
import edu.eckerd.integrations.slate.missingpersoncontact.methods.MissingPersonMethods
import edu.eckerd.integrations.slate.missingpersoncontact.model.{MissingPersonContact, MissingPersonResponse}
import edu.eckerd.integrations.slate.missingpersoncontact.persistence.DBImpl
import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile

import scala.concurrent.Await
import scala.concurrent.duration._
/**
  * Created by davenpcm on 7/27/16.
  */
object MainApplication extends App  with MissingPersonMethods with CourierFunctions with DBImpl{

  override implicit val dbConfig: DatabaseConfig[JdbcProfile] = DatabaseConfig.forConfig("oracle")
  override def pidmResponder: PidmResponder = getPidmFromBannerID
  override def dbUpdateResponder: UpdateResponder = UpdateDB
  override def emailResponder: EmailResponder = sendEmailForConfig("Unparsed Missing Person Contacts")(_)
  override def timeResponder: Timestamp = new java.sql.Timestamp(new java.util.Date().getTime)

  val responseF = Request.SingleRequestForConfig[MissingPersonContact]("slate")
      .flatMap(ProcessResponses)

  val response = Await.result(responseF, 60.seconds)


}
