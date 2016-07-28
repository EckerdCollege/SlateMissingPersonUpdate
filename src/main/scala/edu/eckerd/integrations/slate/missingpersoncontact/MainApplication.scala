package edu.eckerd.integrations.slate.missingpersoncontact

import java.sql.Timestamp

import scala.concurrent.ExecutionContext.Implicits.global
import edu.eckerd.integrations.slate.missingpersoncontact.request.jsonProtocol._
import edu.eckerd.integrations.slate.core.Request
import edu.eckerd.integrations.slate.missingpersoncontact.mail.CourierFunctions
import edu.eckerd.integrations.slate.missingpersoncontact.methods.MissingPersonMethods
import edu.eckerd.integrations.slate.missingpersoncontact.model.{MissingPersonContact, MissingPersonResponse}
import edu.eckerd.integrations.slate.missingpersoncontact.persistence.DBImpl

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
/**
  * Created by davenpcm on 7/27/16.
  */
object MainApplication extends App with DBImpl with MissingPersonMethods with CourierFunctions {

  override def pidmResponder: PidmResponder = getPidmFromBannerID
  override def dbUpdateResponder: UpdateResponder = UpdateDB
  override def emailResponder: EmailResponder = sendEmailForConfig("Unparsed Missing Person Contacts")(_)
  override def timeResponder: Timestamp = new java.sql.Timestamp(new java.util.Date().getTime)

  val responseF = Request.SingleRequestForConfig[MissingPersonContact]("slate")
      .flatMap(ProcessResponses)

//    .flatMap(partitionResponses)
//  .flatMap(Future.traverse(_)(TransformToRowOrEmail))

  val response = Await.result(responseF, 60.seconds)

//  response.foreach(println)

}
