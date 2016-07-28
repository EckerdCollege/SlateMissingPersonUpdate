package edu.eckerd.integrations.slate.missingpersoncontact

import java.sql.Timestamp

import scala.concurrent.ExecutionContext.Implicits.global
import edu.eckerd.integrations.slate.missingpersoncontact.request.jsonProtocol._
import edu.eckerd.integrations.slate.core.Request
import edu.eckerd.integrations.slate.missingpersoncontact.methods.MissingPersonMethods
import edu.eckerd.integrations.slate.missingpersoncontact.model.MissingPersonResponse
import edu.eckerd.integrations.slate.missingpersoncontact.persistence.DBImpl

import scala.concurrent.Await
import scala.concurrent.duration._
/**
  * Created by davenpcm on 7/27/16.
  */
object MainApplication extends App with DBImpl with MissingPersonMethods {

  override def pidmResponder: PidmResponder = getPidmFromBannerID
  override def dbUpdateResponder: UpdateResponder = UpdateDB
  override def emailResponder: EmailResponder = ???
  override def timeResponder: Timestamp = new java.sql.Timestamp(new java.util.Date().getTime)

  val responseF = Request.SingleRequestForConfig[MissingPersonResponse]("slate")
  .flatMap(ProcessResponses)

  val response = Await.result(responseF, 60.seconds)

}
