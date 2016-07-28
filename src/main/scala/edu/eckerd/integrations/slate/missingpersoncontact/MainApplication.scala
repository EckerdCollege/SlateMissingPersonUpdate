package edu.eckerd.integrations.slate.missingpersoncontact

import scala.concurrent.ExecutionContext.Implicits.global
import edu.eckerd.integrations.slate.missingpersoncontact.request.jsonProtocol._
import edu.eckerd.integrations.slate.core.Request
import edu.eckerd.integrations.slate.missingpersoncontact.methods.MissingPersonMethods
import edu.eckerd.integrations.slate.missingpersoncontact.model.MissingPersonResponse
import edu.eckerd.integrations.slate.missingpersoncontact.persistence.DBImpl
import cats.implicits._
import cats.data.Xor

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
/**
  * Created by davenpcm on 7/27/16.
  */
object MainApplication extends App with MissingPersonMethods with DBImpl {
  def pidmResponder = getPidmFromBannerID

  val responseF = Request.SingleRequestForConfig[MissingPersonResponse]("slate")
  .flatMap( seq =>
    Future.traverse(seq)(TranformToRowOrEmail)
  )

  val response = Await.result(responseF, 60.seconds)

//  val updatedResponse = response.map(_.map(row => Await.result(UpdateDB(row), 2.seconds)))
//  updatedResponse.foreach(println)

}
