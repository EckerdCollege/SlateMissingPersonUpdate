package edu.eckerd.integrations.slate.missingpersoncontact

import scala.concurrent.ExecutionContext.Implicits.global
import edu.eckerd.integrations.slate.missingpersoncontact.request.jsonProtocol._
import edu.eckerd.integrations.slate.core.Request
import edu.eckerd.integrations.slate.missingpersoncontact.methods.MissingPersonMethodsUnsafe
import edu.eckerd.integrations.slate.missingpersoncontact.model.MissingPersonResponse

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
/**
  * Created by davenpcm on 7/27/16.
  */
object MainApplication extends App with MissingPersonMethodsUnsafe {
  val ec = global

  val responseF = Request.SingleRequestForConfig[MissingPersonResponse]("slate")
  .flatMap(ProcessResponses)

  val response = Await.result(responseF, 60.seconds)

}
