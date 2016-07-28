package edu.eckerd.integrations.slate.missingpersoncontact.mail

import javax.mail.internet.InternetAddress

import courier.{Envelope, Mailer, Multipart}
import Mailer._
import eri.commons.config.SSConfig

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
/**
  * Created by davenpcm on 7/28/16.
  */
trait CourierFunctions {
  import courier.Defaults.session
  /**
    * This is the implicit class which can convert automatically a string to this class which has these functions
    * available to it. Therefore taking  an normal string and where this class is in scope giving it additional
    * functionality. Such as being an internet address with the write calls.
    * @param name The first part of an email or the full address if the caller intends to call addr
    */
  implicit class addr(name: String) {
    def `@`(domain: String): InternetAddress = new InternetAddress(s"$name@$domain")
    def at = `@` _
    /** In case whole string is email address already */
    def addr = new InternetAddress(name)
  }

  def sendEmailForConfig(subject: String, config: String = "courier")(content: String)
                        (implicit ec: ExecutionContext): Future[Unit] ={
    val conf = new SSConfig(config)
    val senderEmail = conf.senderEmail.as[String]
    val senderPassword = conf.senderPassword.as[String]
    val recipientEmail = conf.recipientEmail.as[String]
    val smtpServer = conf.smtpServer.as[String]
    val smtpPort = conf.smtpPort.asOption[Int].getOrElse(587)
    val authorize = conf.authorize.asOption[Boolean].getOrElse(true)
    val startTls = conf.startTls.asOption[Boolean].getOrElse(true)

    println("The Email Is Sending")

    sendEmail(senderEmail, senderPassword, recipientEmail, subject, content, smtpServer, smtpPort, authorize, startTls)
  }

  def sendEmail(
                 senderEmail: String,
                 senderPassword:String,
                 recipientEmail: String,
                 subject: String,
                 content: String,
                 smtpServer: String,
                 smtpPort: Int,
                 authorize: Boolean,
                 startTtls: Boolean
               )(implicit ec: ExecutionContext): Future[Unit] ={

    val mailer = Mailer(smtpServer, smtpPort)
      .auth(authorize)
      .as(senderEmail, senderPassword)
      .startTtls(startTtls)

    val envelope : Envelope = Envelope
      .from(senderEmail.addr)
      .to(recipientEmail.addr)
      .subject(subject)
      .content(
        Multipart().html(content)
      )

    mailer()(envelope)
  }

}
