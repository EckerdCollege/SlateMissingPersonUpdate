package edu.eckerd.integrations.slate.missingpersoncontact.mail

import de.saly.javamail.mock2.MockMailbox
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import javax.mail.{Folder, Session}
import java.util.Properties
import javax.mail.internet.InternetAddress

/**
  * Created by davenpcm on 8/1/16.
  */
class CourierTests extends FlatSpec with Matchers with CourierFunctions {

  "sendEmail" should "send an email when all information is provided" in {
    import scala.collection.JavaConverters._
    val recipient = "test1recipient@test.com"
    val f = sendEmail("test1@test.com", "testpassword", recipient,
      "Test 1", "Test 1 Content",
      "smtp.test.com", 587, true, true
    )
    Await.result(f, 10.seconds)

    val mf = MockMailbox.get(recipient)
    val session = Session.getInstance(new Properties())
    val store = session.getStore("pop3s")
    store.connect(recipient, null)
    val inbox = store.getFolder("INBOX")
    inbox.open(Folder.READ_ONLY)

    inbox.getMessageCount should be (1)
    val message = inbox.getMessage(1)
    message.getSubject should be ("Test 1")
    message.getFrom should be (Array("test1@test.com".addr))
    inbox.close(true)
    store.close()
  }

  "sendEmailForConfig" should "send An Email From a Loaded Configuration" in {
    val f = sendEmailForConfig("Test 2")("Test 2 Content")
    Await.result(f, 10.seconds)

    val mf = MockMailbox.get("testrecipient@test.com")
    val session = Session.getInstance(new Properties())
    val store = session.getStore("pop3s")
    store.connect("testrecipient@test.com", null)
    val inbox = store.getFolder("INBOX")
    inbox.open(Folder.READ_ONLY)

    inbox.getMessageCount should be (1)
    val message = inbox.getMessage(1)
    message.getSubject should be ("Test 2")
    message.getFrom should be (Array("testsender@test.com".addr))
    inbox.close(true)
    store.close()
  }

  "addr" should "conver a string to an internet address" in {
   "chris@test.com".addr should be (new InternetAddress("chris@test.com"))
  }

  "`@`" should "convert two strings into an internet Address" in {
    "chris" `@` "test.com" should be (new InternetAddress("chris@test.com"))
  }

  "at" should "convert two strings into an internet Address" in {
    "chris" at "test.com" should be (new InternetAddress("chris@test.com"))
  }

}
