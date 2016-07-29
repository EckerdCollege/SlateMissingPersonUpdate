package edu.eckerd.integrations.slate.missingpersoncontact.persistence


import java.sql.Timestamp

import edu.eckerd.integrations.slate.missingpersoncontact.methods.MissingPersonMethods
import edu.eckerd.integrations.slate.missingpersoncontact.model.{MissingPersonResponse, OptOut}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.exact
import scala.math.BigDecimal
/**
  * Created by davenpcm on 7/28/16.
  */
class PersistenceTests extends FlatSpec with Matchers with MissingPersonMethods with DBImpl  {
  override def pidmResponder: PidmResponder = (s: String) => Future.successful(Some(s.toInt))
  override def dbUpdateResponder: UpdateResponder = UpdateDB
  override def emailResponder: EmailResponder = (s: String) => Future.successful(())
  override def timeResponder: Timestamp = Timestamp.valueOf("2016-07-28 10:10:10.0")
  import dbConfig.driver.api._

  "Spremrg" should "be able to be created successfully" in {
    Await.result(db.run(sqlu"""CREATE SCHEMA SATURN"""), 2.seconds) should be (0)
    Await.result(db.run(Spremrg.schema.create), 2.seconds) should be (())
  }

  "UpdateDB" should "Be Able To Add A Row" in {
    val id = 100
    val row1 = createRow(OptOut(s"${id.toString}"), None, id, None)
    Await.result(UpdateDB(row1), 2.seconds) should be (1)
    Await.result(db.run(Spremrg.filter(_.spremrgPidm === id).result.head), 2.seconds) should be (row1)
  }

  it should "update the record if it exists" in {
    val id = 100
    val row1 = createRow(
      MissingPersonResponse(s"${id.toString}", "8", "First Last", "Cell", "Street", "City", "Zip"),
      None,
      id,
      None
    )
    Await.result(UpdateDB(row1), 2.seconds) should be (1)

    Await.result(db.run(Spremrg.filter(_.spremrgPidm === id).result.head), 2.seconds) should be (row1)
  }

  "QueryIfEmergencyContactExists" should "return true if the row already exists" in {
    val id = 100
    val row1 = createRow(OptOut(s"${id.toString}"), None, id, None)
    Await.result(queryIfEmergencyContactExists(row1), 2.seconds) should be (true)
  }

  it should "return false if the row does not exist" in {
    val id = 200
    val row1 = createRow(OptOut(s"${id.toString}"), None, id, None)
    Await.result(queryIfEmergencyContactExists(row1), 2.seconds) should be (false)
  }

}
