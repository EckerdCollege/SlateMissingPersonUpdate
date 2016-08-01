package edu.eckerd.integrations.slate.missingpersoncontact.persistence

import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile

/**
 * Created by davenpcm on 7/7/16.
 */
trait HasDB {
  implicit val dbConfig: DatabaseConfig[JdbcProfile]
  implicit lazy val profile = dbConfig.driver
  implicit lazy val db = dbConfig.db
}
