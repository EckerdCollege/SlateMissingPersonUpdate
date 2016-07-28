package edu.eckerd.integrations.slate.missingpersoncontact.persistence

import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile

/**
 * Created by davenpcm on 7/7/16.
 */
trait HasDB {
  implicit val dbConfig: DatabaseConfig[JdbcProfile] = DatabaseConfig.forConfig("oracle")
  implicit val profile = dbConfig.driver
  implicit val db = dbConfig.db
}
