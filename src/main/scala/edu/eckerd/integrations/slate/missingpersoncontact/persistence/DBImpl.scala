package edu.eckerd.integrations.slate.missingpersoncontact.persistence

//import edu.eckerd.integrations.slate.missingpersoncontact.utils.ConfigurationModuleImpl
import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile

/**
 * Created by davenpcm on 7/7/16.
 */
trait DBImpl extends HasDB with DBFunctions
