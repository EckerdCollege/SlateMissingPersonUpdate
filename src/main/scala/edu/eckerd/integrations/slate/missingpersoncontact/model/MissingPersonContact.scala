package edu.eckerd.integrations.slate.missingpersoncontact.model

/**
  * Created by davenpcm on 7/27/16.
  */
case class MissingPersonContact(
                               BannerID: String,
                               Relationship: String,
                               Name: String,
                               Cell: Option[String],
                               AddressStreet: Option[String],
                               AddressCity: Option[String],
                               AddressPostal: Option[String]
                               )