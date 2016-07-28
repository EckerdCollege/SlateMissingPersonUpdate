package edu.eckerd.integrations.slate.missingpersoncontact.model

/**
  * Created by davenpcm on 7/27/16.
  */
sealed trait MissingPersonContact{
  val BannerID: String
}

case class MissingPersonResponse(
                                  BannerID: String,
                                  Relationship: String,
                                  Name: String,
                                  Cell: Option[String],
                                  AddressStreet: Option[String],
                                  AddressCity: Option[String],
                                  AddressPostal: Option[String]
                                ) extends MissingPersonContact

sealed trait FinishedMissingPersonContact extends MissingPersonContact

case class OptOut(
                   BannerID: String
                 ) extends FinishedMissingPersonContact

case class CompleteMissingPersonContact(
                                         BannerID: String,
                                         Relationship: String,
                                         Name: String,
                                         Cell: String,
                                         AddressStreet: String,
                                         AddressCity: String,
                                         AddressPostal: String
                                       ) extends FinishedMissingPersonContact

