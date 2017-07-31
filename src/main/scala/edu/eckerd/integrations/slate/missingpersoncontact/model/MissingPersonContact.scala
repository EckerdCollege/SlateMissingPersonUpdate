package edu.eckerd.integrations.slate.missingpersoncontact.model

/**
  * Created by davenpcm on 7/27/16.
  */
sealed trait MissingPersonContact {
  val BannerID: String
}

object MissingPersonContact{
  def apply(BannerID: String,
            Relationship: String,
            Name: String,
            Cell: String,
            AddressStreet: String,
            AddressCity: String,
            AddressState: String,
            AddressPostal: String): MissingPersonContact = Name match {
    case optOutConvert if optOutConvert == "OPTION DECLINED" =>
      OptOut(BannerID)
    case _ => MissingPersonResponse(BannerID, Relationship, Name, Cell, AddressStreet, AddressCity, AddressState, AddressPostal)
  }

  def apply(missingPersonResponse: MissingPersonResponse): MissingPersonContact =
    MissingPersonContact.apply(
      missingPersonResponse.BannerID,
      missingPersonResponse.Relationship,
      missingPersonResponse.Name,
      missingPersonResponse.Cell,
      missingPersonResponse.AddressStreet,
      missingPersonResponse.AddressCity,
      missingPersonResponse.AddressState,
      missingPersonResponse.AddressPostal
    )

}

case class OptOut(
                   BannerID: String
                 ) extends MissingPersonContact

case class MissingPersonResponse(
                                         BannerID: String,
                                         Relationship: String,
                                         Name: String,
                                         Cell: String,
                                         AddressStreet: String,
                                         AddressCity: String,
                                         AddressState: String,
                                         AddressPostal: String
                                       ) extends MissingPersonContact

