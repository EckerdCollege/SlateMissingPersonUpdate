package edu.eckerd.integrations.slate.missingpersoncontact.methods

import edu.eckerd.integrations.slate.missingpersoncontact.persistence.DBImpl

/**
  * Created by davenpcm on 7/27/16.
  */
trait MissingPersonMethodsUnsafe extends MissingPersonMethods with DBImpl {
  override def pidmResponder: PidmResponder = getPidmFromBannerID
  override def updateResponder: UpdateResponder = UpdateDB
  override def emailResponder: EmailResponder = ???
}
