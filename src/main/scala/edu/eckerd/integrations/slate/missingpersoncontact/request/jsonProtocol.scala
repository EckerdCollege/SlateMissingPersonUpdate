package edu.eckerd.integrations.slate.missingpersoncontact.request

import edu.eckerd.integrations.slate.core.DefaultJsonProtocol
import edu.eckerd.integrations.slate.missingpersoncontact.model.MissingPersonContact

/**
  * Created by davenpcm on 7/27/16.
  */
object jsonProtocol extends DefaultJsonProtocol{
  implicit val NameIDFormat = jsonFormat7(MissingPersonContact)
}
