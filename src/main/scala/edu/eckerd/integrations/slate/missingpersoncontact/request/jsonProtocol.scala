package edu.eckerd.integrations.slate.missingpersoncontact.request

import edu.eckerd.integrations.slate.core.DefaultJsonProtocol
import edu.eckerd.integrations.slate.missingpersoncontact.model.{MissingPersonContact, MissingPersonResponse}

/**
  * Created by davenpcm on 7/27/16.
  */
object jsonProtocol extends DefaultJsonProtocol{
  implicit val MissingPersonResponseFormat = jsonFormat7(MissingPersonResponse)
}
