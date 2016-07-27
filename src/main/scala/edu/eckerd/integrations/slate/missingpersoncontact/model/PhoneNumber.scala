package edu.eckerd.integrations.slate.missingpersoncontact.model

/**
  * Created by davenpcm on 7/27/16.
  */

/**
  * What it takes to be a phone number
  * @param natnCode A Nation Code Is Required
  * @param areaCode An area code is optional because we don't all live in the United States
  * @param phoneNumber A number
  */
case class PhoneNumber(
                        natnCode: String,
                        areaCode: Option[String],
                        phoneNumber: String
                      )