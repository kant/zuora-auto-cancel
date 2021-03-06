package com.gu.digitalSubscriptionExpiry.zuora

import com.gu.digitalSubscriptionExpiry.zuora.GetAccountSummary.{AccountId, AccountSummaryResult}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import play.api.libs.json.{JsResult, JsSuccess, Json}

class AccountSummaryResultDeserialiseTest extends FlatSpec {

  it should "deserialise correctly Account with null postcode" in {

    val expected: JsResult[AccountSummaryResult] = JsSuccess(
      AccountSummaryResult(
        accountId = AccountId("testId"),
        billToLastName = "billingLastName",
        billToPostcode = None,
        soldToLastName = "soldToLastName",
        soldToPostcode = None
      )

    )

    val testAccount = getTestAccount(None, None)
    val event: JsResult[AccountSummaryResult] = Json.parse(testAccount).validate[AccountSummaryResult]

    event should be(expected)
  }

  it should "deserialise correctly Account with empty string postcode" in {

    val expected: JsResult[AccountSummaryResult] = JsSuccess(
      AccountSummaryResult(
        accountId = AccountId("testId"),
        billToLastName = "billingLastName",
        billToPostcode = Some(""),
        soldToLastName = "soldToLastName",
        soldToPostcode = Some("")
      )

    )

    val testAccount = getTestAccount(Some(""), Some(""))
    val event: JsResult[AccountSummaryResult] = Json.parse(testAccount).validate[AccountSummaryResult]

    event should be(expected)
  }

  it should "deserialise correctly Account with postcode" in {

    val expected: JsResult[AccountSummaryResult] = JsSuccess(
      AccountSummaryResult(
        accountId = AccountId("testId"),
        billToLastName = "billingLastName",
        billToPostcode = Some("billtoPostcodeValue"),
        soldToLastName = "soldToLastName",
        soldToPostcode = Some("SoldToPostcodeValue")
      )

    )

    val testAccount = getTestAccount(
      billToPostcode = Some("billtoPostcodeValue"),
      soldToPostcode = Some("SoldToPostcodeValue")
    )
    val event: JsResult[AccountSummaryResult] = Json.parse(testAccount).validate[AccountSummaryResult]

    event should be(expected)
  }

  def getTestAccount(billToPostcode: Option[String] = None, soldToPostcode: Option[String]) = {
    def toFieldValue(o: Option[String]) = o.map(s => '"' + s + '"').getOrElse("null")

    s"""
      {
       |    "basicInfo": {
       |        "id": "testId",
       |        "name": "testName",
       |        "accountNumber": "TestAccountNumber",
       |        "notes": null,
       |        "status": "Active",
       |        "crmId": "someID",
       |        "batch": "Batch1",
       |        "invoiceTemplateId": "templateID",
       |        "communicationProfileId": null,
       |        "IdentityId__c": "12344",
       |        "sfContactId__c": "00xdxE00000NKaRgQAL",
       |        "CCURN__c": null,
       |        "NonStandardDataReason__c": null,
       |        "salesRep": null,
       |        "parentId": null
       |    },
       |    "billingAndPayment": {
       |        "billCycleDay": 16,
       |        "currency": "GBP",
       |        "paymentTerm": "Due Upon Receipt",
       |        "paymentGateway": "Stripe 2",
       |        "invoiceDeliveryPrefsPrint": false,
       |        "invoiceDeliveryPrefsEmail": true,
       |        "additionalEmailAddresses": []
       |    },
       |    "metrics": {
       |        "balance": 0,
       |        "totalInvoiceBalance": 0,
       |        "creditBalance": 0,
       |        "contractedMrr": 29.2
       |    },
       |    "billToContact": {
       |        "address1": "123 fake st",
       |        "address2": "",
       |        "city": "fakeville",
       |        "country": "United Kingdom",
       |        "county": "",
       |        "fax": "",
       |        "firstName": "billingFirstName",
       |        "homePhone": "",
       |        "lastName": "billingLastName",
       |        "mobilePhone": "",
       |        "nickname": "",
       |        "otherPhone": "",
       |        "otherPhoneType": "Work",
       |        "personalEmail": "",
       |        "state": "",
       |        "taxRegion": "",
       |        "workEmail": "test.testerson@gu.com",
       |        "workPhone": "",
       |        "zipCode": ${toFieldValue(billToPostcode)},
       |        "SpecialDeliveryInstructions__c": null,
       |        "Title__c": "Mr"
       |    },
       |    "soldToContact": {
       |        "address1": "123 fake st",
       |        "address2": "",
       |        "city": "fakeville",
       |        "country": "United Kingdom",
       |        "county": "",
       |        "fax": "",
       |        "firstName": "soldToFirstName",
       |        "homePhone": "",
       |        "lastName": "soldToLastName",
       |        "mobilePhone": "",
       |        "nickname": "",
       |        "otherPhone": "",
       |        "otherPhoneType": "Work",
       |        "personalEmail": "",
       |        "state": "",
       |        "taxRegion": "",
       |        "workEmail": "test.testerson@gu.com",
       |        "workPhone": "",
       |        "zipCode": ${toFieldValue(soldToPostcode)},
       |        "SpecialDeliveryInstructions__c": null,
       |        "Title__c": "Mr"
       |    },
       |    "taxInfo": null,
       |    "success": true
       |}
      """.stripMargin
  }
}
