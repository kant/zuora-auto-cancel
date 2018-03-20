package com.gu.stripeCustomerSourceUpdated

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import play.api.libs.json.{JsResult, JsSuccess, Json}
import SourceUpdatedCallout._

class StripeCustomerUpdatedReadsTest extends FlatSpec {

  "SourceUpdatedCallout" should "deserialise correctly from a valid event" in {

    val validEventJson =
      """
        |{
        |  "id": "evt_abc123",
        |  "object": "event",
        |  "api_version": "2017-08-15",
        |  "created": 1513761863,
        |  "data": {
        |    "object": {
        |      "id": "card_def456",
        |      "object": "card",
        |      "address_city": null,
        |      "address_country": null,
        |      "address_line1": null,
        |      "address_line1_check": null,
        |      "address_line2": null,
        |      "address_state": null,
        |      "address_zip": null,
        |      "address_zip_check": null,
        |      "brand": "Visa",
        |      "country": "US",
        |      "customer": "cus_ghi789",
        |      "cvc_check": "unchecked",
        |      "dynamic_last4": null,
        |      "exp_month": 7,
        |      "exp_year": 2020,
        |      "fingerprint": "abab1212",
        |      "funding": "credit",
        |      "last4": "1234",
        |      "metadata": {
        |      },
        |      "name": null,
        |      "tokenization_method": null
        |    },
        |    "previous_attributes": {
        |      "exp_month": 5,
        |      "exp_year": 2018
        |    }
        |  },
        |  "livemode": true,
        |  "pending_webhooks": 1,
        |  "request": {
        |    "id": null,
        |    "idempotency_key": null
        |  },
        |  "type": "customer.source.updated"
        |}
      """.stripMargin

    val expected: JsResult[SourceUpdatedCallout] = JsSuccess(
      SourceUpdatedCallout(
        id = StripeEventId("evt_abc123"),
        data = EventData(
          `object` = EventDataObject(
            id = StripeSourceId("card_def456"),
            brand = StripeBrand.Visa,
            country = StripeCountry("US"),
            customer = StripeCustomerId("cus_ghi789"),
            expiry = StripeExpiry(exp_month = 7, exp_year = 2020),
            last4 = StripeLast4("1234")
          )
        )
      )
    )

    val event: JsResult[SourceUpdatedCallout] = Json.parse(validEventJson).validate[SourceUpdatedCallout]

    event should be(expected)
  }

}
