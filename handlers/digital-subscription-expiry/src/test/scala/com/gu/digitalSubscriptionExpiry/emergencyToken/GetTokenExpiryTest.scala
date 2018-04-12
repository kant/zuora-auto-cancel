package com.gu.digitalSubscriptionExpiry.emergencyToken

import com.gu.cas.{PrefixedTokens, SevenDay}
import com.gu.digitalSubscriptionExpiry.{Expiry, ExpiryType, SuccessResponse}
import com.gu.util.apigateway.ResponseModels.{ApiResponse, Headers}
import org.joda.time.format.DateTimeFormat
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.Json
import scalaz.{-\/, \/-}

class GetTokenExpiryTest extends FlatSpec with Matchers {

  val getTokenExpiry = {
    val codec = PrefixedTokens(secretKey = "secret", emergencySubscriberAuthPrefix = "G99")
    GetTokenExpiry(EmergencyTokens("G99", codec))(_)
  }

  it should "return right for invalid token" in {
    getTokenExpiry("invalidToken").shouldBe(\/-(()))
  }
  it should "read valid token" in {

    val dateFormatter = DateTimeFormat.forPattern("dd/MM/yyyy")

    val expiry = Expiry(
      expiryDate = dateFormatter.parseLocalDate("21/07/2017"),
      expiryType = ExpiryType.SUB,
      subscriptionCode = Some(SevenDay),
      provider = Some("G99")
    )

    val responseBody = Json.prettyPrint(Json.toJson(SuccessResponse(expiry)))
    val expectedResponse = -\/(ApiResponse("200", new Headers, responseBody))

    getTokenExpiry("G99IZXCEZLYF").shouldBe(expectedResponse)
  }
}

