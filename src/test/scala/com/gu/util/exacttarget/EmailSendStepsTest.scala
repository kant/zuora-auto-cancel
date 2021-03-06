package com.gu.util.exacttarget

import com.gu.util.config.ETConfig.ETSendId
import com.gu.util.config.Stage
import org.scalatest.{FlatSpec, Matchers}
import com.gu.util.reader.Types.ApiGatewayOp.ContinueProcessing

class EmailSendStepsTest extends FlatSpec with Matchers {

  def makeMessage(recipient: String): Message = {
    Message(
      To = ToDef(
        Address = recipient,
        SubscriberKey = recipient,
        ContactAttributes = ContactAttributesDef(
          SubscriberAttributes = SubscriberAttributesDef(
            subscriber_id = "subIdValue",
            product = "productValue",
            payment_method = "paymentMethodValue",
            card_type = "cardTypeValue",
            card_expiry_date = "cardExpiryValue",
            first_name = "firstNameValue",
            last_name = "lastNameValue",
            primaryKey = PaymentId("paymentId"),
            price = "49.0 GBP",
            serviceStartDate = "31 January 2016",
            serviceEndDate = "31 January 2017"
          )
        )
      )
    )
  }

  private val guardian = "john.duffell@guardian.co.uk"
  private val public = "john.duffell@gutools.co.uk" // non gu

  def tryEmail(isProd: Boolean, email: String, expectedEmail: Boolean) = {

    val req = EmailRequest(
      etSendId = ETSendId("etSendId"),
      makeMessage(email)
    )

    val stage = Stage(if (isProd) "PROD" else "CODE")

    var varAttempted: Boolean = false

    val send = EmailSendSteps(
      sendEmail = req => {
        varAttempted = true
        ContinueProcessing(()) // always success
      },
      filterEmail = FilterEmail.apply(stage)
    )_

    send(req)

    varAttempted should be(expectedEmail)
  }

  "emailer" should "send an email to any address in prod" in {

    tryEmail(isProd = true, email = public, expectedEmail = true)
    tryEmail(isProd = false, email = public, expectedEmail = false)
    tryEmail(isProd = false, email = guardian, expectedEmail = true)
  }

}
