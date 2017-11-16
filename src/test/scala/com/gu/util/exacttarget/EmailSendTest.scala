package com.gu.util.exacttarget

import com.gu.autoCancel.WithDependenciesFailableOp
import com.gu.effects.RawEffects
import com.gu.util.ETConfig
import com.gu.util.ETConfig.ETSendKeysForAttempt
import com.gu.util.exacttarget.EmailSend.{ ETS, HUDeps }
import com.gu.util.exacttarget.SalesforceAuthenticate.SalesforceAuth
import com.gu.util.reader.Types._
import okhttp3._
import org.scalatest.{ FlatSpec, Matchers }

import scala.util.Success
import scalaz.{ Reader, \/- }

class EmailSendTest extends FlatSpec with Matchers {

  def makeMessage(recipient: String): Message = {
    Message(
      DataExtensionName = "first-failed-payment-email",
      To = ToDef(
        Address = recipient,
        SubscriberKey = recipient,
        ContactAttributes = ContactAttributesDef(
          SubscriberAttributes = SubscriberAttributesDef(
            SubscriberKey = recipient,
            EmailAddress = recipient,
            subscriber_id = "subIdValue",
            product = "productValue",
            payment_method = "paymentMethodValue",
            card_type = "cardTypeValue",
            card_expiry_date = "cardExpiryValue",
            first_name = "firstNameValue",
            last_name = "lastNameValue",
            paymentId = "paymentId",
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
    def requestBuilder(attempt: Int) = new Request.Builder()
      .url(s"http://$attempt")
      .post(RequestBody.create(MediaType.parse("text/plain"), s"$attempt"))

    val req = EmailRequest(1, makeMessage(email))
    val env = new TestingRawEffectsET(isProd)
    var varAttempt: Option[Int] = None
    EmailSend(HUDeps(
      sendEmail = (attempt, message) => {
      varAttempt = Some(attempt)
      WithDependenciesFailableOp.liftT(())
    }
    ))(req).run.run(env.configHttp)

    varAttempt should be(if (expectedEmail) Some(1) else None)
  }

  "emailer" should "send an email to any address in prod" in {

    tryEmail(isProd = true, email = public, expectedEmail = true)
    tryEmail(isProd = false, email = public, expectedEmail = false)
    tryEmail(isProd = false, email = guardian, expectedEmail = true)
  }

}

class TestingRawEffectsET(val isProd: Boolean) {

  var result: Option[Request] = None // !

  val stage = if (isProd) "PROD" else "CODE"

  def response: Request => Response = {
    req =>
      result = Some(req)
      new Response.Builder().request(req).protocol(Protocol.HTTP_1_1).code(1).body(ResponseBody.create(MediaType.parse("text/plain"), "body result test")).build()
  }

  val rawEffects = RawEffects(response, () => stage, _ => Success(""))

  val fakeETConfig = ETConfig(stageETIDForAttempt = ETSendKeysForAttempt(Map(0 -> "h")), clientId = "jjj", clientSecret = "kkk")

  val configHttp = ETS(response, stage, fakeETConfig)

}
