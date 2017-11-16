package com.gu.autoCancel

import com.gu.autoCancel.AutoCancel.ACDeps
import com.gu.effects.RawEffects
import com.gu.util.ETConfig.ETSendKeysForAttempt
import com.gu.util.apigateway.ApiGatewayHandler.StageAndConfigHttp
import com.gu.util.apigateway.{ ApiGatewayRequest, URLParams }
import com.gu.util.reader.Types._
import com.gu.util.zuora.Zuora.DisableAutoPay
import com.gu.util.zuora.ZuoraModels._
import com.gu.util.{ Config, ETConfig, TrustedApiConfig, ZuoraRestConfig }
import okhttp3._
import org.joda.time.LocalDate
import org.scalatest._

import scala.util.Success
import scalaz.{ Reader, \/ }

class AutoCancelStepsTest extends FlatSpec with Matchers {

  val basicInfo = BasicAccountInfo("id123", 11.99)
  val subscription = SubscriptionSummary("id123", "A-S123", "Active")
  val singleOverdueInvoice = Invoice("inv123", LocalDate.now.minusDays(14), 11.99, "Posted")

  "auto cancel" should "turn off auto pay" in {
    var disableAutoPayAccountId: Option[String] = None
    def disableAutoPay: DisableAutoPay = { accountId =>
      disableAutoPayAccountId = Some(accountId)
      WithDependenciesFailableOp.liftT(UpdateAccountResult())
    }
    val autoCancelJson =
      """
        |{"accountId": "AID", "autoPay": "true", "paymentMethodType": "GoldBars"}
      """.stripMargin // should probaly base on a real payload
    val fakeRequest = ApiGatewayRequest(Some(URLParams(None, None, Some("false"))), autoCancelJson)
    val aCDeps = ACDeps(
      disableAutoPay = disableAutoPay,
      getAccountSummary = _ => WithDependenciesFailableOp.liftT(AccountSummary(basicInfo, List(subscription), List(singleOverdueInvoice))),
      updateCancellationReason = _ => WithDependenciesFailableOp.liftT(UpdateSubscriptionResult("subid")),
      cancelSubscription = (_, _) => WithDependenciesFailableOp.liftT(CancelSubscriptionResult(LocalDate.now))
    )
    AutoCancelSteps(aCDeps)(fakeRequest).run.run(new TestingRawEffects(false).configHttp)

    disableAutoPayAccountId should be(Some("AID"))
  }

}

class TestingRawEffects(val isProd: Boolean) {

  var result: Option[Request] = None // !

  val stage = if (isProd) "PROD" else "DEV"

  def response: Request => Response = {
    req =>
      result = Some(req)
      new Response.Builder().request(req).protocol(Protocol.HTTP_1_1).code(1).body(ResponseBody.create(MediaType.parse("text/plain"), "body result test")).build()
  }

  val rawEffects = RawEffects(response, () => stage, _ => Success(""))

  val fakeConfig = Config(stage, TrustedApiConfig("a", "b", "c"), zuoraRestConfig = ZuoraRestConfig("https://ddd", "e@f.com", "ggg"),
    etConfig = ETConfig(stageETIDForAttempt = ETSendKeysForAttempt(Map(0 -> "h")), clientId = "jjj", clientSecret = "kkk"))

  val configHttp = StageAndConfigHttp(response, fakeConfig)

}

object WithDependenciesFailableOp {

  // lifts any plain value all the way in, usually useful in tests
  def liftT[R, T](value: R): WithDeps[T]#FailableOp[R] =
    \/.right(value).toReader[T]

  // lifts any plain value all the way in, usually useful in tests
  def liftR[R, T](value: R): Reader[T, FailableOp[R]] =
    //\/.right(value).toReader[T]
    Reader[T, FailableOp[R]]((_: T) => \/.right(value))
}
