package com.gu.identityRetention

import java.time.LocalDate

import com.gu.effects.{RawEffects, S3ConfigLoad}
import com.gu.identityRetention.Handler.StepsConfig
import com.gu.identityRetention.Types.AccountId
import com.gu.test.EffectsTest
import com.gu.util.config.{LoadConfig, Stage}
import com.gu.util.zuora.{ZuoraQuery, ZuoraRestRequestMaker}
import org.scalatest.{FlatSpec, Matchers}
import scalaz.\/-
import scalaz.syntax.std.either._

class SubscriptionsForAccountsEffectsTest extends FlatSpec with Matchers {

  it should "successfull query multiple accounts" taggedAs EffectsTest in {

    val testAccountIds = List(
      AccountId("2c92c0f86371efdc0163871a9ad72274"),
      AccountId("2c92c0f86371f0360163871d94eb0e68")
    )

    val expectedEndDates = List(
      LocalDate.of(2019, 5, 21),
      LocalDate.of(2018, 4, 4)
    )

    val actual = for {
      configAttempt <- S3ConfigLoad.load(Stage("DEV")).toEither.disjunction
      config <- LoadConfig.parseConfig[StepsConfig](configAttempt)
      zuoraQuerier = ZuoraQuery(ZuoraRestRequestMaker(RawEffects.response, config.stepsConfig.zuoraRestConfig))
      subsForAccounts = SubscriptionsForAccounts(zuoraQuerier) _
      subs <- subsForAccounts(testAccountIds).toDisjunction
    } yield {
      subs
    }
    actual.map(_.map(_.TermEndDate)) should be(\/-(expectedEndDates))

  }

}
