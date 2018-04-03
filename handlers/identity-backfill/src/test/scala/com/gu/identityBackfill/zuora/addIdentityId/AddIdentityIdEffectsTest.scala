package com.gu.identityBackfill.zuora.addIdentityId

import com.gu.effects.{ConfigLoad, RawEffects}
import com.gu.identityBackfill.Handler.StepsConfig
import com.gu.identityBackfill.Types
import com.gu.identityBackfill.Types.IdentityId
import com.gu.identityBackfill.zuora.AddIdentityIdToAccount
import com.gu.test.EffectsTest
import com.gu.util.zuora.ZuoraDeps
import com.gu.util.{Config, Stage}
import org.scalatest.{FlatSpec, Matchers}
import scalaz.\/-
import scalaz.syntax.std.either._

import scala.util.Random

// run this manually
class AddIdentityIdEffectsTest extends FlatSpec with Matchers {

  it should "successfully update the identity id against dev" taggedAs EffectsTest in {

    val unique = s"${Random.nextInt(10000)}"
    val testAccount = Types.AccountId("2c92c0f9624bbc5f016253e573970b16")

    val actual = for {
      configAttempt <- ConfigLoad.load(Stage("DEV")).toEither.disjunction
      config <- Config.parseConfig[StepsConfig](configAttempt)
      zuoraDeps = ZuoraDeps(RawEffects.createDefault.response, config.stepsConfig.zuoraRestConfig)
      _ <- AddIdentityIdToAccount(zuoraDeps)(testAccount, IdentityId(unique))
      identityId <- GetIdentityIdForAccount(zuoraDeps)(testAccount)
    } yield {
      identityId
    }
    actual should be(\/-(IdentityId(unique)))

  }

}