package com.gu.zuora.retention.updateAccounts

import java.io.{InputStream, OutputStream}

import com.amazonaws.services.lambda.runtime.Context
import com.gu.effects.RawEffects
import com.gu.util.apigateway.ApiGatewayHandler.LambdaIO
import com.gu.util.config.ConfigReads.ConfigFailure
import com.gu.util.config.{Config, LoadConfig, Stage}
import com.gu.util.handlers.{JsonHandler, LambdaException}
import com.gu.util.zuora.RestRequestMaker.{ClientFailableOp, Requests}
import com.gu.util.zuora.{ZuoraRestConfig, ZuoraRestRequestMaker}
import okhttp3.{Request, Response}
import play.api.libs.json.{JsSuccess, Json, Reads}
import scalaz.{-\/, \/, \/-}
import UpdateAccountsResponse._
import UpdateAccountsRequest._
import com.gu.zuora.retention.filterCandidates.S3Iterator
import com.gu.zuora.retention.updateAccounts.SetDoNotProcess.UpdateRequestBody._

import scala.util.{Failure, Success, Try}

object Handler {

  case class StepsConfig(zuoraRestConfig: ZuoraRestConfig)

  implicit val stepsConfigReads: Reads[StepsConfig] = Json.reads[StepsConfig]
  type SetDoNotProcess = String => ClientFailableOp[Unit]
  type GetRemainingTime = () => Int

  def getZuoraRequestMaker(
    response: Request => Response,
    stage: Stage,
    s3Load: Stage => ConfigFailure \/ String
  ): Try[Requests] = for {
    config <- toTry(LoadConfig.default[StepsConfig](implicitly)(stage, s3Load(stage)))
  } yield ZuoraRestRequestMaker(response, config.stepsConfig.zuoraRestConfig)

  def operation(
    s3Iterator: String => Try[Iterator[String]],
    updateAccounts: (String, AccountIdIterator) => Try[UpdateAccountsResponse]
  )(request: UpdateAccountsRequest): Try[UpdateAccountsResponse] = for {
    linesIterator <- s3Iterator(request.uri)
    accountIdsIterator <- AccountIdIterator(linesIterator, request.nextIndex.getOrElse(0))
    response <- updateAccounts(request.uri, accountIdsIterator)
    _ <- failIfNoProgress(request, response)
  } yield (response)

  def toTry(res: ConfigFailure \/ Config[StepsConfig]) = res match {
    case -\/(configError) => Failure(LambdaException(configError.error))
    case \/-(config) => Success(config)
  }

  def failIfNoProgress(request: UpdateAccountsRequest, response: UpdateAccountsResponse): Try[Unit] =
    if (!response.done && request.nextIndex == response.nextIndex) Failure(LambdaException("no accounts processed in execution!")) else Success(())

  // this is the entry point
  // it's referenced by the cloudformation so make sure you keep it in step
  def apply(inputStream: InputStream, outputStream: OutputStream, context: Context): Unit = {
    val getRemainingTime = context.getRemainingTimeInMillis _

    val s3Iterator = S3Iterator.apply(RawEffects.fetchContent) _

    implicit val unitReads: Reads[Unit] = Reads(_ => JsSuccess(()))

    def wiredOperation(updateAccountsRequest: UpdateAccountsRequest): Try[UpdateAccountsResponse] = for {
      zuoraRequests <- getZuoraRequestMaker(RawEffects.response, RawEffects.stage, RawEffects.s3Load)
      setDoNotProcess = SetDoNotProcess(zuoraRequests.put) _
      operation <- operation(
        s3Iterator,
        UpdateAccounts(setDoNotProcess, getRemainingTime) _
      )(updateAccountsRequest)
    } yield operation

    JsonHandler(
      lambdaIO = LambdaIO(inputStream, outputStream, context),
      operation = wiredOperation
    )
  }
}

