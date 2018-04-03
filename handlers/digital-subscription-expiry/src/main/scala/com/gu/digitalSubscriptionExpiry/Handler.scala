package com.gu.identityBackfill

import java.io.{InputStream, OutputStream}

import com.amazonaws.services.lambda.runtime.Context
import com.gu.effects.RawEffects
import com.gu.util.Config
import com.gu.util.apigateway.{ApiGatewayHandler, ApiGatewayRequest, ApiGatewayResponse}
import com.gu.util.apigateway.ApiGatewayHandler.{LambdaIO, Operation}
import com.gu.util.reader.Types.FailableOp
import com.gu.util.zuora.ZuoraRestConfig
import play.api.libs.json.{Json, Reads}

import scalaz.{-\/}

object Handler {

  // this is the entry point
  // it's referenced by the cloudformation so make sure you keep it in step
  // it's the only part you can't test of the handler
  def apply(inputStream: InputStream, outputStream: OutputStream, context: Context): Unit =
    runWithEffects(RawEffects.createDefault, LambdaIO(inputStream, outputStream, context))

  case class StepsConfig(
    zuoraRestConfig: ZuoraRestConfig
  )
  implicit val stepsConfigReads: Reads[StepsConfig] = Json.reads[StepsConfig]

  def runWithEffects(rawEffects: RawEffects, lambdaIO: LambdaIO): Unit = {
    def operation: Config[StepsConfig] => Operation =
      config => {
        def steps(apiGatewayRequest: ApiGatewayRequest): FailableOp[Unit] = {
          println("hello")
          -\/(ApiGatewayResponse.successfulExecution)
        }
        Operation.noHealthcheck(steps)
      }
    ApiGatewayHandler.default[StepsConfig](operation, lambdaIO).run((rawEffects.stage, rawEffects.s3Load(rawEffects.stage)))
  }

}

