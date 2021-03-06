package com.gu.zuora.reports

import com.gu.util.Logging
import com.gu.util.apigateway.ApiGatewayHandler.LambdaIO
import com.gu.util.config.ConfigReads.ConfigFailure
import com.gu.util.config.{Config, LoadConfig, Stage}
import com.gu.util.handlers.{LambdaException, ParseRequest, SerialiseResponse}
import com.gu.util.zuora.RestRequestMaker.ClientFailableOp
import com.gu.util.zuora.ZuoraRestConfig
import play.api.libs.json.{Json, Reads, Writes}
import scalaz.Scalaz._
import scalaz._

object ReportsLambda extends Logging {

  case class StepsConfig(zuoraRestConfig: ZuoraRestConfig)

  implicit val stepsConfigReads: Reads[StepsConfig] = Json.reads[StepsConfig]

  type AquaCall[REQUEST, RESPONSE] = REQUEST => ClientFailableOp[RESPONSE]

  def apply[REQUEST, RESPONSE](
    stage: Stage,
    s3Load: Stage => ConfigFailure \/ String,
    lambdaIO: LambdaIO,
    wireCall: Config[StepsConfig] => AquaCall[REQUEST, RESPONSE]
  )(implicit r: Reads[REQUEST], w: Writes[RESPONSE]): Unit = {

    val lambdaResponse = for {
      request <- ParseRequest[REQUEST](lambdaIO.inputStream).toEither.disjunction
      config <- LoadConfig.default[StepsConfig](implicitly)(stage, s3Load(stage)).leftMap(configError => LambdaException(configError.error))
      aquaCall = wireCall(config)
      callResponse <- aquaCall(request).leftMap(error => LambdaException(error.message))
    } yield callResponse

    lambdaResponse match {
      case -\/(exception) => {
        logger.error("terminating lambda with error ", exception)
        throw exception
      }
      case \/-(successResponse) => SerialiseResponse(lambdaIO.outputStream, successResponse)
    }

  }

}
