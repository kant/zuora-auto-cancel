package com.gu.zuora.reports.aqua

import java.util.Base64
import com.gu.util.zuora.{Logging, RestRequestMaker, ZuoraRestConfig}
import com.gu.util.zuora.RestRequestMaker.{ClientFailableOp, GenericError}
import okhttp3.{Request, Response}
import play.api.libs.json._
import scalaz.Scalaz._

object ZuoraAquaRequestMaker extends Logging {

  case class ZuoraAquaResponse(
    status: String,
    errorCode: Option[String] = None,
    message: Option[String] = None
  )

  implicit val reads = Json.reads[ZuoraAquaResponse]

  def apply(response: Request => Response, config: ZuoraRestConfig): RestRequestMaker.Requests = {
    val credentials = s"${config.username}:${config.password}"
    val encodedCredentials = Base64.getEncoder.encodeToString(credentials.getBytes("UTF-8"))
    new RestRequestMaker.Requests(
      headers = Map(
        "Authorization" -> s"Basic $encodedCredentials"
      ),
      baseUrl = config.baseUrl + "/",
      getResponse = response,
      jsonIsSuccessful = zuoraIsSuccessful
    )
  }

  def zuoraIsSuccessful(bodyAsJson: JsValue): ClientFailableOp[Unit] = {

    bodyAsJson.validate[ZuoraAquaResponse] match {
      case JsSuccess(ZuoraAquaResponse("error", errorCode, message), _) => {
        logger.error(s"Zuora Aqua Api rejected our call $bodyAsJson")
        val codePart = errorCode.map(c => s"error code $c:")
        val messagePart = message.getOrElse("No error message")
        GenericError(s"$codePart $messagePart").left
      }
      case JsSuccess(_: ZuoraAquaResponse, _) => ().right

      case error: JsError => {
        val errorMessage = s"Failed to parse Zuora AQuA API response: $error. Response body was: \n $bodyAsJson"
        logger.error(errorMessage)
        GenericError(errorMessage).left
      }
    }
  }
}

