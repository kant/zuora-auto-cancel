package com.gu.paymentFailure

import com.gu.paymentFailure.GetPaymentData.PaymentFailureInformation
import com.gu.paymentFailure.ZuoraEmailSteps.ZuoraEmailStepsDeps
import com.gu.util.Auth.validTenant
import com.gu.util.ETConfig.ETSendIds
import com.gu.util._
import com.gu.util.apigateway.ApiGatewayResponse.unauthorized
import com.gu.util.apigateway.{ ApiGatewayRequest, ApiGatewayResponse }
import com.gu.util.exacttarget.EmailSendSteps.EmailSendStepsDeps
import com.gu.util.exacttarget.{ EmailRequest, EmailSendSteps }
import com.gu.util.reader.Types._
import com.gu.util.zuora.ZuoraGetInvoiceTransactions.InvoiceTransactionSummary
import com.gu.util.zuora.{ ZuoraDeps, ZuoraGetInvoiceTransactions }
import okhttp3.{ Request, Response }
import play.api.libs.json.Json

import scalaz.syntax.std.option._
import scalaz.{ -\/, \/- }

object PaymentFailureSteps extends Logging {

  def loggableData(callout: PaymentFailureCallout) = {
    s"accountId: ${callout.accountId}, paymentId: ${callout.paymentId}, failureNumber: ${callout.failureNumber}, paymentMethodType: ${callout.paymentMethodType}, currency: ${callout.currency}"
  }

  def apply(deps: PFDeps)(apiGatewayRequest: ApiGatewayRequest): FailableOp[Unit] = {
    for {
      paymentFailureCallout <- Json.fromJson[PaymentFailureCallout](Json.parse(apiGatewayRequest.body)).toFailableOp
      _ = logger.info(s"received ${loggableData(paymentFailureCallout)}")
      _ <- validateTenantCallout(deps.trustedApiConfig)(paymentFailureCallout.tenantId)
      request <- makeRequest(deps.etSendIDs, paymentFailureCallout)
      _ <- deps.sendEmailRegardingAccount(paymentFailureCallout.accountId, request)
    } yield ()
  }

  def makeRequest(etSendIds: ETSendIds, paymentFailureCallout: PaymentFailureCallout): FailableOp[PaymentFailureInformation => EmailRequest] = {
    etSendIds.find(paymentFailureCallout.failureNumber).map { etId => pFI: PaymentFailureInformation => EmailRequest(etId, ToMessage(paymentFailureCallout, pFI))
    }.toRightDisjunction(ApiGatewayResponse.internalServerError(s"no ET id configured for failure number: ${paymentFailureCallout.failureNumber}"))
  }

  def validateTenantCallout(trustedApiConfig: TrustedApiConfig)(calloutTenantId: String): FailableOp[Unit] = {
    if (validTenant(trustedApiConfig, calloutTenantId)) \/-(()) else -\/(unauthorized)
  }

  object PFDeps {
    def default(response: Request => Response, config: Config): PFDeps = {
      PFDeps(
        ZuoraEmailSteps.sendEmailRegardingAccount(ZuoraEmailStepsDeps.default(response, config)),
        config.etConfig.etSendIDs,
        config.trustedApiConfig
      )
    }
  }

  case class PFDeps(
    sendEmailRegardingAccount: (String, PaymentFailureInformation => EmailRequest) => FailableOp[Unit],
    etSendIDs: ETSendIds,
    trustedApiConfig: TrustedApiConfig
  )

}

object ZuoraEmailSteps {

  def sendEmailRegardingAccount(deps: ZuoraEmailStepsDeps)(accountId: String, toMessage: PaymentFailureInformation => EmailRequest): FailableOp[Unit] = {
    for {
      invoiceTransactionSummary <- deps.getInvoiceTransactions(accountId)
      paymentInformation <- GetPaymentData(accountId)(invoiceTransactionSummary)
      message = toMessage(paymentInformation)
      _ <- deps.sendEmail(message).leftMap(resp => resp.copy(body = s"email not sent for account ${accountId}"))
    } yield ()
  }

  object ZuoraEmailStepsDeps {
    def default(response: Request => Response, config: Config): ZuoraEmailStepsDeps = {
      ZuoraEmailStepsDeps(
        EmailSendSteps.apply(EmailSendStepsDeps.default(config.stage, response, config.etConfig)),
        a => ZuoraGetInvoiceTransactions(a).run.run(ZuoraDeps(response, config.zuoraRestConfig))
      )
    }
  }

  case class ZuoraEmailStepsDeps(
    sendEmail: EmailRequest => FailableOp[Unit],
    getInvoiceTransactions: String => FailableOp[InvoiceTransactionSummary]
  )

}