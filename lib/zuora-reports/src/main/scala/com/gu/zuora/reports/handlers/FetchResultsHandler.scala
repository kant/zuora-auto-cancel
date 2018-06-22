package com.gu.zuora.reports.handlers

import java.io.{InputStream, OutputStream}

import com.amazonaws.services.lambda.runtime.Context
import com.gu.effects.RawEffects
import com.gu.util.apigateway.ApiGatewayHandler.LambdaIO
import com.gu.util.config.Config
import com.gu.util.zuora.RestRequestMaker.Requests
import com.gu.zuora.reports.ReportsLambda.{AquaCall, StepsConfig}
import com.gu.zuora.reports.aqua.ZuoraAquaRequestMaker
import com.gu.zuora.reports.{GetJobResult, JobResult, JobResultRequest, ReportsLambda}

object FetchResultsHandler {

  private def wireCall(call: Requests => AquaCall[JobResultRequest, JobResult])(config: Config[StepsConfig]): AquaCall[JobResultRequest, JobResult] = {
    call(ZuoraAquaRequestMaker(RawEffects.response, config.stepsConfig.zuoraRestConfig))
  }

  // this is the entry point
  // it's referenced by the cloudformation so make sure you keep it in step
  def apply(inputStream: InputStream, outputStream: OutputStream, context: Context): Unit =
    ReportsLambda[JobResultRequest, JobResult](RawEffects.stage, RawEffects.s3Load, LambdaIO(inputStream, outputStream, context), wireCall(GetJobResult.apply))
}
