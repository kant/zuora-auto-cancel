package com.gu.paymentFailure

import java.lang.System.getenv

trait Config {
  def apiClientId: String
  def apiToken: String
  def tenantId: String
}

object EnvConfig extends Config {
  override def apiClientId: String = getenv("ApiClientId")
  override def apiToken: String = getenv("ApiToken")
  override def tenantId: String = getenv("TenantId")
}