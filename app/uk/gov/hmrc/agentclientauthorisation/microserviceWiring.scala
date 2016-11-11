/*
 * Copyright 2016 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.agentclientauthorisation

import java.net.URL

import play.api.ApplicationLoader.Context
import play.api.inject.{BindingKey, Injector, SimpleInjector}
import play.api.libs.ws.ahc.AhcWSComponents
import play.api.mvc.Controller
import play.api.routing.Router
import play.api.{ApplicationLoader, BuiltInComponentsFromContext, Logger}
import play.modules.reactivemongo.{ReactiveMongoComponent, ReactiveMongoComponentImpl}
import uk.gov.hmrc.agentclientauthorisation.connectors.{AgenciesFakeConnector, RelationshipsConnector}
import uk.gov.hmrc.agentclientauthorisation.controllers.{AgencyInvitationsController, ClientInvitationsController, WhitelistController}
import uk.gov.hmrc.agentclientauthorisation.repository.InvitationsMongoRepository
import uk.gov.hmrc.agentclientauthorisation.service.{InvitationsService, PostcodeService}
import uk.gov.hmrc.play.audit.http.HttpAuditing
import uk.gov.hmrc.play.audit.http.config.LoadAuditingConfig
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.auth.microservice.connectors.AuthConnector
import uk.gov.hmrc.play.config.{AppName, RunMode, ServicesConfig}
import uk.gov.hmrc.play.http.hooks.HttpHook
import uk.gov.hmrc.play.http.ws._

import scala.reflect.ClassTag

object WSHttp extends WSGet with WSPut with WSPost with WSDelete with WSPatch with AppName with HttpAuditing {
  override val hooks: Seq[HttpHook] = Seq(AuditingHook)
  override val auditConnector = MicroserviceAuditConnector
}

object MicroserviceAuditConnector extends AuditConnector with RunMode {
  override lazy val auditingConfig = LoadAuditingConfig(s"auditing")
}

object MicroserviceAuthConnector extends AuthConnector with ServicesConfig {
  override val authBaseUrl = baseUrl("auth")
}


trait ServiceRegistry extends ServicesConfig {

  // Instantiate services here
  lazy val relationshipsConnector = new RelationshipsConnector(new URL(baseUrl("relationships")), WSHttp)
  lazy val invitationsService = new InvitationsService(InvitationsMongoRepository.store, relationshipsConnector)
  lazy val authConnector = new uk.gov.hmrc.agentclientauthorisation.connectors.AuthConnector(new URL(baseUrl("auth")), WSHttp)
  lazy val agenciesFakeConnector = new AgenciesFakeConnector(new URL(baseUrl("agencies-fake")), WSHttp)
}

trait ControllerRegistry {
  registry: ServiceRegistry =>

  private lazy val controllers = Map[Class[_], Controller](
    classOf[AgencyInvitationsController] -> new AgencyInvitationsController(new PostcodeService, invitationsService, authConnector, agenciesFakeConnector),
    classOf[ClientInvitationsController] -> new ClientInvitationsController(invitationsService, authConnector, agenciesFakeConnector),
    classOf[WhitelistController] -> new WhitelistController()
  )

  def getController[A](controllerClass: Class[A]): A = controllers(controllerClass).asInstanceOf[A]
}

class MicroserviceApplicationLoader extends ApplicationLoader {
  def load(context: Context) = {
    new MicroserviceComponents(context).application
  }
}

class MicroserviceComponents(context: Context) extends BuiltInComponentsFromContext(context) with AhcWSComponents with ControllerRegistry with ServiceRegistry {
  lazy val router: Router = new prod.Routes()

  private object MicroserviceInjector extends Injector {
    override def instanceOf[T](implicit ct: ClassTag[T]) = instanceOf(ct.runtimeClass.asInstanceOf[Class[T]])

    override def instanceOf[T](key: BindingKey[T]) = instanceOf(key.clazz)

    //TODO ReactiveMongoHmrcModule specifies ReactiveMongoComponent as eager
    private lazy val lazyBindings = Map[Class[_], AnyRef](
      classOf[ReactiveMongoComponent] -> new ReactiveMongoComponentImpl(application, applicationLifecycle)
    )

    override def instanceOf[T](clazz: Class[T]) = {
      Logger.debug(s"instanceOf(${clazz.getName})")
      lazyBindings.get(clazz).map(_.asInstanceOf[T]).getOrElse(
        getController(clazz)
      )
    }
  }

  //TODO remove deprecated global if possible
  override lazy val injector: Injector =
    new SimpleInjector(MicroserviceInjector) + router + cookieSigner + csrfTokenSigner + httpConfiguration + tempFileCreator + global + crypto + wsClientConfig + ahcWsClientConfig + wsApi + wsClient

}