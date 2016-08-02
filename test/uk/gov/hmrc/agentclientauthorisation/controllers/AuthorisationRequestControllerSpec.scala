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

package uk.gov.hmrc.agentclientauthorisation.controllers

import org.mockito.Matchers
import org.mockito.Matchers.{any, anyString}
import org.mockito.Mockito.{reset, when}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.mock.MockitoSugar
import play.api.libs.json.{JsArray, JsValue}
import play.api.mvc.Result
import play.api.test.FakeRequest
import reactivemongo.bson.BSONObjectID
import uk.gov.hmrc.agentclientauthorisation.connectors.{Accounts, AuthConnector, _}
import uk.gov.hmrc.agentclientauthorisation.model.{AgentClientAuthorisationHttpRequest, AgentClientAuthorisationRequest}
import uk.gov.hmrc.agentclientauthorisation.repository.AuthorisationRequestRepository
import uk.gov.hmrc.agentclientauthorisation.sa.services.SaLookupService
import uk.gov.hmrc.domain.{AgentCode, SaUtr}
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.Future

class AuthorisationRequestControllerSpec extends UnitSpec with MockitoSugar with BeforeAndAfterEach {
  val repository = mock[AuthorisationRequestRepository]
  val saLookupService = mock[SaLookupService]
  val authConnector = mock[AuthConnector]
  val userDetailsConnector = mock[UserDetailsConnector]
  val controller = new AuthorisationRequestController(repository, saLookupService, authConnector, userDetailsConnector)

  implicit val hc = HeaderCarrier()

  val agentCode = AgentCode("123456789012")

  override protected def beforeEach() = {
    super.beforeEach()
    reset(repository)
    reset(saLookupService)
  }


  "createRequest" should {
    "return a 403 when the UTR and postcode don't match" in {
      givenAgentIsLoggedInAndHasActiveSaEnrolment()
      when(saLookupService.utrAndPostcodeMatch(any[SaUtr], anyString)(any[HeaderCarrier])).thenReturn(Future.successful(false))

      val body: JsValue = AgentClientAuthorisationHttpRequest.format.writes(
        AgentClientAuthorisationHttpRequest(agentCode, "sa", "54321", "BB2 2BB"))

      val result: Result = await(controller.createRequest()(FakeRequest().withBody(body)))

      status(result) shouldBe 403
    }

    "return a 501 Not Implemented when the regime is not 'sa'" in {
      givenAgentIsLoggedInAndHasActiveSaEnrolment()
      when(saLookupService.utrAndPostcodeMatch(any[SaUtr], anyString)(any[HeaderCarrier])).thenReturn(Future.successful(true))

      val body: JsValue = AgentClientAuthorisationHttpRequest.format.writes(
        AgentClientAuthorisationHttpRequest(agentCode, "vat", "54321", "AA1 1AA"))

      val result: Result = await(controller.createRequest()(FakeRequest().withBody(body)))

      status(result) shouldBe 501
      (jsonBodyOf(result) \ "message").as[String] shouldBe "This service does not currently support the 'vat' tax regime"
    }

    "propagate exceptions when the repository fails" in {
      givenAgentIsLoggedInAndHasActiveSaEnrolment()
      when(saLookupService.utrAndPostcodeMatch(any[SaUtr], anyString)(any[HeaderCarrier])).thenReturn(Future.successful(true))
      when(repository.create(any[AgentCode], anyString, anyString, Matchers.eq("user-details-link"))).thenReturn(Future failed new RuntimeException("dummy exception"))

      val body: JsValue = AgentClientAuthorisationHttpRequest.format.writes(
        AgentClientAuthorisationHttpRequest(agentCode, "sa", "54321", "AA1 1AA"))

      intercept[RuntimeException] {
        await(controller.createRequest()(FakeRequest().withBody(body)))
      }.getMessage shouldBe "dummy exception"
    }
  }

  "getRequests" should {
    "return an array of requests even if there is only one" in {
      givenAgentIsLoggedInAndHasActiveSaEnrolment()
      val aRequest = AgentClientAuthorisationRequest(BSONObjectID.generate, agentCode, "sa", "1", "user-details-link", List.empty)
      when(saLookupService.lookupByUtr(any[SaUtr])(any[HeaderCarrier])).thenReturn(Future.successful(Some("name")))
      when(repository.list(any[AgentCode])).thenReturn(Future successful List(aRequest))

      val result: Result = await(controller.getRequests()(FakeRequest()))
      status(result) shouldBe 200
      (jsonBodyOf(result) \ "_embedded" \ "requests") shouldBe a[JsArray]
    }
  }

  def givenAgentIsLoggedInAndHasActiveSaEnrolment(): Unit = {
    when(authConnector.currentUserInfo()(any(), any())).thenReturn(Future successful UserInfo(Accounts(Some(agentCode), None), "user-details-link", hasActivatedIrSaEnrolment = true))
    when(userDetailsConnector.userDetails(Matchers.eq("user-details-link"))(any[HeaderCarrier])).thenReturn(UserDetails("Name", Some("agent"), Some("Friendly name")))
  }
}