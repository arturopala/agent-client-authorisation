/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.agentclientauthorisation.controllers.actions

import org.scalatest.mock.MockitoSugar
import play.api.mvc.{Result, Results}
import uk.gov.hmrc.agentclientauthorisation.connectors.{AddressDetails, BusinessDetails, DesConnector}
import uk.gov.hmrc.agentclientauthorisation.model.AgentInvitation
import uk.gov.hmrc.agentclientauthorisation.service.PostcodeService
import uk.gov.hmrc.play.test.UnitSpec
import org.mockito.Mockito._
import uk.gov.hmrc.domain.Nino
import org.mockito.Matchers._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class AgentInvitationValidationSpec extends UnitSpec with AgentInvitationValidation with Results with MockitoSugar {

  private val desConnector = mock[DesConnector]
  override val postcodeService: PostcodeService = new PostcodeService(desConnector)

  private val validInvite: AgentInvitation = AgentInvitation("mtd-sa", "AA123456A", "AN11PA")
  private implicit val hc = HeaderCarrier()

  private implicit class ResultChecker(r: Result) {
    def is(r1: Result) = status(r1) shouldBe status(r)
  }

  private def responseFor(invite: AgentInvitation): Result = {
    await(checkForErrors(invite)).head
  }
  private def businessHasPostcode(postcode: String = "AN11PA") = when(desConnector.getBusinessDetails(any[Nino])(any[HeaderCarrier], any[ExecutionContext])).thenReturn(Future successful Some(BusinessDetails(AddressDetails("GB", Some(postcode)))))

  "checkForErrors" should {

    "fail with Forbidden if the postcode doesn't match" in {
      businessHasPostcode()
      responseFor(validInvite.copy(postcode = "BN29AB")) is Forbidden
    }

    "fail with BadRequest if the postcode is not valid" in {
      businessHasPostcode()
      responseFor(validInvite.copy(postcode = "AAAAAA")) is BadRequest
    }

    "fail with NotImplemented if the regime is not mtd-sa" in {
      businessHasPostcode()
      responseFor(validInvite.copy(regime = "mtd-vat")) is NotImplemented
    }

    "report multiple failures" in {
      businessHasPostcode()
      val errors: Seq[Result] = await(checkForErrors(validInvite.copy(postcode = "BN29AB", regime = "mtd-vat")))
      status(errors.head) shouldBe status(Forbidden)
      status(errors(1)) shouldBe status(NotImplemented)
    }

    "pass when the postcode is valid, matches and has mtd-sa as the regime" in {
      businessHasPostcode()
      await(checkForErrors(validInvite)) shouldBe Nil
    }

    "pass when the postcodes differ by case" in {
      businessHasPostcode("an11pa")
      await(checkForErrors(validInvite)) shouldBe Nil

      businessHasPostcode()
      await(checkForErrors(validInvite.copy(postcode = "an11pa"))) shouldBe Nil
    }

    "pass when the postcodes differ by spacing" in {
      businessHasPostcode("AN1 1PA")
      await(checkForErrors(validInvite)) shouldBe Nil

      businessHasPostcode()
      await(checkForErrors(validInvite.copy(postcode = "AN1 1PA"))) shouldBe Nil
    }
  }
}
