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

import org.joda.time.DateTime
import org.scalatest.concurrent.Eventually
import org.scalatest.{Inside, Inspectors}
import play.api.Logger
import play.api.libs.json._
import play.mvc.Http.HeaderNames.LOCATION
import reactivemongo.bson.BSONObjectID
import uk.gov.hmrc.agentclientauthorisation.model.Arn
import uk.gov.hmrc.agentclientauthorisation.support._
import uk.gov.hmrc.domain.AgentCode
import uk.gov.hmrc.play.http.HttpResponse
import uk.gov.hmrc.play.test.UnitSpec

class AgentClientAuthorisationISpec extends UnitSpec with MongoAppAndStubs with Inspectors with Inside with Eventually with SecuredEndpointBehaviours {

  private implicit val arn = Arn("ABCDEF12345678")
  private implicit val agentCode = AgentCode("LMNOP123456")

  private val REGIME: String = "mtd-sa"
  private val getInvitationsUrl = s"/agent-client-authorisation/agencies/${arn.arn}/invitations/sent"
  private val getInvitationUrl = s"/agent-client-authorisation/agencies/${arn.arn}/invitations/sent/"
  private val createInvitationUrl = s"/agent-client-authorisation/agencies/${arn.arn}/invitations"

  val clientId: String = "1234567890"

  "GET /agencies/:arn/invitations/sent" should {
    behave like anEndpointAccessibleForMtdAgentsOnly(responseForGetInvitations())

    "return 403 for someone else's invitation list" in {
      given().agentAdmin(Arn("98765"), AgentCode("123456")).isLoggedIn().andHasMtdBusinessPartnerRecord()

      val response = responseForGetInvitations()

      response.status shouldBe 403
    }

    "return only invitations for the specified client" in {
      val clientId = "1234567890"
      given().agentAdmin(arn, agentCode).isLoggedIn().andHasMtdBusinessPartnerRecord()

      responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "$clientId", "postcode": "AA1 1AA"}""").header("location")
      responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "9876543210", "postcode": "AA1 1AA"}""").header("location")

      val response = responseForGetInvitations(clientId)

      response.status shouldBe 200
      val invitation = embeddedInvitations(response.json)
      invitation.value.size shouldBe 1
      invitation.value.head \ "clientId" shouldBe JsString(clientId)
      response.json \ "_links" \ "self" \ "href" shouldBe JsString("/agent-client-authorisation/agencies/ABCDEF12345678/invitations/sent?clientId=1234567890")
    }
  }

  "POST /agencies/:arn/invitations" should {
    val clientId = "1234567899"
    behave like anEndpointAccessibleForMtdAgentsOnly(responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "$clientId", "postcode": "AA1 1AA"}"""))
  }

  "GET /agencies/:arn/invitations/sent/:invitationId" should {
    behave like anEndpointAccessibleForMtdAgentsOnly(responseForGetInvitation())

    "Return a created invitation" in {
      val testStartTime = DateTime.now().getMillis
      given().agentAdmin(arn, agentCode).isLoggedIn().andHasMtdBusinessPartnerRecord()
      val clientId = "1234567899"
      val location = responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "$clientId", "postcode": "AA1 1AA"}""").header("location")

      val invitation = new Resource(location.get, port).get().json
      checkInvitation(clientId, invitation, testStartTime)
    }

    "Return 404 for an invitation that doesn't exist" in {
      given().agentAdmin(arn, agentCode).isLoggedIn().andHasMtdBusinessPartnerRecord()

      val response = responseForGetInvitation(BSONObjectID.generate.stringify)
      response.status shouldBe 404
    }

    "Return 403 if accessing someone else's invitation" in {
      given().agentAdmin(arn, agentCode).isLoggedIn().andHasMtdBusinessPartnerRecord()
      val clientId = "1234567899"
      val location = responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "$clientId", "postcode": "AA1 1AA"}""").header("location")

      given().agentAdmin(Arn("98765"), AgentCode("123456")).isLoggedIn().andHasMtdBusinessPartnerRecord()
      val response = new Resource(location.get, port).get()
      response.status shouldBe 403
    }
  }

  "/agencies/:arn/invitations" should {
    "create and retrieve invitations" in {
      val testStartTime = DateTime.now().getMillis
      val numberOfExpectedInvitations: Long = 2
      val ((_, client1Id), (_ ,client2Id)) = createInvitations()

      note("the freshly added invitations should be available")
      val responseJson = getResponseJson(){ json =>
        embeddedInvitations(json).value.sortBy(j => (j \ "clientId").as[String]) should have size numberOfExpectedInvitations
      }

      val selfLinkHref = (responseJson \ "_links" \ "self" \ "href").as[String]
      selfLinkHref shouldBe getInvitationsUrl


      val invitationsArray = embeddedInvitations(responseJson).value
      val firstInvitation = invitationsArray head
      val secondInvitation = invitationsArray(1)

      val firstInvitationId = checkInvitation(client1Id, firstInvitation, testStartTime)
      val secondInvitationId = checkInvitation(client2Id, secondInvitation, testStartTime)

      firstInvitationId should not be secondInvitationId


      val invitationsInLinkSection = linksInvitations(responseJson).value
      invitationsInLinkSection should have size numberOfExpectedInvitations

      // TODO check for links to the client invitations too
      // TODO: Would be nice to be able to 'pimp' these values to add methods
      val firstLinkInvite = invitationsInLinkSection head
      val secondLinkInvite = invitationsInLinkSection(1)
      (firstLinkInvite \ "href").as[String] shouldBe expectedAgencyInvitationLink(arn, firstInvitationId)
      (secondLinkInvite \ "href").as[String] shouldBe expectedAgencyInvitationLink(arn, secondInvitationId)
    }

    "create and retrieve duplicate invitations" in {
      val testStartTime = DateTime.now().getMillis
      createDuplicateInvitations()

      note("the freshly added invitations should be available")
      val responseJson = getResponseJson(){ json =>
        embeddedInvitations(json).value should have size 2
      }

      val selfLinkHref = (responseJson \ "_links" \ "self" \ "href").as[String]
      selfLinkHref shouldBe getInvitationsUrl

      val invitationsArray = embeddedInvitations(responseJson).value
      val firstInvitation = invitationsArray.head
      val secondInvitation = invitationsArray(1)

      val firstInvitationId = checkInvitation("1234567890", firstInvitation, testStartTime)
      val secondInvitationId = checkInvitation("1234567890", secondInvitation, testStartTime)

      firstInvitationId should not be secondInvitationId
    }

    "should not create invitation if postcodes do not match" in {
      given().agentAdmin(arn, agentCode).isLoggedIn().andHasMtdBusinessPartnerRecord()
      responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "9876543210", "postcode": "BA1 1AA"}""").status shouldBe 403
    }

    "should not create invitation if postcode is not in a valid format" in {
      given().agentAdmin(arn, agentCode).isLoggedIn().andHasMtdBusinessPartnerRecord()
      responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "9876543210", "postcode": "BAn 1AA"}""").status shouldBe 400
    }

    "should not create invitation for an unsupported regime" in {
      given().agentAdmin(arn, agentCode).isLoggedIn().andHasMtdBusinessPartnerRecord()
      val response = responseForCreateInvitation(s"""{"regime": "sa", "clientId": "9876543210", "postcode": "A11 1AA"}""")
      response.status shouldBe 501
      (response.json \ "code").as[String] shouldBe "UNSUPPORTED_REGIME"
      (response.json \ "message").as[String] shouldBe "Unsupported regime \"sa\", the only currently supported regime is \"mtd-sa\""
    }

    "should create invitation if postcode has no spaces" in {
      given().agentAdmin(arn, agentCode).isLoggedIn().andHasMtdBusinessPartnerRecord()
      responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "9876543210", "postcode": "AA11AA"}""").status shouldBe 201
    }

    "should create invitation if postcode has more than one space" in {
      given().agentAdmin(arn, agentCode).isLoggedIn().andHasMtdBusinessPartnerRecord()
      responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "9876543210", "postcode": "A A1 1A A"}""").status shouldBe 201
    }
  }


  private def checkInvitation(client2Id: String, invitation: JsValue, testStartTime: Long): String = {
    val beRecent = be >= testStartTime and be <= (testStartTime + 5000)
    val alphanumeric = "[0-9A-Za-z]+"
    val invitationId = (invitation \ "id").as[String]
    invitationId should fullyMatch regex alphanumeric
    (invitation \ "_links" \ "self" \ "href").as[String] shouldBe expectedAgencyInvitationLink(arn, invitationId)
    (invitation \ "_links" \ "cancel" \ "href").as[String] shouldBe s"/agent-client-authorisation/agencies/${arn.arn}/invitations/sent/$invitationId"
    (invitation \ "_links" \ "agency" \ "href").as[String] shouldBe s"http://localhost:$wiremockPort/agencies-fake/agencies/${arn.arn}"
    (invitation \ "arn") shouldBe JsString(arn.arn)
    (invitation \ "regime") shouldBe JsString(REGIME)
    (invitation \ "clientId") shouldBe JsString(client2Id)
    (invitation \ "status") shouldBe JsString("Pending")
    (invitation \ "created").as[Long] should beRecent
    (invitation \ "lastUpdated").as[Long] should beRecent
    invitationId
  }

  private def expectedAgencyInvitationLink(arn: Arn, invitationId: String): String =
    s"/agent-client-authorisation/agencies/${arn.arn}/invitations/sent/$invitationId"

  def createInvitations(): ((String, String), (String, String)) = {
    dropMongoDb()
    given().agentAdmin(arn, agentCode).isLoggedIn().andHasMtdBusinessPartnerRecord()
    val client1Id = "1234567890"
    val client2Id = "1234567891"


    note("there should be no requests")
    eventually {
      inside(responseForGetInvitations()) { case resp =>
        resp.status shouldBe 200
        embeddedInvitations(resp.json).value shouldBe 'empty
      }
    }

    note("we should be able to add 2 new requests")
    val location1: String = checkCreatedResponse(responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "$client1Id", "postcode": "AA1 1AA"}"""))
    val location2: String = checkCreatedResponse(responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "$client2Id", "postcode": "AA1 1AA"}"""))

    val json1: JsValue = new Resource(location1, port).get().json
    val json2: JsValue = new Resource(location2, port).get().json

    (invitation(json1), invitation(json2))
  }


  private def createDuplicateInvitations(): ((String, String), (String, String)) = {
    dropMongoDb()
    given().agentAdmin(arn, agentCode).isLoggedIn().andHasMtdBusinessPartnerRecord()
    val clientId = "1234567890"

    note("there should be no requests")
    eventually {
      inside(responseForGetInvitations()) { case resp =>
        resp.status shouldBe 200
        embeddedInvitations(resp.json).value shouldBe 'empty
      }
    }

    note("we should be able to add 2 new requests")
    val location1: String = checkCreatedResponse(responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "$clientId", "postcode": "AA1 1AA"}"""))
    val location2: String = checkCreatedResponse(responseForCreateInvitation(s"""{"regime": "$REGIME", "clientId": "$clientId", "postcode": "AA1 1AA"}"""))

    val json1: JsValue = new Resource(location1, port).get().json
    val json2: JsValue = new Resource(location2, port).get().json

    (invitation(json1), invitation(json2))
  }


  private def getResponseJson[T]()(whenThisPasses: JsValue => T) = eventually {
    val responseJson = responseForGetInvitations().json
    Logger.info(s"responseJson = $responseJson")
    whenThisPasses(responseJson)
    responseJson
  }

  private def invitation(json: JsValue): (String, String) = {
      (json \ "id").as[String] ->
      (json \ "clientId").as[String]
  }

  private def embeddedInvitations(response: JsValue) = {
    normaliseInvitations(response \ "_embedded" \ "invitations")
  }

  private def linksInvitations(response: JsValue) = {
    normaliseInvitations(response \ "_links" \ "invitations")
  }

  private def normaliseInvitations(embedded: JsValue): JsArray = {
    embedded match {
      case array: JsArray => array
      case obj: JsObject => JsArray(Seq(obj))
    }
  }

  private def responseForGetInvitations(): HttpResponse = {
    new Resource(getInvitationsUrl, port).get()
  }

  private def responseForGetInvitations(clientId: String): HttpResponse = {
    new Resource(getInvitationsUrl + s"?clientId=$clientId", port).get()
  }

  private def responseForGetInvitation(invitationId: String = "none"): HttpResponse = {
    new Resource(getInvitationUrl + invitationId, port).get()
  }

  private def responseForCreateInvitation(body: String): HttpResponse = {
    new Resource(createInvitationUrl, port).postAsJson(body)
  }
  private def checkCreatedResponse(httpResponse: HttpResponse) = {
    httpResponse.status shouldBe 201
    val expectedUri: String = httpResponse.header(LOCATION).get
    expectedUri should startWith (s"/agent-client-authorisation/agencies/${arn.arn}/invitations/sent/")
    expectedUri
  }
}

