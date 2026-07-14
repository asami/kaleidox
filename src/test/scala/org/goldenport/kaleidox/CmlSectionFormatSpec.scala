package org.goldenport.kaleidox

import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner
import org.smartdox.{Section, Text}
import org.goldenport.record.v2.{CFormat, CMaxLength, CMinLength, CRegex}
import org.goldenport.kaleidox.model.OperationModel

/*
 * @since   Mar. 24, 2026
 *  version Mar. 25, 2026
 *  version Apr.  9, 2026
 *  version May. 24, 2026
 * @version Jul. 15, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class CmlSectionFormatSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  private val _config = Config.log.debug.withoutLocation

  "Model parser" should {
    "operation and Value grammar" which {
    "preserve top-level VALUE properties from the CML AST" in {
      Given("a VALUE with a description-list input-kind property")
      val source = """# VALUE
        |
        |## CreateOrder
        |- input-kind :: COMMAND
        |
        |### ATTRIBUTE
        |
        || name    | type | multiplicity |
        ||---------+------+--------------|
        || orderId | name | 1            |
        |""".stripMargin

      When("Kaleidox builds the Value model")
      val value = Model.parse(_config, source).getValueModel.flatMap(_.get("CreateOrder"))

      Then("the structured property is available without plain-text re-parsing")
      value.flatMap(_.getProperty("input-kind")) shouldBe Some("COMMAND")
      value.toVector.flatMap(_.schema.columns).map(_.name) should contain ("orderId")
    }

    "accept OPERATION/COMMAND sections" in {
      val s = """# COMMAND

## CreateOrder

### ATTRIBUTE

| name    | type    | multiplicity |
|---------+---------+--------------|
| orderId | OrderId | 1            |
| amount  | Money   | 1            |

# QUERY

## GetOrder

### ATTRIBUTE

| name    | type    | multiplicity |
|---------+---------+--------------|
| orderId | OrderId | 1            |

# OPERATION

## createOrder

### TYPE
COMMAND

### INPUT
CreateOrder

### OUTPUT
CreateOrderResult
"""
      val model = Model.parse(_config, s)
      val opmodel = model.takeOperationModel
      opmodel.operations.map(_.name) should contain ("createOrder")
      opmodel.values.map(_.name) should contain ("CreateOrder")
      opmodel.values.find(_.name == "CreateOrder").map(_.fields.size) should be (Some(2))
      opmodel.normalizedOperations.head.inputType should be ("CreateOrder")
    }

    "infer OPERATION kind from INPUT value definitions when TYPE is omitted" in {
      val s = """# COMMAND

## CreateOrder

### ATTRIBUTE

| name    | type    | multiplicity |
|---------+---------+--------------|
| orderId | OrderId | 1            |

# QUERY

## GetOrder

### ATTRIBUTE

| name    | type    | multiplicity |
|---------+---------+--------------|
| orderId | OrderId | 1            |

# OPERATION

## createOrder

### INPUT
CreateOrder

### OUTPUT
CreateOrderResult

## getOrder

### INPUT
GetOrder

### OUTPUT
GetOrderResult
"""
      val model = Model.parse(_config, s)
      val opmodel = model.takeOperationModel
      val normalized = opmodel.normalizedOperations
      normalized.exists(x => x.name == "createOrder" && x.kind == OperationModel.OperationKind.Command) should be (true)
      normalized.exists(x => x.name == "getOrder" && x.kind == OperationModel.OperationKind.Query) should be (true)
    }

    }

    "component, event, and use-case grammar" which {
    "accept YAML in EVENT section" in {
      val s = """* EVENT
** person.created
category: ActionEvent
kind: created
selector:
  source: crm
action_name: person.sync
priority: 2
"""
      val model = Model.parse(_config, s)
      val eventmodel = model.getEventModel.getOrElse(fail("EventModel is missing"))
      eventmodel.receptionDefinitions.size should be (1)
      val event = eventmodel.receptionDefinitions.head
      event.name should be ("person.created")
      event.selectors.get("source") should be (Some("crm"))
      event.priority should be (2)
    }

    "accept YAML in COMPONENT/SUBSYSTEM sections" in {
      val s = """* COMPONENT
** person
*** COORDINATE
- org.simplemodeling.car:person-service:0.1.0
*** EXTENSIONBINDING
transport: grpc

* SUBSYSTEM
** identity
*** COMPONENT
- org.simplemodeling.car:person-service:0.1.0
*** CONFIG
profile: prod
"""
      val model = Model.parse(_config, s)
      val cs = model.takeComponentSubsystemModel
      cs.components.map(_.name) should contain ("person")
      cs.subsystems.map(_.name) should contain ("identity")
      cs.components.head.coordinates.size should be (1)
      cs.subsystems.head.config.get("profile") should be (Some("prod"))
    }

    "preserve SPI-prefixed component service metadata" in {
      Given("a provider service and a component API consumer declared under COMPONENT")
      val source = """# COMPONENT
                     |
                     |## scraper
                     |
                     |### PACKAGE
                     |
                     |org.example.scraper
                     |
                     |### SERVICE
                     |
                     |#### Scraping
                     |
                     |- spi-direction :: provides
                     |- spi-socket :: true
                     |- spi-api-name :: TextusScraper
                     |
                     |#### Scrapers
                     |
                     |- spi-direction :: requires
                     |- spi-component-api :: org.example.ScraperComponent.TextusScraperApi
                     |- spi-multiplicity :: "*"
                     |- spi-required :: true
                     |""".stripMargin

      When("Kaleidox parses the CML component metadata")
      val model = Model.parse(_config, source)
      val component = model.takeComponentSubsystemModel.components.headOption.getOrElse(
        fail(s"Component is missing; divisions=${model.divisions.map(_.name).mkString(",")}, errors=${model.errors.mkString(";")}")
      )

      Then("the provider metadata remains distinct from the consumer socket metadata")
      val provider = component.services.find(_.name == "Scraping").getOrElse(fail("Provider service is missing"))
      provider.spiDirection shouldBe "provides"
      provider.spiSocket shouldBe true
      provider.spiApiName shouldBe Some("TextusScraper")

      val consumer = component.services.find(_.name == "Scrapers").getOrElse(fail("Consumer service is missing"))
      consumer.spiDirection shouldBe "requires"
      consumer.spiComponentApi shouldBe Some("org.example.ScraperComponent.TextusScraperApi")
      consumer.spiMultiplicity shouldBe Some("*")
      consumer.spiRequired shouldBe true
    }

    "preserve ordinary component services without treating them as SPI declarations" in {
      Given("a component service entry containing narrative metadata but no spi-prefixed properties")
      val source = """# COMPONENT
                     |
                     |## catalog
                     |
                     |### SERVICE
                     |
                     |#### Catalog
                     |
                     |Catalog application service.
                     |""".stripMargin

      When("Kaleidox parses the component composition")
      val component = Model.parse(_config, source).takeComponentSubsystemModel.components.head

      Then("the service remains an ordinary non-SPI service")
      val service = component.services.headOption.getOrElse(fail("Ordinary service is missing"))
      service.name shouldBe "Catalog"
      service.spiStandard shouldBe None
      service.spiSocket shouldBe false
    }

    "reject SPI properties that are invalid for the declared direction" in {
      Given("a standard SPI provider with an API name but without component API socket generation")
      val source = """# COMPONENT
                     |
                     |## scraper
                     |
                     |### SERVICE
                     |
                     |#### Scraping
                     |
                     |- spi-standard :: cncf.web-content-fetcher
                     |- spi-api-name :: TextusScraper
                     |""".stripMargin

      When("Kaleidox validates the SPI service declaration")
      val model = Model.parse(_config, source)

      Then("the ignored API-name combination is rejected deterministically")
      model.errors.mkString(";") should include ("spi-api-name requires spi-socket=true")
    }

    "accept USE CASE sections in COMPONENT" in {
      val s = """# COMPONENT
                |
                |## user-account
                |
                |### PACKAGE
                |
                |org.example.useraccount
                |
                |### USE CASE
                |
                |#### provisional_onboarding
                |
                |##### SUMMARY
                |
                |Create a provisional user account.
                |
                |##### ACTOR
                |
                |User registration context
                |
                |##### PRIMARY ACTOR
                |
                |EndUser
                |
                |##### SECONDARY ACTOR
                |
                |IdentityOperator
                |
                |##### SUPPORTING ACTOR
                |
                |MailService
                |
                |##### STAKEHOLDER
                |
                |BusinessOwner
                |
                |##### GOAL
                |
                |Capture a lightweight lead.
                |
                |##### PRECONDITION
                |
                |The user is not registered.
                |
                |##### POSTCONDITION
                |
                |A provisional account exists.
                |
                |##### SCENARIO
                |
                |###### happy_path
                |
                |1. User enters email and password.
                |2. System creates a provisional account.
                |
                |###### duplicate_email
                |
                |1. User enters an existing email.
                |2. System rejects the request.
                |""".stripMargin
      val model = Model.parse(_config, s)
      val component = model.takeComponentSubsystemModel.components.headOption.getOrElse(fail("Component is missing"))
      val usecase = component.useCases.headOption.getOrElse(fail("Use case is missing"))

      usecase.name should be ("provisional_onboarding")
      usecase.summary should be (Some("Create a provisional user account."))
      usecase.actor should be (Some("User registration context"))
      usecase.primaryActor should be (Some("EndUser"))
      usecase.secondaryActor should be (Some("IdentityOperator"))
      usecase.supportingActor should be (Some("MailService"))
      usecase.stakeholder should be (Some("BusinessOwner"))
      usecase.goal should be (Some("Capture a lightweight lead."))
      usecase.precondition should be (Some("The user is not registered."))
      usecase.postcondition should be (Some("A provisional account exists."))
      usecase.scenarios.map(_.name) should contain allOf ("happy_path", "duplicate_email")
      usecase.scenarios.find(_.name == "happy_path").flatMap(_.steps.headOption).getOrElse("") should include ("User enters email")
    }

    "accept top-level USE CASE sections" in {
      val s = """# USE CASE
                |
                |## domain_identity_lifecycle
                |
                |### SUMMARY
                |
                |Cover the domain-wide identity lifecycle.
                |
                |### PRIMARY ACTOR
                |
                |EndUser
                |
                |### GOAL
                |
                |Provide a shared domain use-case definition above component scope.
                |""".stripMargin
      val model = Model.parse(_config, s)
      val usecase = model.takeComponentSubsystemModel.useCases.headOption.getOrElse(fail("Top-level use case is missing"))

      usecase.name should be ("domain_identity_lifecycle")
      usecase.summary should be (Some("Cover the domain-wide identity lifecycle."))
      usecase.primaryActor should be (Some("EndUser"))
      usecase.goal should be (Some("Provide a shared domain use-case definition above component scope."))
    }

    "accept top-level CAPABILITY sections" in {
      val s = """# CAPABILITY
                |
                |## Authentication
                |
                |### SUMMARY
                |
                |Provide authentication capability.
                |
                |### PRIMARY ACTOR
                |
                |EndUser
                |
                |### GOAL
                |
                |Allow a user to authenticate.
                |""".stripMargin
      val model = Model.parse(_config, s)
      val capability = model.takeComponentSubsystemModel.capabilities.headOption.getOrElse(fail("Top-level capability is missing"))

      capability.name should be ("Authentication")
      capability.summary should be (Some("Provide authentication capability."))
      capability.primaryActor should be (Some("EndUser"))
      capability.goal should be (Some("Allow a user to authenticate."))
    }

    "accept top-level VISION sections" in {
      val s = """# VISION
                |
                |## TrustedIdentity
                |
                |### SUMMARY
                |
                |Provide a trusted identity foundation for Textus.
                |
                |### GOAL
                |
                |Enable coherent user identity as a domain capability.
                |""".stripMargin
      val model = Model.parse(_config, s)
      val vision = model.takeComponentSubsystemModel.visions.headOption.getOrElse(fail("Top-level vision is missing"))

      vision.name should be ("TrustedIdentity")
      vision.summary should be (Some("Provide a trusted identity foundation for Textus."))
      vision.goal should be (Some("Enable coherent user identity as a domain capability."))
    }

    "accept top-level QUALITY sections" in {
      val s = """# QUALITY
                |
                |## AuthenticationLatency
                |
                |### SUMMARY
                |
                |Authentication should respond quickly.
                |
                |### GOAL
                |
                |Keep authentication response latency within an acceptable range.
                |""".stripMargin
      val model = Model.parse(_config, s)
      val quality = model.takeComponentSubsystemModel.qualities.headOption.getOrElse(fail("Top-level quality is missing"))

      quality.name should be ("AuthenticationLatency")
      quality.summary should be (Some("Authentication should respond quickly."))
      quality.goal should be (Some("Keep authentication response latency within an acceptable range."))
    }

    "accept top-level CONSTRAINT sections" in {
      val s = """# CONSTRAINT
                |
                |## LegacyOperatingSystem
                |
                |### SUMMARY
                |
                |The system must run on Windows 95.
                |
                |### GOAL
                |
                |Respect the required legacy operating environment.
                |""".stripMargin
      val model = Model.parse(_config, s)
      val constraint = model.takeComponentSubsystemModel.constraints.headOption.getOrElse(fail("Top-level constraint is missing"))

      constraint.name should be ("LegacyOperatingSystem")
      constraint.summary should be (Some("The system must run on Windows 95."))
      constraint.goal should be (Some("Respect the required legacy operating environment."))
    }

    "accept USE CASE sections in SERVICE" in {
      val s = """# SERVICE
                |
                |## User
                |
                |### DESCRIPTION
                |
                |Public user-facing operations.
                |
                |### USE CASE
                |
                |#### regular_registration
                |
                |##### SUMMARY
                |
                |Create a standard user account.
                |
                |##### ACTOR
                |
                |Self-service registration
                |
                |##### PRIMARY ACTOR
                |
                |EndUser
                |
                |##### SUPPORTING ACTOR
                |
                |NotificationService
                |
                |##### STAKEHOLDER
                |
                |CustomerSupport
                |
                |##### GOAL
                |
                |Register through the normal self-service path.
                |
                |##### SCENARIO
                |
                |###### happy_path
                |
                |1. User enters account information.
                |2. System creates a regular account.
                |
                |### OPERATION
                |
                |#### register
                |
                |##### INPUT
                |
                |Register input.
                |
                |###### TYPE
                |
                |RegisterInput
                |
                |##### OUTPUT
                |
                |Register result.
                |
                |###### TYPE
                |
                |RegisterResult
                |""".stripMargin
      val model = Model.parse(_config, s)
      val service = model.getServiceModel.flatMap(_.classes.get("User")).getOrElse(fail("Service is missing"))
      val usecase = service.useCases.headOption.getOrElse(fail("Use case is missing"))

      usecase.name should be ("regular_registration")
      usecase.summary should be (Some("Create a standard user account."))
      usecase.actor should be (Some("Self-service registration"))
      usecase.primaryActor should be (Some("EndUser"))
      usecase.supportingActor should be (Some("NotificationService"))
      usecase.stakeholder should be (Some("CustomerSupport"))
      usecase.goal should be (Some("Register through the normal self-service path."))
      usecase.scenarios.map(_.name) should contain ("happy_path")
      usecase.scenarios.head.steps should contain ("System creates a regular account.")
    }

    "split concatenated SCENARIO steps in USE CASE" in {
      val s = """# COMPONENT
                |
                |## account
                |
                |### USE CASE
                |
                |#### onboarding
                |
                |##### SCENARIO
                |
                |###### happy_path
                |
                |A user submits a request.The system creates an account.The system returns the result.
                |""".stripMargin
      val model = Model.parse(_config, s)
      val component = model.takeComponentSubsystemModel.components.headOption.getOrElse(fail("Component is missing"))
      val scenario = component.useCases.headOption.flatMap(_.scenarios.headOption).getOrElse(fail("Scenario is missing"))

      scenario.steps should be (
        Vector(
          "A user submits a request.",
          "The system creates an account.",
          "The system returns the result."
        )
      )
    }

    "accept explicit STEP ALTERNATE and EXCEPTION in USE CASE SCENARIO" in {
      val s = """# COMPONENT
                |
                |## account
                |
                |### USE CASE
                |
                |#### onboarding
                |
                |##### SCENARIO
                |
                |###### happy_path
                |
                |####### STEP
                |
                |User submits a request.
                |
                |####### STEP
                |
                |System creates an account.
                |
                |####### ALTERNATE
                |
                |System asks for additional confirmation before account creation.
                |
                |####### EXCEPTION
                |
                |Datastore is unavailable and the request fails.
                |""".stripMargin
      val model = Model.parse(_config, s)
      val component = model.takeComponentSubsystemModel.components.headOption.getOrElse(fail("Component is missing"))
      val scenario = component.useCases.headOption.flatMap(_.scenarios.headOption).getOrElse(fail("Scenario is missing"))

      scenario.steps should be (
        Vector(
          "User submits a request.",
          "System creates an account."
        )
      )
      scenario.alternates should be (
        Vector(
          "System asks for additional confirmation before account creation."
        )
      )
      scenario.exceptions should be (
        Vector(
          "Datastore is unavailable and the request fails."
        )
      )
    }

    }

    "schema and field metadata" which {
    "accept YAML in FEATURES section" in {
      val s = """* ENTITY
** Person
*** ATTRIBUTE
| name | type     | multiplicity |
|------+----------+--------------|
| id   | entityid | 1            |
| name | name     | 1            |
*** FEATURES
extends:
  - SimpleEntity
"""
      val model = Model.parse(_config, s)
      val entity = model.takeEntityModel.get("Person").getOrElse(fail("Entity Person is missing"))
      entity.parents.nonEmpty should be (true)
    }

    "accept literate POWERTYPE package text with table" in {
      val s = """# POWERTYPE

## InformationLifecycleState

Lifecycle state of editable and confirmed Information.

package = org.goldenport.cncf.information.value

| name      | label     |
| ---       | ---       |
| imported  | Imported  |
| confirmed | Confirmed |
"""
      val model = Model.parse(_config, s)
      val powertype = model.takePowertypeModel.classes("InformationLifecycleState")
      powertype.packageName should be ("org.goldenport.cncf.information.value")
      powertype.kinds.map(_.name) should be (Vector("imported", "confirmed"))
    }

    "normalize table attribute constraint metadata to record constraints" in {
      val s = """# ENTITY

## CountryCode

### ATTRIBUTE

| name  | type   | multiplicity | min_length | max_length | pattern     | format |
|-------+--------+--------------+------------+------------+-------------+--------|
| value | string | 1            | 2          | 2          | pass:[^[A-Z]{2}$] | uuid   |
"""
      val model = Model.parse(_config, s)
      val schema = model.takeEntityModel.get("CountryCode").getOrElse(fail("Entity CountryCode is missing")).schema
      val column = schema.columns.find(_.name == "value").getOrElse(fail("Column value is missing"))

      column.constraints.exists(_.isInstanceOf[CMinLength]) should be (true)
      column.constraints.exists(_.isInstanceOf[CMaxLength]) should be (true)
      column.constraints.collectFirst {
        case CRegex(pattern) => pattern.regex
      } should be (Some("^[A-Z]{2}$"))
      column.constraints.exists(_.isInstanceOf[CFormat]) should be (true)
    }

    "accept extended format names as CFormat constraints from table metadata" in {
      val s = """# ENTITY

## ContactProfile

### ATTRIBUTE

| name          | type   | multiplicity | format    |
|---------------+--------+--------------+-----------|
| created_at    | string | 1            | date-time |
| phone_number  | string | 1            | phone     |
"""
      val model = Model.parse(_config, s)
      val schema = model.takeEntityModel.get("ContactProfile").getOrElse(fail("Entity ContactProfile is missing")).schema
      val createdat = schema.columns.find(_.name == "created_at").getOrElse(fail("Column created_at is missing"))
      val phonenumber = schema.columns.find(_.name == "phone_number").getOrElse(fail("Column phone_number is missing"))
      val createdatformats = createdat.constraints.collect { case CFormat(f) => f.toLowerCase }
      val phoneformats = phonenumber.constraints.collect { case CFormat(f) => f.toLowerCase }

      createdatformats should contain ("date-time")
      phoneformats should contain ("phone")
    }

    "merge ATTRIBUTE table rows with subsection metadata by name" in {
      val s = """# ENTITY
                 |
                 |## Person
                 |
                 |### ATTRIBUTE
                 |
                 || name  | type   | multiplicity |
                 ||-------+--------+--------------|
                 || name  | name   | 1            |
                 || title | string | 1            |
                 |
                 |#### name
                 |
                 |##### DESCRIPTION
                 |
                 |Primary display name.
                 |
                 |#### title
                 |
                 |##### DESCRIPTION
                 |
                 |Job title shown in UI.
                 |""".stripMargin
      val model = Model.parse(_config, s)
      val entity = model.takeEntityModel.get("Person").getOrElse(fail("Entity Person is missing"))
      val name = entity.schemaClass.attributeMap.get("name").getOrElse(fail("Attribute name is missing"))
      val title = entity.schemaClass.attributeMap.get("title").getOrElse(fail("Attribute title is missing"))

      name.rawTypeName shouldBe Some("name")
      title.rawTypeName shouldBe Some("string")
      name.descriptionText shouldBe Some("Primary display name.")
      title.descriptionText shouldBe Some("Job title shown in UI.")
    }

    "accept yaml attribute record maps as base metadata" in {
      val s = """name: title
                 |type: string
                 |multiplicity: 1
                 |""".stripMargin
      val records = CmlSectionFormat.recordMaps(s)

      records should have size 1
      records.head.get("name") shouldBe Some("title")
      records.head.get("type") shouldBe Some("string")
      records.head.get("multiplicity") shouldBe Some("1")
    }

    "merge VIEW ATTRIBUTE table rows with subsection metadata by name" in {
      val s = """# ENTITY
                 |
                 |## Person
                 |
                 |### ATTRIBUTE
                 |
                 || name | type     | multiplicity |
                 ||------+----------+--------------|
                 || id   | entityid | 1            |
                 |
                 |### VIEW
                 |
                 |#### ATTRIBUTE
                 |
                 || name | type   | multiplicity |
                 ||------+--------+--------------|
                 || city | string | 1            |
                 |
                 |##### city
                 |
                 |type: text
                 |summary: City for list display.
                 |""".stripMargin
      val model = Model.parse(_config, s)
      val entity = model.takeEntityModel.get("Person").getOrElse(fail("Entity Person is missing"))
      val view = entity.view.getOrElse(fail("View definition is missing"))
      val city = view.attributes.find(_.name == "city").getOrElse(fail("View attribute city is missing"))

      city.datatype shouldBe Some("text")
      city.properties.get("summary") shouldBe Some("City for list display.")
    }

    "merge VIEW ATTRIBUTE hocon rows with subsection metadata by name" in {
      val s = """# ENTITY
                 |
                 |## Person
                 |
                 |### ATTRIBUTE
                 |
                 || name | type     | multiplicity |
                 ||------+----------+--------------|
                 || id   | entityid | 1            |
                 |
                 |### VIEW
                 |
                 |#### ATTRIBUTE
                 |
                 |city {
                 |  type = string
                 |  multiplicity = 1
                 |}
                 |
                 |##### city
                 |
                 |type: text
                 |summary: City for list display.
                 |""".stripMargin
      val model = Model.parse(_config, s)
      val entity = model.takeEntityModel.get("Person").getOrElse(fail("Entity Person is missing"))
      val view = entity.view.getOrElse(fail("View definition is missing"))
      val city = view.attributes.find(_.name == "city").getOrElse(fail("View attribute city is missing"))

      city.datatype shouldBe Some("text")
      city.properties.get("summary") shouldBe Some("City for list display.")
    }

    "merge OPERATION ATTRIBUTE table rows with subsection metadata by name" in {
      val s = """# COMMAND
                 |
                 |## SavePerson
                 |
                 |### ATTRIBUTE
                 |
                 || name  | type   | multiplicity |
                 ||-------+--------+--------------|
                 || title | string | 1            |
                 |
                 |#### title
                 |
                 |type: text
                 |""".stripMargin
      val model = Model.parse(_config, s)
      val opmodel = model.takeOperationModel
      val value = opmodel.values.find(_.name == "SavePerson").getOrElse(fail("SavePerson value is missing"))
      val title = value.fields.find(_.name == "title").getOrElse(fail("title field is missing"))

      title.datatype shouldBe "text"
    }

    "parse OPERATION ATTRIBUTE web form metadata" in {
      val s = """# COMMAND
                 |
                 |## SavePerson
                 |
                 |### ATTRIBUTE
                 |
                 || name | type   | multiplicity | web-label | web-control-type | web-placeholder | web-help         | web-required |
                 ||------+--------+--------------+-----------+------------------+-----------------+------------------+--------------|
                 || body | string | 1            | Body      | textarea         | Write body      | Main body text.  | true         |
                 |
                 |#### title
                 |
                 |type: string
                 |web-label: Subject
                 |web-placeholder: Short title
                 |web-help: Visible title.
                 |web-required: false
                 |""".stripMargin
      val model = Model.parse(_config, s)
      val opmodel = model.takeOperationModel
      val value = opmodel.values.find(_.name == "SavePerson").getOrElse(fail("SavePerson value is missing"))
      val body = value.fields.find(_.name == "body").getOrElse(fail("body field is missing"))
      val title = value.fields.find(_.name == "title").getOrElse(fail("title field is missing"))

      body.label shouldBe Some("Body")
      body.controlType shouldBe Some("textarea")
      body.placeholder shouldBe Some("Write body")
      body.help shouldBe Some("Main body text.")
      body.required shouldBe Some(true)
      title.label shouldBe Some("Subject")
      title.placeholder shouldBe Some("Short title")
      title.help shouldBe Some("Visible title.")
      title.required shouldBe Some(false)
    }

    "merge OPERATION ATTRIBUTE dl rows with subsection metadata by name" in {
      val s = """# COMMAND
                 |
                 |## SavePerson
                 |
                 |### ATTRIBUTE
                 |
                 |- title :: string
                 |
                 |#### title
                 |
                 |type: text
                 |""".stripMargin
      val model = Model.parse(_config, s)
      val opmodel = model.takeOperationModel
      val value = opmodel.values.find(_.name == "SavePerson").getOrElse(fail("SavePerson value is missing"))
      val title = value.fields.find(_.name == "title").getOrElse(fail("title field is missing"))

      title.datatype shouldBe "text"
    }



    "accept named view aliases in VIEW metadata" in {
      val s = """# ENTITY
                 |
                 |## Person
                 |
                 |### ATTRIBUTE
                 |
                 || name | type     | multiplicity |
                 ||------+----------+--------------|
                 || id   | entityid | 1            |
                 || name | name     | 1            |
                 |
                 |### VIEW
                 |
                 |views: summary, detail, summary
                 |rebuildable: true
                 |""".stripMargin
      val model = Model.parse(_config, s)
      val entity = model.takeEntityModel.get("Person").getOrElse(fail("Entity Person is missing"))
      val view = entity.view.getOrElse(fail("View definition is missing"))
      view.viewNames shouldBe Vector("summary", "detail")
      view.rebuildable shouldBe Some(true)
    }

    "merge DELEGATE value lines with subsection metadata by name" in {
      val s = """# ENTITY
                 |
                 |## UserProfile
                 |
                 |### DELEGATE
                 |
                 |IdentityPresentation
                 |PersonalProfile
                 |
                 |#### PersonalProfile
                 |
                 |multiplicity = ?
                 |""".stripMargin
      val model = Model.parse(_config, s)
      val entity = model.takeEntityModel.get("UserProfile").getOrElse(fail("Entity UserProfile is missing"))
      val delegates = entity.schemaClass.features.delegates
      val personal = delegates.find(_.name == "PersonalProfile").getOrElse(fail("PersonalProfile delegate is missing"))
      val identity = delegates.find(_.name == "IdentityPresentation").getOrElse(fail("IdentityPresentation delegate is missing"))

      delegates.map(_.name) shouldBe Vector("IdentityPresentation", "PersonalProfile")
      identity.multiplicity shouldBe "1"
      personal.multiplicity shouldBe "?"
    }

    "merge AGGREGATE STATE hocon rows with subsection metadata by name" in {
      val s = """# ENTITY
                 |
                 |## Person
                 |
                 |### ATTRIBUTE
                 |
                 || name | type     | multiplicity |
                 ||------+----------+--------------|
                 || id   | entityid | 1            |
                 |
                 |### AGGREGATE
                 |
                 |#### STATE
                 |
                 |status {
                 |  type = string
                 |  multiplicity = 1
                 |}
                 |
                 |##### status
                 |
                 |type: text
                 |multiplicity: 1
                 |summary: Aggregate snapshot status.
                 |""".stripMargin
      val model = Model.parse(_config, s)
      val entity = model.takeEntityModel.get("Person").getOrElse(fail("Entity Person is missing"))
      val aggregate = entity.aggregate.getOrElse(fail("Aggregate definition is missing"))
      val status = aggregate.state.find(_.name == "status").getOrElse(fail("Aggregate state status is missing"))

      status.datatype shouldBe Some("text")
      status.multiplicity shouldBe Some("1")
      status.properties.get("summary") shouldBe Some("Aggregate snapshot status.")
    }

    "accept VIEW QUERY subsection metadata records" in {
      val s = """# ENTITY
                 |
                 |## Person
                 |
                 |### ATTRIBUTE
                 |
                 || name | type     | multiplicity |
                 ||------+----------+--------------|
                 || id   | entityid | 1            |
                 |
                 |### VIEW
                 |
                 |#### QUERY
                 |
                 |##### recent
                 |
                 |expression = "status = 'ACTIVE'"
                 |cache = "PT5M"
                 |""".stripMargin
      val model = Model.parse(_config, s)
      val entity = model.takeEntityModel.get("Person").getOrElse(fail("Entity Person is missing"))
      val view = entity.view.getOrElse(fail("View definition is missing"))
      val recent = view.queries.find(_.name == "recent").getOrElse(fail("View query recent is missing"))

      recent.expression shouldBe Some("\"status = 'ACTIVE'\"")
      recent.properties.get("cache") shouldBe Some("\"PT5M\"")
    }

    }

    "service operation grammar" which {
    "accept SERVICE scoped OPERATION contract grammar" in {
      val s = """# SERVICE
                 |
                 |## Greeting
                 |
                 |### DESCRIPTION
                 |
                 |Greeting service.
                 |
                 |### OPERATION
                 |
                 |#### greeting
                 |
                 |Returns hello world text.
                 |
                 |- type :: QUERY
                 |- input :: GreetingQuery
                 |- output :: GreetingResult
                 |- execution :: async-job
                 |
                 |##### TYPE
                 |
                 |QUERY
                 |
                 |##### SUMMARY
                 |
                 |Return a greeting.
                 |
                 |##### DESCRIPTION
                 |
                 |Returns a greeting message for the supplied name.
                 |
                 |##### INPUT
                 |
                 |Greeting query payload.
                 |
                 |###### SUMMARY
                 |
                 |Greeting query payload.
                 |
                 |###### DESCRIPTION
                 |
                 |Structured query input accepted by greeting.
                 |
                 |###### TYPE
                 |
                 |GreetingQuery
                 |
                 |##### OUTPUT
                 |
                 |Greeting result payload.
                 |
                 |###### SUMMARY
                 |
                 |Greeting result payload.
                 |
                 |###### DESCRIPTION
                 |
                 |Structured result returned by greeting.
                 |
                 |###### TYPE
                 |
                 |GreetingResult
                 |""".stripMargin
      val model = Model.parse(_config, s)
      val service = model.getServiceModel.getOrElse(fail("ServiceModel is missing"))
      val greeting = service.classes.get("Greeting").flatMap(_.operations.getOperation("greeting")).getOrElse(fail("greeting operation is missing"))

      greeting.kind.map(_.toString) should be (Some("Query"))
      greeting.execution should be (Some("async-job"))
      greeting.summary should be (Some("Return a greeting."))
      greeting.description should be (Some("Returns a greeting message for the supplied name."))
      greeting.input.tpe should be (Some("GreetingQuery"))
      greeting.input.summary should be (Some("Greeting query payload."))
      greeting.input.description should be (Some("Structured query input accepted by greeting."))
      greeting.output.tpe should be (Some("GreetingResult"))
      greeting.output.summary should be (Some("Greeting result payload."))
      greeting.output.description should be (Some("Structured result returned by greeting."))
    }

    "accept SERVICE scoped OPERATION contract grammar with inline VALUE" in {
      val s = """# COMPONENT
                |
                |## Domain
                |
                |### PACKAGE
                |
                |domain
                |
                |# SERVICE
                |
                |## Greeting
                |
                |### DESCRIPTION
                |
                |Greeting service for inline operation contract testing.
                |
                |### OPERATION
                |
                |#### greeting
                |
                |Returns a greeting for the supplied name.
                |
                |##### TYPE
                |
                |QUERY
                |
                |##### SUMMARY
                |
                |Return a greeting.
                |
                |##### DESCRIPTION
                |
                |Returns a greeting message for the supplied name.
                |
                |##### INPUT
                |
                |Greeting query payload.
                |
                |###### VALUE
                |
                |####### GreetingQuery
                |
                |######## EXTENDS
                |
                |QueryAction
                |
                |######## ATTRIBUTE
                |
                || name | type | multiplicity |
                ||------+------|--------------|
                || name | name | 1            |
                |
                |##### OUTPUT
                |
                |Greeting result payload.
                |
                |###### VALUE
                |
                |####### GreetingResult
                |
                |######## EXTENDS
                |
                |OperationResult
                |
                |######## ATTRIBUTE
                |
                || message | type   | multiplicity |
                ||---------+--------+--------------|
                || message | string | 1            |
                |
                |# ENTITY
                |
                |## Person
                |
                |### ATTRIBUTE
                |
                || name | type     | multiplicity |
                ||------+----------+--------------|
                || id   | entityid | 1            |
                || name | name     | 1            |
                |""".stripMargin
      val model = Model.parse(_config, s)
      val service = model.getServiceModel.flatMap(_.classes.get("Greeting")).getOrElse(fail("ServiceModel is missing"))
      val greeting = service.operations.getOperation("greeting").getOrElse(fail("greeting operation is missing"))

      greeting.input.tpe should be (Some("GreetingQuery"))
      greeting.input.value.map(_.name) should be (Some("GreetingQuery"))
      greeting.output.tpe should be (Some("GreetingResult"))
      greeting.output.value.map(_.name) should be (Some("GreetingResult"))
    }

    "build anonymous operation-local input and output Values from the CML AST" in {
      Given("a query operation with ATTRIBUTE sections directly under INPUT and OUTPUT")
      val source = """# COMPONENT
                     |
                     |## Domain
                     |
                     |### PACKAGE
                     |domain
                     |
                     |# SERVICE
                     |
                     |## Notification
                     |
                     |### OPERATION
                     |
                     |#### searchNotifications
                     |
                     |##### TYPE
                     |QUERY
                     |
                     |##### INPUT
                     |
                     |###### ATTRIBUTE
                     || name | type   | multiplicity |
                     ||------+--------+--------------|
                     || text | string | ?            |
                     |
                     |##### OUTPUT
                     |
                     |###### ATTRIBUTE
                     || name  | type | multiplicity |
                     ||-------+------+--------------|
                     || total | int  | 1            |
                     |""".stripMargin

      When("Kaleidox parses the service operation through the common CML AST")
      val operation = Model.parse(_config, source).
        getServiceModel.
        flatMap(_.classes.get("Notification")).
        flatMap(_.operations.getOperation("searchNotifications")).
        getOrElse(fail("searchNotifications operation is missing"))

      Then("the anonymous local Values receive deterministic operation-local names")
      operation.input.tpe should be (Some("SearchNotificationsQuery"))
      operation.input.value.map(_.name) should be (Some("SearchNotificationsQuery"))
      operation.input.value.toVector.flatMap(_.schemaClass.attributes.map(_.name)) should be (Vector("text"))
      operation.output.tpe should be (Some("SearchNotificationsResult"))
      operation.output.value.map(_.name) should be (Some("SearchNotificationsResult"))
      operation.output.value.toVector.flatMap(_.schemaClass.attributes.map(_.name)) should be (Vector("total"))
    }

    "build named operation-local Values from canonical VALUE and ATTRIBUTE siblings" in {
      Given("a command operation with explicit local Value names")
      val source = """# COMPONENT
                     |
                     |## Domain
                     |
                     |### PACKAGE
                     |domain
                     |
                     |# SERVICE
                     |
                     |## Notification
                     |
                     |### OPERATION
                     |
                     |#### registerNotification
                     |
                     |##### TYPE
                     |COMMAND
                     |
                     |##### INPUT
                     |
                     |###### VALUE
                     |NotificationRegistration
                     |
                     |###### ATTRIBUTE
                     || name  | type   | multiplicity |
                     ||-------+--------+--------------|
                     || title | string | 1            |
                     |
                     |##### OUTPUT
                     |
                     |###### VALUE
                     |NotificationReceipt
                     |
                     |###### ATTRIBUTE
                     || name | type   | multiplicity |
                     ||------+--------+--------------|
                     || id   | string | 1            |
                     |""".stripMargin

      When("Kaleidox parses the service operation through the common CML AST")
      val operation = Model.parse(_config, source).
        getServiceModel.
        flatMap(_.classes.get("Notification")).
        flatMap(_.operations.getOperation("registerNotification")).
        getOrElse(fail("registerNotification operation is missing"))

      Then("the explicit names and schemas are retained without text reparsing")
      operation.input.tpe should be (Some("NotificationRegistration"))
      operation.input.value.map(_.name) should be (Some("NotificationRegistration"))
      operation.input.value.toVector.flatMap(_.schemaClass.attributes.map(_.name)) should be (Vector("title"))
      operation.output.tpe should be (Some("NotificationReceipt"))
      operation.output.value.map(_.name) should be (Some("NotificationReceipt"))
      operation.output.value.toVector.flatMap(_.schemaClass.slots.map(_.name)) should be (Vector("id"))
    }
    }
  }
}
