package org.goldenport.kaleidox

import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner
import org.smartdox.{Section, Text}
import org.goldenport.record.v2.{CFormat, CMaxLength, CMinLength, CRegex}

/*
 * @since   Mar. 24, 2026
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class CmlSectionFormatSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  private val config = Config.log.debug.withoutLocation

  "Model parser" should {
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
      val model = Model.parse(config, s)
      val opmodel = model.takeOperationModel
      opmodel.operations.map(_.name) should contain ("createOrder")
      opmodel.values.map(_.name) should contain ("CreateOrder")
      opmodel.values.find(_.name == "CreateOrder").map(_.fields.size) should be (Some(2))
      opmodel.normalizedOperations.head.inputType should be ("CreateOrder")
    }

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
      val model = Model.parse(config, s)
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
      val model = Model.parse(config, s)
      val cs = model.takeComponentSubsystemModel
      cs.components.map(_.name) should contain ("person")
      cs.subsystems.map(_.name) should contain ("identity")
      cs.components.head.coordinates.size should be (1)
      cs.subsystems.head.config.get("profile") should be (Some("prod"))
    }

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
      val model = Model.parse(config, s)
      val entity = model.takeEntityModel.get("Person").getOrElse(fail("Entity Person is missing"))
      entity.parents.nonEmpty should be (true)
    }

    "normalize table attribute constraint metadata to record constraints" in {
      val s = """# ENTITY

## CountryCode

### ATTRIBUTE

| name  | type   | multiplicity | min_length | max_length | pattern     | format |
|-------+--------+--------------+------------+------------+-------------+--------|
| value | string | 1            | 2          | 2          | ^[A-Z]{2}$  | uuid   |
"""
      val model = Model.parse(config, s)
      val schema = model.takeEntityModel.get("CountryCode").getOrElse(fail("Entity CountryCode is missing")).schema
      val column = schema.columns.find(_.name == "value").getOrElse(fail("Column value is missing"))

      column.constraints.exists(_.isInstanceOf[CMinLength]) should be (true)
      column.constraints.exists(_.isInstanceOf[CMaxLength]) should be (true)
      column.constraints.exists(_.isInstanceOf[CRegex]) should be (true)
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
      val model = Model.parse(config, s)
      val schema = model.takeEntityModel.get("ContactProfile").getOrElse(fail("Entity ContactProfile is missing")).schema
      val createdAt = schema.columns.find(_.name == "created_at").getOrElse(fail("Column created_at is missing"))
      val phoneNumber = schema.columns.find(_.name == "phone_number").getOrElse(fail("Column phone_number is missing"))
      val createdAtFormats = createdAt.constraints.collect { case CFormat(f) => f.toLowerCase }
      val phoneFormats = phoneNumber.constraints.collect { case CFormat(f) => f.toLowerCase }

      createdAtFormats should contain ("date-time")
      phoneFormats should contain ("phone")
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
      val model = Model.parse(config, s)
      val entity = model.takeEntityModel.get("Person").getOrElse(fail("Entity Person is missing"))
      val view = entity.view.getOrElse(fail("View definition is missing"))
      view.viewNames shouldBe Vector("summary", "detail")
      view.rebuildable shouldBe Some(true)
    }

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
      val model = Model.parse(config, s)
      val service = model.getServiceModel.getOrElse(fail("ServiceModel is missing"))
      val greeting = service.classes.get("Greeting").flatMap(_.operations.getOperation("greeting")).getOrElse(fail("greeting operation is missing"))

      greeting.kind.map(_.toString) should be (Some("Query"))
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
      val model = Model.parse(config, s)
      val service = model.getServiceModel.flatMap(_.classes.get("Greeting")).getOrElse(fail("ServiceModel is missing"))
      val greeting = service.operations.getOperation("greeting").getOrElse(fail("greeting operation is missing"))

      greeting.input.tpe should be (Some("GreetingQuery"))
      greeting.input.value.map(_.name) should be (Some("GreetingQuery"))
      greeting.output.tpe should be (Some("GreetingResult"))
      greeting.output.value.map(_.name) should be (Some("GreetingResult"))
    }
  }
}
