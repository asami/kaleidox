package org.goldenport.kaleidox

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.record.v2.{CMaxLength, CMinLength, CRegex}

/*
 * @since   Mar. 24, 2026
 * @version Mar. 25, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class CmlSectionFormatSpec extends WordSpec with Matchers with GivenWhenThen {
  private val config = Config.log.debug.withoutLocation

  "Model parser" should {
    "accept YAML in OPERATION/COMMAND sections" in {
      val s = """* OPERATION
** createOrder
type: COMMAND
input: CreateOrder
output: CreateOrderResult

* COMMAND
** CreateOrder
*** ATTRIBUTE
- name: orderId
  type: OrderId
  multiplicity: 1
- name: amount
  type: Money
  multiplicity: 1
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

    "normalize attribute constraint metadata to record constraints" in {
      val s = """* ENTITY
** CountryCode
*** ATTRIBUTE
- name: value
  type: String
  multiplicity: 1
  min_length: 2
  max_length: 2
  pattern: "^[A-Z]{2}$"
  format: uuid
"""
      val model = Model.parse(config, s)
      val schema = model.takeEntityModel.get("CountryCode").getOrElse(fail("Entity CountryCode is missing")).schema
      val column = schema.columns.find(_.name == "value").getOrElse(fail("Column value is missing"))
      val regexCount = column.constraints.count(_.isInstanceOf[CRegex])
      val hasFormalFormat = column.constraints.exists(_.getClass.getSimpleName == "CFormat")
      val hasFormatConstraint = hasFormalFormat || regexCount >= 2

      column.constraints.exists(_.isInstanceOf[CMinLength]) should be (true)
      column.constraints.exists(_.isInstanceOf[CMaxLength]) should be (true)
      regexCount should be >= 1
      hasFormatConstraint should be (true)
    }
  }
}
