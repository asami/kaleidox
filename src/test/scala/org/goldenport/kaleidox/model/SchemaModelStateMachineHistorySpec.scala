package org.goldenport.kaleidox.model

import org.goldenport.kaleidox.{Config, Model}
import org.goldenport.sm.NamedHistoryTransitionTo
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/*
 * @since Aug. 14, 2026
 * @version Aug. 14, 2026
 * @author ASAMI, Tomoharu
 */
final class SchemaModelStateMachineHistorySpec
  extends AnyWordSpec
  with Matchers
  with GivenWhenThen {

  "SchemaModel StateMachine CML" should {
    "preserve direct-leaf targets, flattened values, and named shallow-history metadata" in {
      Given("an entity StateMachine with an existing HISTORY-FIELD, Review leaves, and direct-leaf transitions")
      val model = Model.parse(Config.log.debug.withoutLocation, _source)
      val person = model.takeEntityModel.get("Person").getOrElse(fail("Person is missing"))

      When("Kaleidox parses the CML state hierarchy")
      val rule = person.stateMachines.headOption.map(_.rule).getOrElse(fail("lifecycle is missing"))
      val review = rule.statemachines.find(_.name.contains("Review")).getOrElse(fail("Review is missing"))

      Then("the named history target, direct leaves, and source-order flattened values are retained")
      rule.historyFieldName shouldBe Some("lifecycleHistory")
      rule.states.map(x => x.name -> x.value) shouldBe List("Draft" -> 1, "Suspended" -> 4)
      review.states.map(_.name) shouldBe List("Pending", "Approved")
      review.states.map(x => x.name -> x.value) shouldBe List("Pending" -> 2, "Approved" -> 3)
      rule.states.find(_.name == "Draft").flatMap(_.transitions.call.headOption).map(_.to) shouldBe
        Some(org.goldenport.sm.NameTransitionTo("Pending"))
      review.states.find(_.name == "Pending").flatMap(_.transitions.call.headOption).map(_.to) shouldBe
        Some(org.goldenport.sm.NameTransitionTo("Approved"))
      rule.states.find(_.name == "Suspended").flatMap(_.transitions.call.headOption).map(_.to) shouldBe
        Some(NamedHistoryTransitionTo("Review"))
    }
  }

  private val _source = """# ENTITY
    |
    |## Person
    |
    |### ATTRIBUTE
    |
    || name             | type     | multiplicity |
    ||------------------+----------+--------------|
    || id               | entityid | 1            |
    || lifecycleHistory | record   | 1            |
    |
    |### StateMachine
    |
    |#### lifecycle
    |- HISTORY-FIELD :: lifecycleHistory
    |
    |##### State
    |
    |###### Draft
    |
    |####### Transition
    |- to :: Pending
    |- on :: submit
    |
    |###### Review
    |
    |####### State
    |
    |######## Pending
    |
    |######### Transition
    |- to :: Approved
    |- on :: approve
    |
    |######## Approved
    |
    |######### Transition
    |- to :: Suspended
    |- on :: suspend
    |
    |###### Suspended
    |
    |####### Transition
    |- to :: Review.HISTORY
    |- on :: resume
    |
    |##### Event
    |
    |###### submit
    |
    |###### approve
    |
    |###### suspend
    |
    |###### resume
    |""".stripMargin
}
