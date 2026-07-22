package org.goldenport.kaleidox

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/*
 * Executable specification for operation evaluation CML metadata.
 *
 * @since   Jul. 23, 2026
 * @version Jul. 23, 2026
 * @author  ASAMI, Tomoharu
 */
final class OperationEvaluationModelSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  private val _config = Config.log.debug.withoutLocation

  "Operation evaluation CML" should {
    "normalize the same logical declaration through operation and service surfaces" in {
      Given("top-level and service operations with required admission and every terminal outcome")
      val sources = Vector(
        _operation_source("candidate", "required", "success, failure, timeout, cancellation"),
        _service_source("candidate", "required", "success, failure, timeout, cancellation")
      )

      When("Kaleidox parses and normalizes both operation contracts")
      val evaluations = sources.map(_evaluation)

      Then("both parsers produce the same complete logical declaration")
      evaluations.distinct should have size 1
      evaluations.head.corpus.map(_.profile) shouldBe Some("route-resolution")
      evaluations.head.corpus.map(_.admission) shouldBe Some("required")
      evaluations.head.corpus.map(_.outcomes) shouldBe Some(Vector("success", "failure", "timeout", "cancellation"))
      evaluations.head.experiment.map(_.purpose) shouldBe Some("route-resolution")
      evaluations.head.experiment.map(_.admission) shouldBe Some("optional")
      evaluations.head.experiment.flatMap(_.variantProfile) shouldBe Some("execution-plan")
    }

    "reject malformed policy values on both operation surfaces" in {
      Given("equivalent declarations with unsupported capture, admission, names, and booleans")
      val valid = Vector(
        _operation_source("candidate", "required", "success"),
        _service_source("candidate", "required", "success")
      )
      val malformed = valid.flatMap { source =>
        Vector(
          source.replace("capture: candidate", "capture: raw-payload"),
          source.replace("admission: required", "admission: deferred"),
          source.replace("profile: route-resolution", "profile: unsafe name"),
          source.replace("eligible: true", "eligible: perhaps")
        )
      }

      When("Kaleidox parses every malformed declaration")
      val errors = malformed.map(x => Model.parse(_config, x).errors.mkString(";"))

      Then("each parser rejects the same invalid policy boundary deterministically")
      errors should have size 8
      errors.foreach(_ should not be empty)
      errors.count(_.contains("CORPUS capture")) shouldBe 2
      errors.count(_.contains("CORPUS admission")) shouldBe 2
      errors.count(_.contains("bounded logical name")) shouldBe 2
      errors.count(_.contains("EXPERIMENT eligible")) shouldBe 2
    }
  }

  private def _evaluation(source: String): org.goldenport.kaleidox.model.OperationModel.EvaluationDefinition = {
    val model = Model.parse(_config, source)
    model.takeOperationModel.normalizedOperations.headOption.flatMap(_.evaluation)
      .orElse(
        model.getServiceModel
          .flatMap(_.classes.get("Routing"))
          .flatMap(_.operations.getOperation("evaluateRoute"))
          .flatMap(_.evaluation)
      )
      .getOrElse(fail(s"evaluation declaration missing: \${model.errors.mkString(";")}"))
  }

  private def _operation_source(
    capture: String,
    admission: String,
    outcomes: String
  ): String =
    s"""# COMMAND
       |
       |## EvaluateRoute
       |
       |### ATTRIBUTE
       |
       || name | type | multiplicity |
       ||------+------|--------------|
       || id   | name | 1            |
       |
       |# OPERATION
       |
       |## evaluateRoute
       |
       |type: COMMAND
       |input: EvaluateRoute
       |output: RouteResult
       |
       |### EVALUATION
       |
       |#### CORPUS
       |
       |capture: $capture
       |profile: route-resolution
       |admission: $admission
       |outcomes: $outcomes
       |sampling: representative
       |redaction: default
       |
       |#### EXPERIMENT
       |
       |eligible: true
       |purpose: route-resolution
       |variant-profile: execution-plan
       |""".stripMargin

  private def _service_source(
    capture: String,
    admission: String,
    outcomes: String
  ): String =
    s"""# SERVICE
       |
       |## Routing
       |
       |### OPERATION
       |
       |#### evaluateRoute
       |
       |##### TYPE
       |COMMAND
       |##### INPUT
       |###### TYPE
       |EvaluateRouteCommand
       |##### OUTPUT
       |###### TYPE
       |EvaluateRouteResult
       |##### EVALUATION
       |###### CORPUS
       |capture: $capture
       |profile: route-resolution
       |admission: $admission
       |outcomes: $outcomes
       |sampling: representative
       |redaction: default
       |###### EXPERIMENT
       |eligible: true
       |purpose: route-resolution
       |variant-profile: execution-plan
       |""".stripMargin
}
