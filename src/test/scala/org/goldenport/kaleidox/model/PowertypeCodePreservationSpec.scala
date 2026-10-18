package org.goldenport.kaleidox.model

import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner
import org.goldenport.kaleidox.{Config, Model}

/*
 * @since   Oct. 19, 2026
 * @version Oct. 19, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class PowertypeCodePreservationSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  private val _formats = Vector("table", "org-table", "sections", "kinds", "kind")

  "CML powertype declarations" should {
    "preserve declared codes" which {
      _formats.foreach { format =>
        s"retain fixed codes, lowercase names and labels in $format" in {
          Given("a powertype whose explicit codes differ from declaration order")
          val source = _source(format, Vector(
            ("confirmed", Some("2"), "Confirmed"),
            ("imported", Some("1"), "Imported")
          ))

          When("the public parser builds its powertype model")
          val model = Model.parse(Config.default, source)

          Then("the declared codes and content are preserved in the default package")
          withClue(s"Model errors: ${model.errors.mkString("; ")}") { model.errors shouldBe empty }
          val powertype = model.takePowertypeModel.classes("Delivery")
          powertype.packageName shouldBe "domain"
          powertype.kinds.map(kind => (kind.name, kind.value, kind.label)) shouldBe Vector(
            ("confirmed", Some(2), Some("Confirmed")),
            ("imported", Some(1), Some("Imported"))
          )
        }

        s"keep code identity through ordering and label changes in $format" in {
          Given("every ordering of kinds with revised presentation labels")
          val rows = Vector(("imported", Some("1"), "New imported label"), ("confirmed", Some("2"), "New confirmed label"))
          val sources = rows.permutations.map(order => _source(format, order)).toVector

          When("the public parser interprets the declarations")
          val models = sources.map(source => Model.parse(Config.default, source))

          Then("each name retains its written code independently of order and label")
          models.foreach { model =>
            withClue(s"Model errors: ${model.errors.mkString("; ")}") { model.errors shouldBe empty }
            val kinds = model.takePowertypeModel.classes("Delivery").kinds
            kinds.map(kind => kind.name -> kind.value).toMap shouldBe Map("imported" -> Some(1), "confirmed" -> Some(2))
            kinds.map(kind => kind.name -> kind.label).toMap shouldBe Map(
              "imported" -> Some("New imported label"), "confirmed" -> Some("New confirmed label")
            )
          }
        }

        s"retain genuinely omitted transient codes as None in $format" in {
          Given("a transient powertype with omitted and explicit codes")
          val source = _source(format, Vector(
            ("draft", None, "Draft"), ("imported", Some("1"), "Imported"), ("pending", None, "Pending")
          ))

          When("the public parser interprets the declaration")
          val model = Model.parse(Config.default, source)

          Then("omissions remain unspecified without rejecting the transient declaration")
          withClue(s"Model errors: ${model.errors.mkString("; ")}") { model.errors shouldBe empty }
          model.takePowertypeModel.classes("Delivery").kinds.map(_.value) shouldBe Vector(None, Some(1), None)
        }

        s"accept zero and both Int boundaries in $format" in {
          Given("explicit signed base-10 codes covering the Int boundaries")
          val source = _source(format, Vector(
            ("zero", Some("+0"), "Zero"),
            ("minimum", Some("-2147483648"), "Minimum"),
            ("maximum", Some("+2147483647"), "Maximum")
          ))

          When("the public parser interprets the declaration")
          val model = Model.parse(Config.default, source)

          Then("every representable code is preserved exactly")
          withClue(s"Model errors: ${model.errors.mkString("; ")}") { model.errors shouldBe empty }
          model.takePowertypeModel.classes("Delivery").kinds.map(_.value) shouldBe Vector(Some(0), Some(Int.MinValue), Some(Int.MaxValue))
        }
      }

      "accept numeric and string HOCON codes without changing an explicit package" in {
        Given("HOCON kinds mixing numeric and string representations")
        val source = """# POWERTYPE
          |
          |## Delivery
          |
          |package = example.delivery
          |kinds = [
          |  { name = imported, value = 1, label = Imported },
          |  { name = confirmed, value = "2", label = Confirmed }
          |]
          |""".stripMargin

        When("the public parser interprets the declaration")
        val model = Model.parse(Config.default, source)

        Then("both written codes and the explicit package survive")
        withClue(s"Model errors: ${model.errors.mkString("; ")}") { model.errors shouldBe empty }
        val powertype = model.takePowertypeModel.classes("Delivery")
        powertype.packageName shouldBe "example.delivery"
        powertype.kinds.map(_.value) shouldBe Vector(Some(1), Some(2))
      }

      "allow separate powertypes to reuse the same explicit code" in {
        Given("two distinct powertypes using code one")
        val source = _source("org-table", Vector(("imported", Some("1"), "Imported"))) +
          "\n## Receipt\n\n| name | value |\n|------+-------|\n| received | 1 |\n"

        When("the public parser interprets both declarations")
        val model = Model.parse(Config.default, source)

        Then("code uniqueness is local to each powertype")
        withClue(s"Model errors: ${model.errors.mkString("; ")}") { model.errors shouldBe empty }
        model.takePowertypeModel.classes("Delivery").kinds.map(_.value) shouldBe Vector(Some(1))
        model.takePowertypeModel.classes("Receipt").kinds.map(_.value) shouldBe Vector(Some(1))
      }
    }

    "reject invalid explicit codes before model generation" which {
      _formats.foreach { format =>
        s"report malformed and overflowing codes with type and kind context in $format" in {
          Given("explicit fractional, nonnumeric, nondecimal and out-of-range values")
          val values = Vector("1.5", "invalid", "0x10", "1e2", "+", "2147483648", "-2147483649")
          val sources = values.map(value => _source(format, Vector(("imported", Some(value), "Imported"))))

          When("the public parser interprets each invalid declaration")
          val models = sources.map(source => Model.parse(Config.default, source))

          Then("each invalid value produces a contextual model failure instead of an omitted or positional code")
          models.foreach(model => _should_reject(model, Vector("Delivery", "imported")))
        }

        s"reject duplicate explicit codes including signed zero in $format" in {
          Given("two kinds declaring the same numeric identity")
          val pairs = Vector(("1", "+1"), ("0", "+0"), ("-0", "0"))
          val sources = pairs.map { case (first, second) =>
            _source(format, Vector(("imported", Some(first), "Imported"), ("confirmed", Some(second), "Confirmed")))
          }

          When("the public parser interprets each declaration")
          val models = sources.map(source => Model.parse(Config.default, source))

          Then("each collision fails with the powertype and both kind names")
          models.foreach(model => _should_reject(model, Vector("Delivery", "imported", "confirmed", "duplicate")))
        }
      }

      "reject explicit HOCON null and fractional numeric values" in {
        Given("explicit null and fractional values in HOCON kinds and kind sections")
        val sources = Vector("null", "1.5").flatMap { value =>
          Vector("kinds", "kind").map { alias =>
            s"# POWERTYPE\n\n## Delivery\n\n$alias = [{ name = imported, value = $value }]\n"
          } :+ s"# POWERTYPE\n\n## Delivery\n\n### imported\n\nvalue = $value\n"
        }

        When("the public parser interprets the declarations")
        val models = sources.map(source => Model.parse(Config.default, source))

        Then("written nulls and fractions fail with declaration context")
        models.foreach(model => _should_reject(model, Vector("Delivery", "imported")))
      }
    }
  }

  private def _should_reject(model: Model, context: Vector[String]): Unit = {
    model.errors should not be empty
    model.takePowertypeModel.isEmpty shouldBe true
    val diagnostic = model.errors.mkString("; ")
    diagnostic should include ("POWERTYPE")
    context.foreach(word => diagnostic should include (word))
  }

  private def _source(format: String, rows: Vector[(String, Option[String], String)]): String = {
    val body = format match {
      case "table" | "org-table" =>
        val separator = if (format == "org-table") "|------+-------+-------|" else "| --- | --- | --- |"
        val records = rows.map { case (name, value, label) => s"| $name | ${value.getOrElse("")} | $label |" }
        (Vector("| name | value | label |", separator) ++ records).mkString("\n")
      case "sections" =>
        rows.map { case (name, value, label) =>
          val code = value.map(code => s"""value = "$code"
""").getOrElse("")
          s"""### $name

${code}label = "$label"
"""
        }.mkString("\n")
      case "kinds" | "kind" =>
        val records = rows.map { case (name, value, label) =>
          val code = value.map(code => s""", value = "$code"""").getOrElse("")
          s"""{ name = "$name"${code}, label = "$label" }"""
        }
        s"$format = [${records.mkString(", ")}]"
    }
    s"# POWERTYPE\n\n## Delivery\n\n$body\n"
  }
}
