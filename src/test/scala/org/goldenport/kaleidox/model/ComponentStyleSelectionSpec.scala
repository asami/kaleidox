package org.goldenport.kaleidox.model

import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.goldenport.kaleidox.{Config, Model}

/*
 * @since   Jul. 31, 2026
 * @version Jul. 31, 2026
 * @author  ASAMI, Tomoharu
 */
final class ComponentStyleSelectionSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  private def _component(model: ComponentSubsystemModel, name: String): ComponentSubsystemModel.ComponentDefinition =
    model.components.find(_.name == name).getOrElse(fail(s"component '$name' is missing"))

  "ComponentSubsystemModel" should {
    "preserve explicit COMPONENT STYLE selections" in {
      Given("two component-only CML documents with distinct STYLE subsection values")
      val fullsource = """# COMPONENT
        |
        |## full
        |
        |### STYLE
        |
        |full-fledged-with-standalone
        |""".stripMargin
      val domainsource = """# COMPONENT
        |
        |## domain
        |
        |### STYLE
        |
        |domain-only
        |""".stripMargin

      When("Kaleidox parses the component-only documents")
      val fullmodel = Model.parse(Config.log.debug.withoutLocation, fullsource)
      val domainmodel = Model.parse(Config.log.debug.withoutLocation, domainsource)
      val fullcomponent = _component(fullmodel.takeComponentSubsystemModel, "full")
      val domaincomponent = _component(domainmodel.takeComponentSubsystemModel, "domain")

      Then("each ComponentDefinition retains its authored unversioned style selection")
      fullmodel.errors shouldBe empty
      domainmodel.errors shouldBe empty
      fullcomponent.componentStyle shouldBe Some("full-fledged-with-standalone")
      domaincomponent.componentStyle shouldBe Some("domain-only")
    }

    "keep legacy COMPONENT declarations style-less" in {
      Given("a component-only CML document without a STYLE subsection")
      val source = """# COMPONENT
        |
        |## legacy
        |""".stripMargin

      When("Kaleidox parses the legacy component declaration")
      val model = Model.parse(Config.log.debug.withoutLocation, source)
      val component = _component(model.takeComponentSubsystemModel, "legacy")

      Then("the typed selection remains absent for compatibility")
      model.errors shouldBe empty
      component.componentStyle shouldBe None
    }
  }
}
