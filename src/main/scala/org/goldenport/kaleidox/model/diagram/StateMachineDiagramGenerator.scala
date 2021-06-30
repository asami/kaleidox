package org.goldenport.kaleidox.model.diagram

import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.{Context => UmlContext, Config => UmlConfig}
import org.simplemodeling.SimpleModeler.generators.uml.{StateMachineDiagramGenerator => UmlStateMachineDiagramGenerator}
import org.goldenport.sexpr._
import org.goldenport.cli.Environment

/*
 * @since   Jun. 27, 2021
 * @version Jun. 27, 2021
 * @author  ASAMI, Tomoharu
 */
class StateMachineDiagramGenerator(
  environment: Environment,
  model: SimpleModel
) {
  private val _generator = {
    val config = UmlConfig.create(environment)
    val context = new UmlContext(environment, config)
    new UmlStateMachineDiagramGenerator(context, model)
  }

  def generate(p: MStateMachine): SImage = {
    val binary = _generator.makeStateMachineDiagramPng(p)
    SImage.png(binary)
  }
}
