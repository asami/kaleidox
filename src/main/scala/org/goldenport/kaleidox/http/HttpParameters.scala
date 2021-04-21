package org.goldenport.kaleidox.http

/*
 * @since   Apr. 21, 2021
 * @version Apr. 22, 2021
 * @author  ASAMI, Tomoharu
 */
case class HttpParameters(
  controls: HttpParameters.Controls = HttpParameters.Controls.default
) {
}

object HttpParameters {
  case class Controls(
    isTrace: Boolean = false,
    isMetrics: Boolean = false
  )
  object Controls {
    val default = Controls()
  }

  def create(p: HttpRequest): HttpParameters = {
    val q = p.queryMeta.toRecord
    val c = Controls(
      q.asBoolean("trace", Controls.default.isTrace),
      q.asBoolean("metrics", Controls.default.isMetrics)
    )
    HttpParameters(c)
  }
}
