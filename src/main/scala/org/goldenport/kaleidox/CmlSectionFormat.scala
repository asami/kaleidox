package org.goldenport.kaleidox

import scala.util.Try
import scala.collection.JavaConverters._
import com.typesafe.config.{Config => Hocon, ConfigFactory, ConfigValueFactory}
import org.yaml.snakeyaml.Yaml

/*
 * @since   Mar. 24, 2026
 *  version Mar. 24, 2026
 * @version Apr.  3, 2026
 * @author  ASAMI, Tomoharu
 */
object CmlSectionFormat {
  def recordMaps(p: String): Vector[Map[String, String]] =
    _parse_structured(p).map(_to_record_maps).getOrElse(Vector.empty)

  def keyValues(p: String): Vector[(String, String)] = {
    val dl = _key_values_dl(p)
    if (dl.nonEmpty)
      dl
    else
      _structured_pairs(p)
  }

  def valueLines(p: String): Vector[String] = {
    val dl = _value_lines_dl(p)
    if (dl.nonEmpty)
      dl
    else
      _structured_value_lines(p)
  }

  def parseConfig(p: String): Hocon = {
    val s = Option(p).getOrElse("").trim
    if (s.isEmpty)
      ConfigFactory.empty()
    else
      _parse_yaml_hocon(s).orElse(_parse_hocon(s)).getOrElse(ConfigFactory.parseString(s))
  }

  def fieldDefinitions(p: String): Vector[(String, String, String)] = {
    val dl = _field_definitions_dl(p)
    if (dl.nonEmpty)
      dl
    else
      _field_definitions_structured(p)
  }

  private def _key_values_dl(p: String): Vector[(String, String)] =
    Option(p).getOrElse("").split("\\r?\\n").toVector.flatMap { x =>
      val s = x.trim
      if (s.isEmpty)
        None
      else {
        val a = if (s.startsWith("-")) s.drop(1).trim else s
        val i1 = a.indexOf("::")
        val i2 = a.indexOf("=")
        val i =
          if (i1 > 0) i1
          else if (i2 > 0) i2
          else -1
        if (i <= 0)
          None
        else {
          val k = a.substring(0, i).trim.toLowerCase
          val v = if (i == i1) a.substring(i + 2).trim else a.substring(i + 1).trim
          if (k.isEmpty || v.isEmpty)
            None
          else
            Some(k -> v)
        }
      }
    }

  private def _value_lines_dl(p: String): Vector[String] =
    Option(p).getOrElse("").split("\\r?\\n").toVector.map(_.trim).map { x =>
      if (x.startsWith("-")) x.drop(1).trim else x
    }.filterNot(_.isEmpty)

  private def _field_definitions_dl(p: String): Vector[(String, String, String)] =
    Option(p).getOrElse("").split("\\r?\\n").toVector.flatMap { x =>
      val s0 = x.trim
      val s = if (s0.startsWith("-")) s0.drop(1).trim else s0
      if (s.isEmpty)
        None
      else {
        val i = s.indexOf("::")
        if (i <= 0)
          None
        else {
          val n = s.substring(0, i).trim
          val t = s.substring(i + 2).trim
          if (n.isEmpty || t.isEmpty)
            None
          else
            Some((n, t, "1"))
        }
      }
    }

  private def _structured_pairs(p: String): Vector[(String, String)] =
    _parse_structured(p).map(_to_pairs).getOrElse(Vector.empty)

  private def _structured_value_lines(p: String): Vector[String] =
    _parse_structured(p).map {
      case m: java.util.Map[_, _] =>
        _to_pairs(m).map { case (k, v) => s"$k = $v" }
      case m: java.util.List[_] =>
        m.asScala.toVector.map(_to_value_string).filterNot(_.isEmpty)
      case _ =>
        Vector.empty
    }.getOrElse(Vector.empty)

  private def _field_definitions_structured(p: String): Vector[(String, String, String)] =
    _parse_structured(p).map(_to_field_definitions).getOrElse(Vector.empty)

  private def _to_field_definitions(p: Any): Vector[(String, String, String)] =
    p match {
      case m: java.util.List[_] =>
        m.asScala.toVector.flatMap(_to_field_definition)
      case m: java.util.Map[_, _] =>
        val single = _to_field_definition(m)
        if (single.nonEmpty)
          single
        else
          m.asScala.toVector.flatMap {
            case (k, v) =>
              _to_field_definition(v).headOption.map { case (_, t, multi) =>
                (k.toString, t, multi)
              }.orElse {
                _to_scalar_string(v).map(s => (k.toString, s, "1"))
              }
          }
      case _ =>
        Vector.empty
    }

  private def _to_field_definition(p: Any): Vector[(String, String, String)] =
    p match {
      case m: java.util.Map[_, _] =>
        val z = m.asScala.toVector.map { case (k, v) => k.toString.toLowerCase -> v }.toMap
        val name = z.get("name").flatMap(_to_scalar_string)
        val tpe = z.get("type").orElse(z.get("datatype")).flatMap(_to_scalar_string)
        val multi = z.get("multiplicity").flatMap(_to_scalar_string).getOrElse("1")
        (name, tpe) match {
          case (Some(n), Some(t)) if n.nonEmpty && t.nonEmpty =>
            Vector((n, t, multi))
          case _ =>
            Vector.empty
        }
      case _ =>
        Vector.empty
    }

  private def _to_record_maps(p: Any): Vector[Map[String, String]] =
    p match {
      case m: java.util.List[_] =>
        m.asScala.toVector.flatMap(_to_record_map)
      case m: java.util.Map[_, _] =>
        val single = _to_record_map(m)
        if (single.nonEmpty)
          single
        else
          m.asScala.toVector.flatMap {
            case (k, v) =>
              val nested = _to_record_map(v)
              if (nested.nonEmpty)
                nested.map { x =>
                  if (x.contains("name"))
                    x
                  else
                    x + ("name" -> k.toString.trim)
                }
              else
                _to_scalar_string(v).map(s => Map("name" -> k.toString.trim, "value" -> s)).toVector
          }
      case _ =>
        Vector.empty
    }

  private def _to_record_map(p: Any): Vector[Map[String, String]] =
    p match {
      case m: java.util.Map[_, _] =>
        val fields = m.asScala.toVector.flatMap { case (k, v) =>
          _to_scalar_string(v).map(k.toString.trim.toLowerCase -> _)
        }.toMap
        if (fields.isEmpty) Vector.empty else Vector(fields)
      case _ =>
        Vector.empty
    }

  private def _to_pairs(p: Any): Vector[(String, String)] =
    p match {
      case m: java.util.Map[_, _] =>
        m.asScala.toVector.flatMap { case (k, v) =>
          _to_pairs(k.toString.toLowerCase, v)
        }
      case m: java.util.List[_] =>
        m.asScala.toVector.flatMap(_to_pairs)
      case _ =>
        Vector.empty
    }

  private def _to_pairs(key: String, p: Any): Vector[(String, String)] =
    p match {
      case null =>
        Vector.empty
      case m: java.util.Map[_, _] =>
        m.asScala.toVector.flatMap { case (k, v) =>
          _to_scalar_string(v).map { s =>
            key -> s"${k.toString.trim}=${s}"
          }
        }
      case m: java.util.List[_] =>
        m.asScala.toVector.flatMap(_to_pairs(key, _))
      case _ =>
        _to_scalar_string(p).map(s => key -> s).toVector
    }

  private def _to_scalar_string(p: Any): Option[String] =
    p match {
      case null => None
      case m: String =>
        val s = m.trim
        if (s.isEmpty) None else Some(s)
      case m => Some(m.toString.trim).filterNot(_.isEmpty)
    }

  private def _to_value_string(p: Any): String =
    p match {
      case null => ""
      case m: String => m.trim
      case m => m.toString.trim
    }

  private def _parse_structured(p: String): Option[Any] =
    _parse_yaml_any(p).orElse(_parse_hocon_any(p))

  private def _parse_hocon(s: String): Option[Hocon] =
    Try(ConfigFactory.parseString(s)).toOption

  private def _parse_hocon_any(p: String): Option[Any] =
    _parse_hocon(Option(p).getOrElse("")).map(_.root().unwrapped())

  private def _parse_yaml_hocon(p: String): Option[Hocon] =
    _parse_yaml_any(p).collect {
      case m: java.util.Map[_, _] =>
        val jmap = m.asInstanceOf[java.util.Map[String, AnyRef]]
        ConfigFactory.parseMap(jmap)
    }

  private def _parse_yaml_any(p: String): Option[Any] =
    Try(new Yaml().load[Any](Option(p).getOrElse(""))).toOption
}
