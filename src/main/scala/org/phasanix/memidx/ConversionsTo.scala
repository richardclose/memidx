package org.phasanix.memidx
import java.time.{Instant, LocalDate, LocalDateTime}
import java.util.Date

/**
  * Protocol for converting values of types from a fixed set
  * of primitive and library types to type A
  */
abstract class ConversionsTo[A](val nilValue: A) {
  def fromInt(value: Int): A
  def fromLong(value: Long): A
  def fromString(value: String): A
  def fromDouble(value: Double): A
  def fromFloat(value: Float): A
  def fromLocalDate(value: java.time.LocalDate): A
  def fromInstant(value: java.time.Instant): A
  def fromLocalDateTime(value: java.time.LocalDateTime): A
  def fromJUDate(value: java.util.Date): A
  def fromByte(value: Byte): A
  def fromChar(value: Char): A
  def fromBoolean(value: Boolean): A
}

abstract class BaseConversionsTo[A](nilValue: A) extends ConversionsTo[A](nilValue) {
  def fromInt(value: Int): A = nilValue
  def fromLong(value: Long): A = nilValue
  def fromString(value: String): A = nilValue
  def fromDouble(value: Double): A = nilValue
  def fromFloat(value: Float): A = nilValue
  def fromLocalDate(value: java.time.LocalDate): A = nilValue
  def fromInstant(value: java.time.Instant): A = nilValue
  def fromLocalDateTime(value: java.time.LocalDateTime): A = nilValue
  def fromJUDate(value: java.util.Date): A = nilValue
  def fromByte(value: Byte): A = nilValue
  def fromChar(value: Char): A = nilValue
  def fromBoolean(value: Boolean): A = nilValue
}

class ConversionsToString extends ConversionsTo[String]("<nil>") {

  def fromInt(value: Int): String = value.toString

  def fromLong(value: Long): String = value.toString

  def fromString(value: String): String = value

  def fromDouble(value: Double): String = value.toString

  def fromFloat(value: Float): String = value.toString

  def fromLocalDate(value: LocalDate): String = value.toString

  def fromInstant(value: Instant): String = value.toString

  def fromLocalDateTime(value: LocalDateTime): String = value.toString

  def fromJUDate(value: Date): String = value.toString

  def fromByte(value: Byte): String = value.toString

  def fromChar(value: Char): String = value.toString

  def fromBoolean(value: Boolean): String = value.toString
}