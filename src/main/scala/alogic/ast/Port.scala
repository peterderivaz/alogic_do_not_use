////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

// Ports have a name, some payload and possibly some flow control signals
sealed trait Port {
  val name: String
  val kind: Type

  def payload: List[Signal] = {
    def signals(prefix: String, kind: Type): List[Signal] = kind match {
      case t: ScalarType => List(Signal(s"${prefix}", t))
      case Struct(_, fields) => fields.toList flatMap {
        case (n, k) => signals(prefix + "_" + n, k)
      }
    }

    signals(name, kind)
  }

  def valid: Option[Signal] = this match {
    case _: PortNone   => None
    case _: PortValid  => Some(Signal(s"${name}_valid", IntType(false, 1)))
    case _: PortReady  => Some(Signal(s"${name}_valid", IntType(false, 1)))
    case _: PortAccept => Some(Signal(s"${name}_valid", IntType(false, 1)))
  }

  def ready: Option[Signal] = this match {
    case _: PortNone   => None
    case _: PortValid  => None
    case _: PortReady  => Some(Signal(s"${name}_ready", IntType(false, 1)))
    case _: PortAccept => None
  }

  def accept: Option[Signal] = this match {
    case _: PortNone   => None
    case _: PortValid  => None
    case _: PortReady  => None
    case _: PortAccept => Some(Signal(s"${name}_accept", IntType(false, 1)))
  }

  def flowControl: List[Signal] = List(valid, ready, accept).flatten

  def signals = payload ::: flowControl
}

object Port {
  def unapply(port: Port) = Some((port.name, port.kind))
}

case class PortNone(name: String, kind: Type) extends Port // no flow control
case class PortValid(name: String, kind: Type) extends Port // valid only
case class PortReady(name: String, kind: Type) extends Port // valid + ready
case class PortAccept(name: String, kind: Type) extends Port // valid + accept
