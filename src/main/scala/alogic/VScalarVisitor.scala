////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

import org.antlr.v4.runtime.tree.RuleNode
import alogic.antlr.VParserBaseVisitor
import org.antlr.v4.runtime.tree.ParseTree

import Antlr4Conversions._

class VScalarVisitor[T] extends VBaseVisitor[T, List[T], Option[T]] {
  override def visit[U <: RuleNode](ctxList: List[U]): List[T] = ctxList map visit

  override def visit[U <: RuleNode](ctxOpt: Option[U]): Option[T] = ctxOpt map visit
}
