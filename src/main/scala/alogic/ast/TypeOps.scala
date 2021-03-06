////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

trait TypeOps extends TypePrettyPrintOps { this: Type =>
  def widthExpr: Expr = this match {
    case IntType(_, size)       => Expr(size)
    case IntVType(_, sizeExprs) => sizeExprs reduce (_ * _)
    case Struct(_, fields)      => fields.values map (_.widthExpr) reduce (_ + _)
  }
}
