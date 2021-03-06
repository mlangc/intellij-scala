package org.jetbrains.plugins.scala
package lang
package parser
package parsing
package statements

import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.parsing.builder.ScalaPsiBuilder
import org.jetbrains.plugins.scala.lang.parser.parsing.expressions.{Block, Expr}
import org.jetbrains.plugins.scala.lang.parser.parsing.params.ParamClauses
import org.jetbrains.plugins.scala.lang.parser.parsing.types.Type
import org.jetbrains.plugins.scala.lang.parser.util.ParserPatcher

/**
* @author Alexander Podkhalyuzin
* Date: 13.02.2008
*/

/*
 * FunDef ::= FunSig [':' Type] '=' Expr
 *          | FunSig [nl] '{' Block '}'
 *          | 'this' ParamClause ParamClauses
 *            ('=' ConstrExpr | [nl] ConstrBlock)
 */
object FunDef extends FunDef {
  override protected def constrExpr = ConstrExpr
  override protected def constrBlock = ConstrBlock
  override protected def block = Block
  override protected def funSig = FunSig
  override protected def expr = Expr
  override protected def paramClauses = ParamClauses
  override protected def `type` = Type
}

trait FunDef {
  protected def constrExpr: ConstrExpr
  protected def constrBlock: ConstrBlock
  protected def expr: Expr
  protected def block: Block
  protected def paramClauses: ParamClauses
  protected def funSig: FunSig
  protected def `type`: Type

  def parse(builder: ScalaPsiBuilder): Boolean = {
    val faultMarker = builder.mark
    builder.getTokenType match {
      case ScalaTokenTypes.kDEF => builder.advanceLexer()
      case _ =>
        faultMarker.drop()
        return false
    }
    builder.getTokenType match {
      case ScalaTokenTypes.tIDENTIFIER =>
        funSig parse builder
        builder.getTokenType match {
          case ScalaTokenTypes.tCOLON =>
            builder.advanceLexer() //Ate :
            if (`type`.parse(builder)) {
              builder.getTokenType match {
                case ScalaTokenTypes.tASSIGN =>
                  builder.advanceLexer() //Ate =
                  if (expr.parse(builder)) {
                    faultMarker.drop()
                    true
                  }
                  else {
                    builder error ScalaBundle.message("wrong.expression")
                    faultMarker.drop()
                    true
                  }
                case _ =>
                  faultMarker.rollbackTo()
                  false
              }
            }
            else {
              faultMarker.rollbackTo()
              false
            }
          case ScalaTokenTypes.tASSIGN =>
            builder.advanceLexer() //Ate =
            ParserPatcher getSuitablePatcher builder parse builder
            if (expr parse builder) {
              faultMarker.drop()
              true
            }
            else {
              builder error ScalaBundle.message("wrong.expression")
              faultMarker.drop()
              true
            }
          case ScalaTokenTypes.tLBRACE =>
            if (builder.twoNewlinesBeforeCurrentToken) {
              faultMarker.rollbackTo()
              return false
            }
            block.parse(builder, hasBrace = true)
            faultMarker.drop()
            true
          case _ =>
            faultMarker.rollbackTo()
            false
        }
      case ScalaTokenTypes.kTHIS =>
        builder.advanceLexer() //Ate this
        paramClauses parse(builder, true)
        builder.getTokenType match {
          case ScalaTokenTypes.tASSIGN =>
            builder.advanceLexer() //Ate =
            if (!constrExpr.parse(builder)) {
              builder error ScalaBundle.message("wrong.constr.expression")
            }
            faultMarker.drop()
            true
          case _ =>
            if (builder.twoNewlinesBeforeCurrentToken) {
              builder error ScalaBundle.message("constr.block.expected")
              faultMarker.drop()
              return true
            }
            if (!constrBlock.parse(builder)) {
              builder error ScalaBundle.message("constr.block.expected")
            }
            faultMarker.drop()
            true
        }
      case _ =>
        faultMarker.rollbackTo()
        false
    }
  }
}