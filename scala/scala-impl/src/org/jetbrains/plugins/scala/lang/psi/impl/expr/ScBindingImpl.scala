package org.jetbrains.plugins.scala
package lang
package psi
package impl
package expr

import com.intellij.lang.ASTNode
import org.jetbrains.plugins.scala.lang.psi.api.expr._

/** 
* @author Alexander Podkhalyuzin
* Date: 06.03.2008
*/

class ScBindingImpl(node: ASTNode) extends ScalaPsiElementImpl(node) with ScBinding {
  override def toString: String = "Parameter"
}