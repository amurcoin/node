package com.amurcoin.lang

import com.amurcoin.lang.directives.Directive

trait ExprCompiler extends Versioned {
  def compile(input: String, directives: List[Directive]): Either[String, version.ExprT]
}
