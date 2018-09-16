package com.amurcoin.lang

import com.amurcoin.lang.v1.BaseGlobal

package object hacks {
  private[lang] val Global: BaseGlobal = com.amurcoin.lang.Global // Hack for IDEA
}
