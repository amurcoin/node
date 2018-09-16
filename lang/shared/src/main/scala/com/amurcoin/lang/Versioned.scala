package com.amurcoin.lang

trait Versioned {
  type V <: ScriptVersion
  val version: V
}
