package com.amurcoin.settings

import java.io.File

import com.amurcoin.state.ByteStr

case class WalletSettings(file: Option[File], password: Option[String], seed: Option[ByteStr])
