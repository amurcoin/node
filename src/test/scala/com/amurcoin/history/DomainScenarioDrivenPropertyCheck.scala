package com.amurcoin.history

import com.amurcoin.db.WithState
import com.amurcoin.settings.WavesSettings
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait DomainScenarioDrivenPropertyCheck extends WithState { _: GeneratorDrivenPropertyChecks =>
  def scenario[S](gen: Gen[S], bs: WavesSettings = DefaultWavesSettings)(assertion: (Domain, S) => Assertion): Assertion =
    forAll(gen) { s =>
      withDomain(bs) { domain =>
        assertion(domain, s)
      }
    }
}
