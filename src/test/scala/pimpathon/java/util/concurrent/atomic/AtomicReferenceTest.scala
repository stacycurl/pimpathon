package pimpathon.java.util.concurrent.atomic

import java.util.concurrent.atomic.AtomicReference
import pimpathon.PSpec
import pimpathon.any._
import pimpathon.java.util.concurrent.atomic.atomicReference._

class AtomicReferenceSpec extends PSpec {
  "update" in 
    (new AtomicReference(123) |> (ref => (ref.update(_ * 2), ref.get()))) ≡ (246, 246)

  "updateEither" in {
    (new AtomicReference(123) |> (ref => (ref.updateEither(i => Left(i * 2)),  ref.get()))) ≡ (Left(246),  123)
    (new AtomicReference(123) |> (ref => (ref.updateEither(i => Right(i * 2)), ref.get()))) ≡ (Right(246), 246)
  }
}
