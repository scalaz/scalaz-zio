package zio.test

import zio.Chunk
import zio.test.Assertion._
import zio.test.internal.myers.{DiffResult, MyersDiff}

object MyersDiffSpec extends ZIOBaseSpec {

  def spec: ZSpec[Environment, Failure] = suite("MyersDiffSpec")(
    test("diffing works for only additions both ways") {
      val original = ""
      val modified = "ADDITIONS"

      assert(MyersDiff.diff(original, modified).applyChanges(original))(equalTo(modified)) &&
      assert(MyersDiff.diff(original, modified).invert.applyChanges(modified))(equalTo(original))
    },
    test("diffing works for only deletions both ways") {
      val original = "DELETIONS"
      val modified = ""

      assert(MyersDiff.diff(original, modified).applyChanges(original))(equalTo(modified)) &&
      assert(MyersDiff.diff(original, modified).invert.applyChanges(modified))(equalTo(original))
    },
    test("diffing works for two empty strings both ways") {
      val original = ""
      val modified = ""

      assert(MyersDiff.diff(original, modified).applyChanges(original))(equalTo(modified)) &&
      assert(MyersDiff.diff(original, modified).invert.applyChanges(modified))(equalTo(original))
    },
    test("diffing works for Myers example both ways") {
      val original = "ABCABBA"
      val modified = "CBABAC"

      assert(MyersDiff.diff(original, modified).applyChanges(original))(equalTo(modified)) &&
      assert(MyersDiff.diff(original, modified).invert.applyChanges(modified))(equalTo(original))
    },
    test("diffing for Myers example produces a sane DiffResult") {
      val original = "ABCABBA"
      val modified = "CBABAC"

      import zio.test.internal.myers.Action._
      assert(MyersDiff.diff(original, modified))(
        equalTo(
          DiffResult(
            Chunk(
              Insert("C"),
              Delete("A"),
              Keep("B"),
              Delete("C"),
              Keep("A"),
              Keep("B"),
              Delete("B"),
              Keep("A"),
              Insert("C")
            )
          )
        )
      )
    },
    testM("diffing works for all random strings both ways") {
      check(Gen.anyString, Gen.anyString) { (original, modified) =>
        assert(MyersDiff.diff(original, modified).applyChanges(original))(equalTo(modified)) &&
        assert(MyersDiff.diff(original, modified).invert.applyChanges(modified))(equalTo(original))
      }
    }
  )
}
