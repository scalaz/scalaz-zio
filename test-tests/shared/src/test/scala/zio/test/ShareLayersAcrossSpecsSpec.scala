package zio.test

import zio._
import zio.test.Assertion._
import zio.test.ShareLayersAcrossSpecsSpec._

object ShareLayersAcrossSpecsSpec {

  type IntRef = Has[Ref[Int]]

  val layer: ULayer[IntRef] = (
    Ref
      .make[Int](0)
      .tap(ref => UIO(println(s"new ref: $ref")))
      .toManaged(ref => UIO(println(s"release ref $ref")))
    )
    .toLayer

  val increment =
    assertM(
      ZIO.accessM[IntRef](
        _.get
          .updateAndGet(_ + 1)
          .tap(i => UIO(println(i)))
      )
    )(equalTo(0)) // letting this fail for now...
}

object ShareLayersAcrossSpecsSpec1 extends CustomRunnableSpec(layer) {

  val spec11 =
    suite("spec11")(
      testM("1")(increment),
      testM("2")(increment)
    )

  val spec12 =
    suite("spec12")(
      testM("3")(increment),
      testM("4")(increment)
    )

  override def spec =
    suite("xs")(
      spec11,
      spec12
    )
}

object ShareLayersAcrossSpecsSpec2 extends CustomRunnableSpec(layer) {

  val spec21 =
    suite("spec21")(
      testM("5")(increment),
      testM("6")(increment)
    )

  val spec22 =
    suite("spec22")(
      testM("7")(increment),
      testM("8")(increment)
    )

  override def spec =
    suite("xs")(
      spec21,
      spec22
    )
}
