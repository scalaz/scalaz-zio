package zio.test

import zio._
import zio.test.Assertion._

import java.util.concurrent.atomic.AtomicInteger
import ShareLayersAcrossSpecs._

object ShareLayersAcrossSpecs {

  private val counter = new AtomicInteger(0)

  final case class BoxedInt(i: Int) {
    override def toString =
      s"BoxedInt($i)@${System.identityHashCode(this).toHexString}"
  }

  val sharedLayer: ULayer[Has[BoxedInt]] =
    UIO(BoxedInt(counter.getAndIncrement()))
      // .tap(boxedInt => UIO(println(s"created $boxedInt")))
      .toLayer

  val checkCounter =
    for {
      c <- UIO(counter.get())
      assert <- assertM(
                  ZIO.service[BoxedInt].map(_.i)
                )(equalTo(c - 1))
    } yield assert
}

object ShareLayersAcrossSpecsSpec1 extends CustomRunnableSpec(sharedLayer) {
  override def spec =
    suite("Shared layer across specs - 1")(
      testM("The same BoxedInt instance should be shared across all Specs")(
        checkCounter
      )
    )
}

object ShareLayersAcrossSpecsSpec2 extends CustomRunnableSpec(sharedLayer) {
  override def spec =
    suite("Shared layer across specs - 2")(
      testM("The same BoxedInt instance should be shared across all Specs")(
        checkCounter
      )
    )
}
