package zio

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.infra.results.IIII_Result

object ZHubConcurrencyTests {

  val runtime = Runtime.default
  /*
   * Tests that there are no race conditions between publishing and taking..
   */
  @JCStressTest
  @Outcome.Outcomes(
    Array(
      new Outcome(id = Array("1, 2, 1, 2"), expect = Expect.ACCEPTABLE),
      new Outcome(id = Array("2, 1, 2, 1"), expect = Expect.ACCEPTABLE)
    )
  )
  @State
  class ManyToManyTest {
    val hub: Hub[Int]       = runtime.unsafeRun(Hub.bounded(2))
    val left: Dequeue[Int]  = runtime.unsafeRun(hub.subscribe.reserve.flatMap(_.acquire))
    val right: Dequeue[Int] = runtime.unsafeRun(hub.subscribe.reserve.flatMap(_.acquire))
    var p1                  = 0
    var p2                  = 0
    var p3                  = 0
    var p4                  = 0

    @Actor
    def actor1(): Unit = {
      runtime.unsafeRun(hub.publish(1))
      ()
    }

    @Actor
    def actor2(): Unit = {
      runtime.unsafeRun(hub.publish(2))
      ()
    }

    @Actor
    def actor3(): Unit =
      runtime.unsafeRun {
        left.take.zipWith(left.take) { (first, last) =>
          p1 = first
          p2 = last
        }
      }

    @Actor
    def actor4(): Unit =
      runtime.unsafeRun {
        right.take.zipWith(right.take) { (first, last) =>
          p3 = first
          p4 = last
        }
      }

    @Arbiter
    def arbiter(r: IIII_Result): Unit = {
      r.r1 = p1
      r.r2 = p2
      r.r3 = p3
      r.r4 = p4
    }
  }
}
