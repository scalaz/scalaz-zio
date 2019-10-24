/*
 * Copyright 2017-2019 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.stm

import zio.test.Assertion._
import zio.test._
import zio.ZIOBaseSpec

object TSetSpec
    extends ZIOBaseSpec(
      suite("TSet")(
        suite("factories")(
          testM("apply") {
            val tx = TSet(1, 2, 2, 3).flatMap(_.toList)
            assertM(tx.commit, hasSameElements(List(1, 2, 3)))

          },
          testM("empty") {
            val tx = TSet.empty[Int].flatMap(_.toList)
            assertM(tx.commit, isEmpty)
          },
          testM("fromIterable") {
            val tx = TSet.fromIterable(List(1, 2, 2, 3)).flatMap(_.toList)
            assertM(tx.commit, hasSameElements(List(1, 2, 3)))
          }
        ),
        suite("lookups")(
          testM("contains existing element") {
            val tx = TSet(1, 2, 3, 4).flatMap(_.contains(1))
            assertM(tx.commit, isTrue)
          },
          testM("contains non-existing element") {
            val tx = TSet(1, 2, 3, 4).flatMap(_.contains(0))
            assertM(tx.commit, isFalse)
          },
          testM("collect all elements") {
            val tx = TSet(1, 2, 3, 4).flatMap(_.toList)
            assertM(tx.commit, hasSameElements(List(1, 2, 3, 4)))
          }
        ),
        suite("insertion and removal")(
          testM("add new element") {
            val tx =
              for {
                tset <- TSet.empty[Int]
                _    <- tset.put(1)
                res  <- tset.toList
              } yield res

            assertM(tx.commit, hasSameElements(List(1)))
          },
          testM("add duplicate element") {
            val tx =
              for {
                tset <- TSet(1)
                _    <- tset.put(1)
                res  <- tset.toList
              } yield res

            assertM(tx.commit, hasSameElements(List(1)))
          },
          testM("remove existing element") {
            val tx =
              for {
                tset <- TSet(1, 2)
                _    <- tset.delete(1)
                res  <- tset.toList
              } yield res

            assertM(tx.commit, hasSameElements(List(2)))
          },
          testM("remove non-existing element") {
            val tx =
              for {
                tset <- TSet(1, 2)
                _    <- tset.delete(3)
                res  <- tset.toList
              } yield res

            assertM(tx.commit, hasSameElements(List(1, 2)))
          }
        ),
        suite("transformations")(
          testM("retainIf") {
            val tx =
              for {
                tset <- TSet("a", "aa", "aaa")
                _    <- tset.retainIf(_ == "aa")
                a    <- tset.contains("a")
                aa   <- tset.contains("aa")
                aaa  <- tset.contains("aaa")
              } yield (a, aa, aaa)

            assertM(tx.commit, equalTo((false, true, false)))
          },
          testM("removeIf") {
            val tx =
              for {
                tset <- TSet("a", "aa", "aaa")
                _    <- tset.removeIf(_ == "aa")
                a    <- tset.contains("a")
                aa   <- tset.contains("aa")
                aaa  <- tset.contains("aaa")
              } yield (a, aa, aaa)

            assertM(tx.commit, equalTo((true, false, true)))
          }
        )
      )
    )
