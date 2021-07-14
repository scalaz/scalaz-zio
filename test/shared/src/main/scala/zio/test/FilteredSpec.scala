/*
 * Copyright 2019-2021 John A. De Goes and the ZIO Contributors
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

package zio.test

/**
 * Filters a given `ZSpec` based on the command-line arguments.
 * If no arguments were specified, the spec returns unchanged.
 */
private[zio] object FilteredSpec {
  def apply[R, E](spec: ZSpec[R, E], args: TestArgs): ZSpec[R, E] = {
    val tagSearchTerms = if(args.fixSnapshots) args.tagSearchTerms.appended("SNAPSHOT_TEST_FILE") else args.tagSearchTerms
    def filtered: Option[ZSpec[R, E]] =
      (args.testSearchTerms, tagSearchTerms) match {
        case (Nil, Nil) => None
        case (testSearchTerms, Nil) =>
          spec.filterLabels(label => testSearchTerms.exists(term => label.contains(term)))
        case (Nil, tagSearchTerms) =>
          spec.filterTags(tag => tagSearchTerms.contains(tag))
        case (testSearchTerms, tagSearchTerms) =>
          spec
            .filterTags(tag => testSearchTerms.contains(tag))
            .flatMap(_.filterLabels(label => tagSearchTerms.exists(term => label.contains(term))))
      }

    filtered.getOrElse(spec)
  }
}
