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
