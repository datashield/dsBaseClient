# Shared by opal-report/armadillo-report's "Compute results & write summary"
# steps (dsBaseClient_test_suite.yaml) - parses one backend's merged JUnit
# XML into a pass/fail tally and, if any failures/errors, a testthat-style
# failure block. The two report jobs are otherwise near-identical, so this
# is the one piece that was previously duplicated between them.

# Each shard/dsdanger matrix entry uploads its own shard_status.txt (written
# from the job.status context, after the test step) alongside its test
# results, regardless of pass/fail. A shard that crashed before producing any
# test XML (e.g. setup failure) leaves 0 tests in that shard's own XML but
# still uploads a "failure" status file - that combination, not the XML
# content alone, is what tells us the shard never actually reported rather
# than reporting a clean pass.
find_incomplete_shards <- function(artifact_dir) {
  status_files <- list.files(artifact_dir, pattern = "^shard_status\\.txt$", recursive = TRUE, full.names = TRUE)
  problems <- character(0)
  for (f in status_files) {
    status <- trimws(readLines(f, warn = FALSE)[1])
    if (status == "success") next

    shard_dir <- dirname(f)
    xml_files <- list.files(shard_dir, pattern = "^test_results_.*\\.xml$", full.names = TRUE)
    n_tests <- if (length(xml_files) == 0) 0 else {
      doc <- xml2::read_xml(xml_files[1])
      sum(as.integer(xml2::xml_attr(xml2::xml_find_all(doc, ".//testsuite"), "tests")), na.rm = TRUE)
    }
    if (n_tests == 0) {
      problems <- c(problems, sprintf("- **%s** did not report any test results (job status: %s)", basename(shard_dir), status))
    }
  }
  problems
}

summarise_junit <- function(xml_path, label, artifact_dir = NULL) {
  doc <- xml2::read_xml(xml_path)
  suites <- xml2::xml_find_all(doc, ".//testsuite")
  n_tests    <- sum(as.integer(xml2::xml_attr(suites, "tests")), na.rm = TRUE)
  n_failures <- sum(as.integer(xml2::xml_attr(suites, "failures")), na.rm = TRUE)
  n_errors   <- sum(as.integer(xml2::xml_attr(suites, "errors")), na.rm = TRUE)
  n_skipped  <- sum(as.integer(xml2::xml_attr(suites, "skipped")), na.rm = TRUE)
  n_pass     <- n_tests - n_failures - n_errors - n_skipped
  tally <- sprintf("[ FAIL %d | WARN 0 | SKIP %d | PASS %d ]", n_failures + n_errors, n_skipped, n_pass)

  failed <- xml2::xml_find_all(doc, ".//testcase[failure or error]")
  fail_block <- character(0)
  if (length(failed) > 0) {
    msgs <- vapply(failed, function(tc) {
      node <- xml2::xml_find_first(tc, "failure|error")
      first <- xml2::xml_attr(node, "message")
      if (is.na(first)) first <- ""
      rest <- strsplit(trimws(xml2::xml_text(node)), "\n")[[1]][-1]
      paste(c(first, rest), collapse = "\n")
    }, character(1))
    labels <- paste0(xml2::xml_attr(failed, "classname"), "::", xml2::xml_attr(failed, "name"))
    fail_block <- unlist(lapply(seq_along(failed), function(i) {
      c(sprintf("-- Failure (%s) %s", labels[i], strrep("-", max(1, 60 - nchar(labels[i])))), msgs[i], "")
    }))
  }

  shard_problems <- if (is.null(artifact_dir)) character(0) else find_incomplete_shards(artifact_dir)

  list(
    ok = (n_failures + n_errors) == 0 && length(shard_problems) == 0,
    tally = tally,
    summary = c(
      sprintf("## %s unit tests", label), "",
      shard_problems, if (length(shard_problems) > 0) "",
      "```", fail_block, tally, "```"
    )
  )
}

find_dsbase_version <- function(artifact_dir) {
  files <- list.files(artifact_dir, pattern = "dsbase_version\\.txt$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return("unknown")
  trimws(readLines(files[1], warn = FALSE)[1])
}
