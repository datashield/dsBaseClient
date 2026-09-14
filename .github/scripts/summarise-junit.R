# Shared by opal-report/armadillo-report's "Compute results & write summary"
# steps (dsBaseClient_test_suite.yaml) - parses one backend's merged JUnit
# XML into a pass/fail tally and, if any failures/errors, a testthat-style
# failure block. The two report jobs are otherwise near-identical, so this
# is the one piece that was previously duplicated between them.
#
# compute_coverage_summary() is used by armadillo-report's separate
# "Compute coverage" step (Opal has no coverage figure - see that step).

summarise_junit <- function(xml_path, label) {
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
      m <- xml2::xml_attr(node, "message")
      if (is.na(m) || !nzchar(m)) m <- trimws(xml2::xml_text(node))
      m
    }, character(1))
    labels <- paste0(xml2::xml_attr(failed, "classname"), "::", xml2::xml_attr(failed, "name"))
    fail_block <- unlist(lapply(seq_along(failed), function(i) {
      c(sprintf("-- Failure (%s) %s", labels[i], strrep("-", max(1, 60 - nchar(labels[i])))), msgs[i], "")
    }))
  }

  list(
    ok = (n_failures + n_errors) == 0,
    tally = tally,
    summary = c(sprintf("## %s unit tests", label), "", "```", fail_block, tally, "```")
  )
}

find_dsbase_version <- function(artifact_dir) {
  files <- list.files(artifact_dir, pattern = "dsbase_version\\.txt$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return("unknown")
  trimws(readLines(files[1], warn = FALSE)[1])
}

compute_coverage_summary <- function(rds_files, threshold) {
  if (length(rds_files) == 0) {
    return(list(icon = "❓", text = "no coverage data found"))
  }
  tallies <- lapply(rds_files, function(f) covr::tally_coverage(readRDS(f)))
  agg <- aggregate(value ~ filename + line, data = do.call(rbind, tallies), FUN = sum)
  total_coverage <- round(sum(agg$value > 0) / nrow(agg) * 100, 1)
  ok <- total_coverage >= threshold
  list(
    icon = if (ok) "✅" else "❌",
    text = sprintf("%.1f%% vs %d%% target", total_coverage, threshold)
  )
}
