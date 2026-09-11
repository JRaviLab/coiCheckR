# Internal helpers for distinguishing "a source returned zero rows" from
# "a source failed and we don't actually know" -- an API timeout or HTTP
# error must never be silently indistinguishable from a genuine null
# result, since that's how an intermittent outage turns into a false
# "no conflict".

.markSourceFailed <- function(x) {
  attr(x, "coiCheckR_failed") <- TRUE
  x
}

.sourceFailed <- function(x) {
  isTRUE(attr(x, "coiCheckR_failed"))
}

# The four possible outcomes of a COI screen. Deliberately never a bare
# TRUE/FALSE: "no_conflict_detected" is not the same claim as "no conflict
# exists", and a source failure must surface as "insufficient_evidence"
# rather than silently collapsing into "no_conflict_detected".
.coiStatusLevels <- function() {
  c("potential_conflict", "no_conflict_detected", "insufficient_evidence")
}

.classifyCoiStatus <- function(direct, second_degree, funding, sources_failed) {
  has_evidence <- nrow(direct) > 0 || nrow(second_degree) > 0 || nrow(funding) > 0
  if (has_evidence) {
    return("potential_conflict")
  }
  if (length(sources_failed) > 0) {
    return("insufficient_evidence")
  }
  "no_conflict_detected"
}
