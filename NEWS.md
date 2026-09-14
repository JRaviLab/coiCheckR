# coiCheckR 0.99.0

* First version gearing towards Bioconductor submission.
* Screens candidate peer reviewers against a manuscript's author list for
  direct co-authorship (PubMed), time-windowed co-authorship, second-degree
  co-authorship, and shared NIH funding (NIH RePORTER), plus preprint
  enrichment via the bioRxiv API.
* Returns tidy, auditable evidence tables rather than a conflicted/clear
  verdict.
* `checkCoi()`/`checkCoiBatch()` now return an explicit `status` --
  `"potential_conflict"`, `"no_conflict_detected"`, or
  `"insufficient_evidence"` -- instead of only evidence tables. A PubMed
  or NIH RePORTER request that errors is surfaced via `sources_failed`
  and forces `status` to `"insufficient_evidence"` rather than being
  silently treated as "no conflict".
