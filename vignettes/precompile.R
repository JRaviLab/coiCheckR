# Regenerate vignettes/coiCheckR.Rmd from vignettes/coiCheckR.Rmd.orig.
#
# Run this by hand, from the package root, whenever the example output
# needs refreshing (e.g. after an API change, or periodically to pick
# up newer publications/awards):
#
#   Rscript vignettes/precompile.R
#
# Requires live network access to PubMed and NIH RePORTER. Not run as
# part of package build/check/CI -- the committed vignettes/coiCheckR.Rmd
# is the already-rendered artifact that ships with the package and is
# what CRAN/Bioconductor actually build (no network dependency at check
# time). Commit the regenerated coiCheckR.Rmd after running this.

knitr::knit(
  input = "vignettes/coiCheckR.Rmd.orig",
  output = "vignettes/coiCheckR.Rmd"
)
