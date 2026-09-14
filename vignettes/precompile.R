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

# Knit from *within* vignettes/, not the package root: knitr's fig.path
# is relative to the working directory at knit time, and the final
# render at package-build time (R CMD build / devtools::build_vignettes())
# always happens with vignettes/ as the working directory -- knitting
# from the package root here instead would embed figure paths that
# resolve correctly now but break (or silently land figures at the
# package root, outside vignettes/ entirely) on the next real build.
# withr::with_dir() (not a bare setwd()/on.exit() pair -- on.exit() does
# not fire at script end when called at the top level of a source()d
# file, only immediately) guarantees the directory is restored either way.
withr::with_dir("vignettes", {
  knitr::knit(input = "coiCheckR.Rmd.orig", output = "coiCheckR.Rmd")
})
