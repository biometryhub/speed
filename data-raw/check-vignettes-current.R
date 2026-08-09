# Fail if a pre-computed vignette is stale.
#
# The shipped `vignettes/*.qmd` are generated from `vignettes/*.qmd.orig` by
# `data-raw/precompute-vignettes.R`. Nothing forces that script to be re-run, so
# an edited `.orig` can silently ship alongside a stale `.qmd`. This compares
# each source against the hashes recorded when the `.qmd` was last generated.
#
#     Rscript data-raw/check-vignettes-current.R

vig_dir <- "vignettes"
manifest <- file.path("data-raw", "vignette-hashes.txt")

if (!file.exists(manifest)) {
  stop(
    "No ",
    manifest,
    ". Run: Rscript data-raw/precompute-vignettes.R",
    call. = FALSE
  )
}

recorded <- readLines(manifest)
recorded <- recorded[nzchar(recorded)]
parts <- strsplit(recorded, "  ", fixed = TRUE)
want <- vapply(parts, `[`, character(1), 1)
names(want) <- vapply(parts, `[`, character(1), 2)

orig <- list.files(vig_dir, pattern = "\\.qmd\\.orig$")
problems <- character(0)

for (f in orig) {
  qmd <- file.path(vig_dir, sub("\\.orig$", "", f))
  if (!file.exists(qmd)) {
    problems <- c(problems, sprintf("%s has no generated %s", f, basename(qmd)))
    next
  }
  if (!f %in% names(want)) {
    problems <- c(problems, sprintf("%s is not in the manifest", f))
    next
  }
  # Strip CR before hashing, matching how `precompute-vignettes.R` wrote the
  # manifest - otherwise a CRLF working tree never matches an LF checkout.
  src <- file.path(vig_dir, f)
  bytes <- readBin(src, "raw", file.size(src))
  tmp <- tempfile()
  writeBin(bytes[bytes != as.raw(13L)], tmp)
  got <- unname(tools::md5sum(tmp)[[1]])
  unlink(tmp)

  if (!identical(unname(got), unname(want[[f]]))) {
    problems <- c(
      problems,
      sprintf("%s changed since %s was generated", f, basename(qmd))
    )
  }
}

missing <- setdiff(names(want), orig)
if (length(missing)) {
  problems <- c(problems, sprintf("%s is in the manifest but gone", missing))
}

if (length(problems)) {
  stop(
    "Pre-computed vignettes are out of date:\n",
    paste0("  - ", problems, collapse = "\n"),
    "\n\nRe-run: Rscript data-raw/precompute-vignettes.R",
    call. = FALSE
  )
}

cat("All", length(orig), "pre-computed vignettes are up to date.\n")
