# Fail if a pre-computed vignette is stale.
#
# Nothing forces `tools/precompute-vignettes.R` to be re-run, so an edited
# `.qmd.orig` can silently ship alongside a stale `.qmd`. Compare each source
# against the hash recorded when the `.qmd` was last generated.
#
#     Rscript tools/check-vignettes-current.R

vig_dir <- "vignettes"
manifest <- file.path("tools", "vignette-hashes.txt")

if (!file.exists(manifest)) {
  stop(
    "No ",
    manifest,
    ". Run: Rscript tools/precompute-vignettes.R",
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

  # A fresh hash says nothing about whether the render is well formed. knitr
  # emits unfenced source if its markdown hooks were never installed, which
  # leaves every chunk as prose, so require the chunks to come back as code.
  qmd_lines <- readLines(qmd, warn = FALSE)
  has_chunks <- any(grepl("^```\\{", readLines(src, warn = FALSE)))
  if (has_chunks && !any(grepl("^```", qmd_lines))) {
    problems <- c(
      problems,
      sprintf(
        "%s has chunks but %s has no fenced code blocks",
        f,
        basename(qmd)
      )
    )
  }

  # The manifest only hashes the source, so content written straight into the
  # generated file is invisible to it. An executable chunk there would run
  # during `R CMD build`, which is what pre-computing exists to avoid.
  live <- grep("^```\\{", qmd_lines)
  if (length(live)) {
    problems <- c(
      problems,
      sprintf(
        "%s has %d unexecuted chunk(s) (line %s) - edit %s instead",
        basename(qmd),
        length(live),
        paste(live, collapse = ", "),
        f
      )
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
    "\n\nRe-run: Rscript tools/precompute-vignettes.R",
    call. = FALSE
  )
}

cat("All", length(orig), "pre-computed vignettes are up to date.\n")
