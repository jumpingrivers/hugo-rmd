# this test was introduce as a result of a bug that appears in esaping
# chars in knitted output resulting from different versions of pandoc
# original bug was found as a difference between R CLI and RStudio using
# different versions of pandoc.
# Pandoc versions:
#  * 2.5 pass
#  * 2.11.4 fail
test_that("knit rmd doesn't escape {{< (system pandoc)", {
  # set the environment to use the default system wide pandoc install
  src = with_rmd()
  oldenv = Sys.getenv("RSTUDIO_PANDOC")
  Sys.unsetenv("RSTUDIO_PANDOC")
  withr::defer(Sys.setenv("RSTUDIO_PANDOC" = oldenv))
  pv = rmarkdown::find_pandoc(cache = FALSE)

  out = rmarkdown::render(src)
  # read knitted file
  lines = readLines(out)
  escaped = stringr::str_detect(lines, "\\&lt")
  expect_equal(sum(escaped), 0)
})

test_that("knit rmd doesn't escape {{< (rstudio pandoc)", {
  # only run test if the rstudio pandoc location is available
  rstudio_pandoc_dir = "/usr/lib/rstudio/bin/pandoc"
  skip_if_not(dir.exists(rstudio_pandoc_dir))

  # set environment to render rmd using the rstudio bundled pandoc
  src = with_rmd()
  oldenv = Sys.getenv("RSTUDIO_PANDOC")
  Sys.setenv("RSTUDIO_PANDOC" = rstudio_pandoc_dir)
  withr::defer(Sys.setenv("RSTUDIO_PANDOC" = oldenv))
  pv = rmarkdown::find_pandoc(cache = FALSE)

  out = rmarkdown::render(src)
  # read knitted file
  lines = readLines(out)
  escaped = stringr::str_detect(lines, "\\&lt")
  expect_equal(sum(escaped), 0)
})
