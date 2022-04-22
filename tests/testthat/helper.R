# Create a minimal rmd to knit using the hugostyle output
# format. To allow tests to inspect the output rmd
# example
# test_that("knitted rmd test", {
#   src = with_rmd()
#   out = rmarkdown::render(src)
#   # read knitted file
#   lines = readLines(out)
#   # expectations
# })
with_rmd = function(envir = parent.frame()) {
  # create a temp dir containing a minimal rmd
  tmp = tempdir()
  rmddir = file.path(tmp, "rmd")
  dir.create(rmddir)
  wd = setwd(rmddir)
  withr::defer(setwd(wd), envir = envir)
  withr::defer(unlink(rmddir, recursive = TRUE, force = TRUE), envir = envir)
  # write minimal Rmd to file
  target = file.path(rmddir, "test.Rmd")
  writeLines(c(
    "---",
    "output:",
    "  hugostyle::hugo_md",
    "---",
    "",
    "{{< rstudio-pro-advert >}}"),
    target
  )
  target
}
