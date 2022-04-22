# definitions for the unexported rmarkdown functions used within the package
# definitions match rmarkdown (2.13)

# see rmarkdown:::partition_yaml_front_matter
partition_yaml_front_matter = function(input_lines) {
  validate_front_matter <- function(delimiters) {
    if (
      length(delimiters) >= 2 &&
      (delimiters[2] - delimiters[1] > 1) &&
      grepl("^---\\s*$", input_lines[delimiters[1]])
    ) {
      if (delimiters[1] == 1) {
        TRUE
      }
      else {
        all(grepl(
          "^\\s*(<!-- rnb-\\w*-(begin|end) -->)?\\s*$",
          input_lines[1:delimiters[1] - 1]
        ))
      }
    }
    else {
      FALSE
    }
  }
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {
    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]
    input_body <- c()
    if (delimiters[1] > 1) {
      input_body <- c(
        input_body,
        input_lines[1:delimiters[1] - 1]
      )
    }

    if (delimiters[2] < length(input_lines)) {
      input_body <- c(input_body, input_lines[-(1:delimiters[2])])
    }
    list(front_matter = front_matter, body = input_body)
  }
  else {
    list(front_matter = NULL, body = input_lines)
  }
}

# see rmarkdown:::read_utf8
read_utf8 = function(file) {
  if (inherits(file, "connection")) {
    con <- file
  }
  else {
    con <- base::file(file, encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
  }
  enc2utf8(readLines(con, warn = FALSE))
}

# see rmarkdown:::write_utf8
write_utf8 = function(text, con, ...) {
  opts <- options(encoding = "native.enc")
  on.exit(options(opts), add = TRUE)
  writeLines(enc2utf8(text), con, ..., useBytes = TRUE)
}
