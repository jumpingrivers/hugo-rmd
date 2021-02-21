#' Set knitr hooks
#'
#' Set standard {knitr} hooks
#' @param optipng Use optipng by default
#' @param ... Other arguments passed to knit_hooks$set
#' @export
set_knitr_hooks = function(optipng = knitr::hook_optipng,
                           ...) {
  knitr::knit_hooks$set(optipng = optipng, ...)
}


#' Set default chunk options
#'
#' Set of standard {knitr} chunk options
#'
#' @param echo TRUE
#' @param eval  TRUE
#' @param collapse  TRUE
#' @param cache  TRUE
#' @param comment  "#>"
#' @param fig.path  "."
#' @param fig.align  "center"
#' @param fig.retina  2
#' @param fig.asp  0.7
#' @param fig.width 500 / 72
#' @param dev.args list(png = list(type = "cairo-png"))
#' @param dev "svg"
#' @param optipng "-o1 -quiet"
#' @param ... Other arguments passed to opts_chunk$set
#' @export
set_knitr_chunk_options = function(echo = TRUE,
                                   eval = TRUE,
                                   collapse = TRUE,
                                   cache = TRUE,
                                   comment = "#>",
                                   fig.path = ".",
                                   fig.align = "center",
                                   fig.retina = 2,
                                   fig.asp = 0.7,
                                   fig.width = 500 / 72,
                                   dev.args = list(png = list(type = "cairo-png")),
                                   dev = "svg",
                                   optipng = "-o1 -quiet",
                                   ...) {

  knitr::opts_chunk$set(echo = echo,
                        eval = TRUE,
                        collapse = collapse,
                        cache = cache,
                        comment = comment,
                        fig.path = fig.path,
                        fig.align = fig.align,
                        fig.retina = fig.retina,
                        fig.asp = fig.asp,
                        fig.width = fig.width,
                        dev.args = dev.args,
                        dev = dev,
                        optipng = optipng,
                        ...)
}

#' MD file for hugo websites
#'
#' This format generates a standard markdown file.
#' This function is simply rmarkdown::md_document with an extra
#' line to insert `{{< url >}}`.
#'
#' See `?rmarkdown::md_document` for help on parameters. Note, that
#' `preserve_yaml` has been set to `TRUE` we need to keep the yaml in the md
#' document for hugo.
#'
#' @param variant,preserve_yaml,toc,toc_depth,number_sections See `?rmarkdown::md_document`
#' @param fig_width,fig_height,fig_retina,dev,df_print,includes See `?rmarkdown::md_document`
#' @param includes,md_extensions,pandoc_args,ext See `?rmarkdown::md_document`
#'
#'
#' @export
hugo_md = function(variant = "markdown_strict", preserve_yaml = TRUE,
                   toc = FALSE, toc_depth = 3, number_sections = FALSE, fig_width = 7,
                   fig_height = 5, fig_retina = 2, dev = "png", df_print = "default",
                   includes = NULL, md_extensions = NULL, pandoc_args = NULL,
                   ext = ".md") {


  args = c(if (variant != "markdown" || preserve_yaml) "--standalone")
  args = c(args, rmarkdown::pandoc_toc_args(toc, toc_depth))
  args = c(args, rmarkdown::includes_to_pandoc_args(includes))
  args = c(args, pandoc_args)
  post_processor = if (preserve_yaml && variant != "markdown") {
    function(metadata, input_file, output_file, clean, verbose) {
      input_lines = rmarkdown:::read_utf8(input_file)
      partitioned = rmarkdown:::partition_yaml_front_matter(input_lines)
      if (!is.null(partitioned$front_matter)) {
        output_lines = c(partitioned$front_matter, "", rmarkdown:::read_utf8(output_file))
        output_lines = stringr::str_replace(string = output_lines,
                                            pattern = '<img.*src="(^")*',
                                            replacement = '<img src="{{< url >}}\\1')
        rmarkdown:::write_utf8(output_lines, output_file)
      }
      output_file
    }
  }
  rmarkdown::output_format(knitr = rmarkdown::knitr_options_html(fig_width, fig_height,
                                                                 fig_retina, FALSE, dev),
                           pandoc = rmarkdown::pandoc_options(to = variant,
                                                              from = rmarkdown::from_rmarkdown(extensions = md_extensions),
                                                              args = args,
                                                              ext = ext,
                                                              lua_filters = if (number_sections)
                                                                rmarkdown::pkg_file_lua("number-sections.lua")),
                           clean_supporting = FALSE,
                           df_print = df_print, post_processor = post_processor)

}
