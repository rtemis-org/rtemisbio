# rtemis.bio-package.R
# ::rtemis.bio::
# 2026 EDG rtemis.org

pkglogo <- function(
  pkg,
  filename = paste0(pkg, ".utf8"),
  fmt_fn = color_txt_columns,
  args = list(
    color_left = rtemis_colors[["blue"]],
    color_right = rtemis_colors[["green"]],
    output_type = "ansi"
  ),
  pad = 0L
) {
  logo_file <- system.file(
    package = .packageName,
    "resources",
    filename
  )
  logo_txt <- readLines(logo_file)
  paste0(
    strrep(" ", pad),
    do.call(fmt_fn, c(list(x = logo_txt), args)),
    collapse = "\n"
  )
} # /rtemis::pkglogo

#' @name rtemis.bio-package
#'
#' @title rtemis.bio: Bioinformatics ops
#'
#' @description
#' Bioinformatics utilities
#'
#' @import rtemis.utils data.table S7
#' @importFrom rtemis draw_protein draw_xt
"_PACKAGE"

NULL
