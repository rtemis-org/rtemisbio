# zzz.R
# ::rtemisbio::
# 2024 EDG rtemis.org

# Get internal rtemis functions
msg2 <- getFromNamespace("msg2", "rtemis")
msg20 <- getFromNamespace("msg20", "rtemis")
highlight <- getFromNamespace("highlight", "rtemis")
bold <- getFromNamespace("bold", "rtemis")
italic <- getFromNamespace("italic", "rtemis")
orange <- getFromNamespace("orange", "rtemis")
green <- getFromNamespace("green", "rtemis")
gray <- getFromNamespace("gray", "rtemis")
check_inherits <- getFromNamespace("check_inherits", "rtemis")
check_dependencies <- getFromNamespace("check_dependencies", "rtemis")
clean_int <- getFromNamespace("clean_int", "rtemis")

rtemisbio_version <- utils::packageVersion("rtemisbio")

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0(
      ".:",
      pkgname,
      " ",
      rtemisbio_version,
      " \U1F9EC",
      " ",
      utils::sessionInfo()[[2]]
    )
  )
} # rtemisbio::.onAttach

#' \pkg{rtemisbio}: Bioinformatics ops
#'
#' @description
#' Bioinformatics utilities
#' @name rtemisbio-package
#' @import rtemis data.table S7
"_PACKAGE"

NULL
