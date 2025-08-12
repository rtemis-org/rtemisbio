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

rtemisbio.version <- utils::packageVersion("rtemisbio")

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0(
      "  .:",
      pkgname,
      " ",
      rtemisbio.version,
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
#' @import rtemis data.table
"_PACKAGE"

NULL
