# zzz.R
# ::rtemisbio::
# 2024- EDG rtemis.org

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
} # /rtemisbio::.onAttach
