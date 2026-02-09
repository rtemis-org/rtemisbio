# zzz.R
# ::rtemis.bio::
# 2024- EDG rtemis.org

rtemis.bio_version <- utils::packageVersion("rtemis.bio")

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0(
      ".:",
      pkgname,
      " ",
      rtemis.bio_version,
      " \U1F9EC",
      " ",
      utils::sessionInfo()[[2]]
    )
  )
} # /rtemis.bio::.onAttach
