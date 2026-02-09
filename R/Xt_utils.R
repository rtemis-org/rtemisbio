# Xtjson.R
# ::rtemis.bio::
# 2024- EDG rtemis.org

#' Write `Xt` object to JSON file
#'
#' @param x `Xt` object, as created by [as_Xt].
#' @param filepath Character: Path to save JSON file.
#' @param overwrite Logical: If TRUE, overwrite existing file.
#'
#' @return Nothing. Writes JSON file.
#'
#' @author EDG
#' @export
write_Xtjson <- function(x, filepath, overwrite = FALSE) {
  # Check types ----
  check_is_S7(x, Xt)
  check_inherits(filepath, "character")

  # Check dependencies ----
  check_dependencies("jsonlite")

  # Normalize path ----
  filepath <- normalizePath(filepath, mustWork = FALSE)

  # Check file ----
  if (file.exists(filepath) && !overwrite) {
    cli::cli_abort(
      "File ",
      filepath,
      " exists.\033[0m",
      "\n  Set",
      highlight("`overwrite = TRUE`"),
      "if you wish to overwrite."
    )
  }

  # Save to file
  jsonlite::write_json(
    x = as.list(x),
    path = filepath,
    simplifyVector = TRUE,
    simplifyMatrix = FALSE
  )
} # /rtemis.bio::write_Xtjson


#' Read `Xt` object from JSON file
#'
#' @details Note that factors saved under `group` are written as character by [write_Xtjson] and
#' when they are read back in, they are converted back to factors using [factor]. This means that
#' the levels will be set alphabetically. If needed, reorder them after reading in the JSON file
#' using [factor].
#'
#' @param filepath Character: Path to JSON file.
#' @param verbosity Integer: if greater than 0, print messages.
#'
#' @return `Xt` object.
#'
#' @author EDG
#' @export
read_Xtjson <- function(filepath, verbosity = 0L) {
  # Check types ----
  check_inherits(filepath, "character")

  # Check dependencies ----
  check_dependencies("jsonlite")

  # Normalize path ----
  filepath <- normalizePath(filepath)

  # Check file ----
  if (!file.exists(filepath)) {
    cli::cli_abort("File", filepath, "does not exist.")
  }

  # Read from file
  xt <- jsonlite::read_json(
    path = filepath,
    simplifyVector = TRUE,
    simplifyMatrix = FALSE
  )
  # Convert empty lists to NULL
  emptylist.idi <- sapply(xt, is.list) & sapply(xt, length) == 0
  xt[emptylist.idi] <- NULL

  # Convert groups to factors
  if (!is.null(xt$group)) {
    xt$group <- lapply(xt$group, factor)
  }

  # Convert to `Xt` object
  xt <- as_Xt(xt)

  if (verbosity > 0) {
    cat("Read", filepath, ":\n", sep = "")
    print(xt)
  }
  xt
} # /rtemis.bio::read_Xtjson
