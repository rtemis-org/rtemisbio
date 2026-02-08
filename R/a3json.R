# write_A3json.R
# ::rtemisbio::
# 2024- EDG rtemis.org

#' Write `A3` object to JSON file
#'
#' @param x `A3` object, as created by `as_A3()`.
#' @param filepath Character: Path to save JSON file.
#' @param overwrite Logical: If TRUE, overwrite existing file.
#'
#' @return Nothing. Writes JSON file.
#'
#' @author EDG
#' @export
write_A3json <- function(x, filepath, overwrite = FALSE) {
  # Check types ----
  check_is_S7(x, A3)
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
} # /rtemisbio::write_A3json


# read_A3json.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' Read `A3` object from JSON file
#'
#' @param filepath Character: Path to JSON file.
#' @param verbosity Integer: if greater than 0, print messages.
#'
#' @return `A3` object.
#'
#' @author EDG
#' @export
read_A3json <- function(filepath, verbosity = 0L) {
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
  A3 <- jsonlite::read_json(
    path = filepath,
    simplifyVector = TRUE,
    simplifyMatrix = FALSE
  ) |>
    as_A3()

  if (verbosity > 0) {
    cat("Read", filepath, ":\n", sep = "")
    print(A3)
  }
  A3
} # /rtemisbio::read_A3json
