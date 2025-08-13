# A3.R
# ::rtemisbio::
# 2024- EDG rtemis.org

#' @title A3 Class
#'
#' @description
#' The annotated amino acid (A3) class is designed to store and manage
#' amino acid sequences and associated annotations.
#'
#' @field sequence Character: Amino acid sequence using single-letter codes.
#' @field annotations List: Named list including site, region, PTM, cleavage_site, and variant information.
#' @field uniprotid Character: Uniprot ID associated with the sequence, if available.
#' @field description Character: Description of the data / experiment.
#' @field reference Character: Link to reference (journal publication, preprint, etc.)
#'
#' @author EDG
#'
#' @noRd
A3 <- new_class(
  "A3",
  properties = list(
    sequence = class_character,
    annotations = class_list,
    uniprotid = class_character,
    description = class_character,
    reference = class_character
  ),
  constructor = function(
    sequence,
    site = NULL,
    region = NULL,
    ptm = NULL,
    cleavage_site = NULL,
    variant = NULL,
    uniprotid = NULL,
    description = NULL,
    reference = NULL
  ) {
    check_inherits(site, "list")
    check_inherits(region, "list")
    check_inherits(ptm, "list")
    check_inherits(cleavage_site, "list")
    check_inherits(variant, "list")
    annotations <- list(
      site = site,
      region = region,
      ptm = ptm,
      cleavage_site = cleavage_site,
      variant = variant
    )
    new_object(
      S7_object(),
      sequence = sequence,
      annotations = annotations,
      uniprotid = uniprotid,
      description = description,
      reference = reference
    )
  }
) # /rtemisbio::A3

# `[` method for A3 ----
method(`[`, A3) <- function(x, names) {
  lapply(names, function(name) prop(x, name))
} # A3.[

# `[[` method for A3 ----
method(`[[`, A3) <- function(x, name) {
  prop(x, name)
} # A3.[[

# create_A3 ----
#' Create an `A3` object
#'
#' Creates an `A3` object given amino acid sequence and annotations.
#'
#' @details
#' We choose to keep NULL elements as empty lists in JSON, since we want users to be
#' able to easily add annotations, whether programmaticaly, using a web app, or
#' manually.
#'
#' @param sequence Character: Amino acid sequence.
#' @param site Named list of vectors of integer indices of sites, e.g.
#' `list("N-terminal repeat" = c(46, 47, 52), "Microtubule binding domain" = c(244, 245, 246))`
#' @param region Named list of integer indices,
#' e.g. `list("Phosphodegron" = c(46, 47, 48, 49, 50, 51), "KXGS" = c(259, 260, 261, 262))`
#' or character vectors with index range of regions in format
#' `start:end`, e.g. `list(Phosphodegron = c("46:51", "149:154"), KXGS = c("259:262", "290:293"))`
#' @param ptm Named list of vectors with indices of post-translational modifications, e.g.
#' `list("Phosphorylation" = c(17, 18, 29, 30), "Acetylation" = c(148, 150, 163))`
#' @param cleavage_site Named list of cleavage sites, e.g.
#' `list(CTSL = c(54, 244, 319), CTSD = c(340, 391, 426))`
#' @param variant List of lists with variant information. Each list must contain a
#' `position` element.
#' @param uniprotid Character: Uniprot ID.
#' @param description Character: Description of the data / experiment.
#' @param reference Character: Link to reference (journal publication, preprint, etc.)
#'
#' @author EDG
#' @return `A3` object
#' @export

create_A3 <- function(
  sequence,
  site = NULL,
  region = NULL,
  ptm = NULL,
  cleavage_site = NULL,
  variant = NULL,
  uniprotid = NULL,
  description = NULL,
  reference = NULL
) {
  # Check types
  check_inherits(sequence, "character")
  check_inherits(site, "list")
  check_inherits(region, "list")
  check_inherits(ptm, "list")
  check_inherits(cleavage_site, "list")
  check_inherits(variant, "list")
  check_inherits(uniprotid, "character")
  check_inherits(description, "character")
  check_inherits(reference, "character")

  # Convert to JSON
  A3(
    sequence = sequence,
    site = site,
    region = region,
    ptm = ptm,
    cleavage_site = cleavage_site,
    variant = variant,
    uniprotid = uniprotid,
    description = description,
    reference = reference
  )
} # /rtemisbio::create_A3

# Print A3 ----
#' Print method for `A3` object
#'
#' @method print A3
#' @param x `A3` object.
#' @param head_n Integer: Number of characters to show from the sequence.
#'
#' @author EDG
#'
#' @noRd
method(print, A3) <- function(x, head_n = 10) {
  cat(
    gray("<"),
    orange("A3", bold = TRUE),
    gray(">"),
    " (Annotation Amino Acid sequence object)\n",
    sep = ""
  )
  if (!is.null(x[["description"]])) {
    cat("  Description:", highlight(x[["description"]]), "\n")
  }
  if (!is.null(x[["uniprotid"]])) {
    cat("   Uniprot ID:", highlight(x[["uniprotid"]]), "\n")
  }
  site_annotations <- names(x[["annotations"]][["site"]])
  region_annotations <- names(x[["annotations"]][["region"]])
  ptm_annotations <- names(x[["annotations"]][["ptm"]])
  n_cleavage_site_annotations <- length(x[["annotations"]][["cleavage_site"]])
  n_variant_annotations <- length(x[["annotations"]][["variant"]])
  # Head of sequence
  cat(
    "     Sequence: ",
    bold(utils::head(x[["sequence"]], head_n)),
    "...",
    " (length = ",
    length(x[["sequence"]]),
    ")\n",
    sep = ""
  )
  # Names of annotations
  cat("  Annotations:\n")
  if (
    is.null(site_annotations) &&
      is.null(region_annotations) &&
      is.null(ptm_annotations) &&
      n_cleavage_site_annotations == 0 &&
      n_variant_annotations == 0
  ) {
    cat(italic("             None\n"))
  }
  if (length(site_annotations) > 0) {
    cat(
      "          ",
      gray(italic("Site:")),
      paste(green(site_annotations), collapse = ", "),
      "\n"
    )
  }
  if (length(region_annotations) > 0) {
    cat(
      "        ",
      gray(italic("Region:")),
      paste(green(region_annotations), collapse = ", "),
      "\n"
    )
  }
  if (length(ptm_annotations) > 0) {
    cat(
      "           ",
      gray(italic("PTM:")),
      paste(green(ptm_annotations), collapse = ", "),
      "\n"
    )
  }
  if (n_cleavage_site_annotations > 0) {
    cat(
      " ",
      gray(italic("Cleavage site:")),
      bold(n_cleavage_site_annotations),
      "annotations.\n"
    )
  }
  if (n_variant_annotations > 0) {
    cat(
      "      ",
      gray(italic("Variants:")),
      bold(n_variant_annotations),
      "variant annotations.\n"
    )
  }
  if (!is.null(x[["reference"]])) {
    cat("    Reference:", highlight(x[["reference"]]), "\n")
  }
} # /rtemisbio::print.A3


#' as_A3
#'
#' @param x List: Named list with elements `Sequence`, `Annotations`, `UniprotID`.
#' `Annotations` is a named list with possible elements `Site`, `Region`, `PTM`,
#' `Cleavage_site`, `Variant`, `Description`, `Reference`.
#'
#' @author EDG
#' @return `A3` object.
#' @export

as_A3 <- new_generic("as_A3", "x")

method(as_A3, class_list) <- function(x) {
  create_A3(
    sequence = x[["sequence"]],
    site = x[["annotations"]][["site"]],
    region = x[["annotations"]][["region"]],
    ptm = x[["annotations"]][["ptm"]],
    cleavage_site = x[["annotations"]][["cleavage_site"]],
    variant = x[["annotations"]][["variant"]],
    uniprotid = x[["uniprotid"]],
    description = x[["description"]],
    reference = x[["reference"]]
  )
} # /rtemisbio::as_A3.list


#' Plot method for `A3` object
#'
#' @param x `A3` object.
#' @param ... Additional arguments passed to [rtemis::draw_protein].
#'
#' @author EDG
#' @export

plot.A3 <- function(x, ...) {
  draw_protein(
    x = x[["sequence"]],
    site = x[["annotations"]][["site"]],
    region = x[["annotations"]][["region"]],
    ptm = x[["annotations"]][["ptm"]],
    cleavage_site = x[["annotations"]][["cleavage_site"]],
    variant = x[["annotations"]][["variant"]],
    ...
  )
} # /rtemisbio::plot.A3

method(plot, A3) <- plot.A3

#' Convert integer range to character with colon separator
#'
#' @param x Integer vector. Must be consecutive integers from lowest to highest.
#'
#' @author EDG
#' @return Character with colon separator.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- 34:42
#' int2range(x)
#' int2range(28:34)
#' int2range(c(3, 4, 5, 6))
#' # This will throw an error:
#' int2range(c(3, 4, 5, 6, 8))
#' }
int2range <- function(x) {
  # Check that x consists of consecutive integers from loweest to highest
  isTRUE(all.equal(x, seq(min(x), max(x)))) ||
    cli::cli_abort("x must be consecutive integers from lowest to highest.")

  paste0(x[1], ":", x[length(x)])
} # /rtemisbio::int2range


#' Summary method for `A3` object
#'
#' @param object `A3` object.
#'
#' @author EDG
#' @export

summary.A3 <- function(object, ...) {
  cat("Sequence length: ", length(object$Sequence), "\n")
  if (!is.null(object$UniprotID)) {
    cat("Uniprot ID: ", object$UniprotID, "\n")
  }
  if (!is.null(object$Description)) {
    cat("Description: ", object$Description, "\n")
  }
  if (!is.null(object$Reference)) {
    cat("Reference: ", object$Reference, "\n")
  }
  cat("Annotations:\n")
  if (length(object$Annotations$Site) > 0) {
    cat(length(object$Annotations$Site), "site annotations.\n")
  }
  if (length(object$Annotations$Region) > 0) {
    cat(length(object$Annotations$Region), "region annotations.\n")
  }
  if (length(object$Annotations$PTM) > 0) {
    cat(length(object$Annotations$PTM), "PTM annotations.\n")
  }
  if (length(object$Annotations$Cleavage_site) > 0) {
    cat(
      length(object$Annotations$Cleavage_site),
      "cleavage site annotations.\n"
    )
  }
  if (length(object$Annotations$Variant) > 0) {
    cat(length(object$Annotations$Variant), "variant annotations.\n")
  }
} # /rtemisbio::summary.A3
