# xt.R
# ::rtemisbio::
# 2024 EDG rtemis.org

#' @title Xt Timeseries Class
#'
#' @description
#' Timeseries class is designed to store and manipulate time series data.
#'
#' @field x Named list of datetime vectors.
#' @field y Named list of numeric vectors: When plotted, these will correspond to the
#' left y-axis.
#' @field x2 Named list of datetime vectors: When plotted, these will correspond to the
#' right x-axis. If not provided, `x` will be used for both `y` and `y2`.
#' @field y2 Named list of numeric vectors: When plotted, these will correspond to the
#' right y-axis.
#' @field zt Numeric vector: Zeitgeber time. If provided, this will be used to label x-axis ticks.
#' Assumes a single datetime vector in `x`. Elements in `zt` must correspond to elements in `x`.
#' @field shade Binary vector: `0` indicates no shading, `1` indicates shading. If provided,
#' this will be used to shade the plot.
#' @field group Named list of factors: Grouping variable(s).
#' @field xunits Character: Units for `x`.
#' @field yunits Character: Units for `y`.
#' @field y2units Character: Units for `y2`.
#' @field description Character: Description of the data / experiment.
#' @field reference Character: Link to reference (journal publication, preprint, etc.)
#'
#' @author EDG
#' @noRd
Xt <- new_class(
  "Xt",
  properties = list(
    x = class_list,
    y = class_list,
    x2 = class_list | NULL,
    y2 = class_list | NULL,
    zt = class_numeric | NULL,
    shade = class_numeric | NULL,
    group = class_list | NULL,
    xunits = class_character | NULL,
    yunits = class_character | NULL,
    y2units = class_character | NULL,
    description = class_character | NULL,
    reference = class_character | NULL
  )
) # /rtemisbio::Xt


# `[` method for Xt ----
method(`[`, Xt) <- function(x, elements) {
  sapply(
    elements,
    function(name) prop(x, name),
    simplify = FALSE,
    USE.NAMES = TRUE
  )
} # Xt.[


# `[[` method for Xt ----
method(`[[`, Xt) <- function(x, name) {
  prop(x, name)
} # Xt.[[


#' Create an `Xt` object
#'
#' Creates an `Xt` object from time series data.
#'
#' @param x Named list of datetime vectors.
#' @param y Named list of numeric vectors: When plotted, these will correspond to the
#' left y-axis.
#' @param x2 Named list of datetime vectors: When plotted, these will correspond to the
#' right x-axis. If not provided, `x` will be used for both `y` and `y2`.
#' @param y2 Named list of numeric vectors: When plotted, these will correspond to the
#' right y-axis.
#' @param zt Numeric vector: Zeitgeber time. If provided, this will be used to label x-axis ticks.
#' Assumes a single datetime vector in `x`. Elements in `zt` must correspond to elements in `x`.
#' @param shade Binary vector: `0` indicates no shading, `1` indicates shading. If provided,
#' this will be used to shade the plot.
#' @param group Named list of factors: Grouping variable(s).
#' @param xunits Character: Units for `x`.
#' @param yunits Character: Units for `y`.
#' @param y2units Character: Units for `y2`.
#' @param description Character: Description of the data / experiment.
#' @param reference Character: Link to reference (journal publication, preprint, etc.)
#'
#' @return `Xt` object
#'
#' @author EDG
#' @export
create_Xt <- function(
  x,
  y,
  x2 = NULL,
  y2 = NULL,
  zt = NULL,
  shade = NULL,
  group = NULL,
  xunits = NULL,
  yunits = NULL,
  y2units = NULL,
  description = NULL,
  reference = NULL
) {
  # Check types
  check_inherits(xunits, "character")
  check_inherits(yunits, "character")
  check_inherits(y2units, "character")
  check_inherits(description, "character")
  check_inherits(reference, "character")

  if (!is.list(x)) {
    x <- list(x)
  }

  if (!is.list(y)) {
    y <- list(y)
  }

  if (!is.null(x2)) {
    if (!is.list(x2)) {
      x2 <- list(x2)
    }
  }

  if (!is.null(y2)) {
    if (!is.list(y2)) {
      y2 <- list(y2)
    }
  }

  if (!is.null(group)) {
    if (!is.list(group)) {
      group <- list(group)
    }
  }

  # Convert to `Xt` object
  Xt(
    x = x,
    x2 = x2,
    y = y,
    y2 = y2,
    zt = zt,
    shade = shade,
    group = group,
    xunits = xunits,
    yunits = yunits,
    y2units = y2units,
    description = description,
    reference = reference
  )
} # /rtemisbio::create_Xt


#' Print method for `Xt` object
#'
#' @method print `Xt`
#'
#' @param x `Xt` object.
#' @param ... Not used.
#'
#' @return `Xt` object, invisibly.
#'
#' @author EDG
#'
#' @noRd
method(print, Xt) <- function(x, head_n = 10, ...) {
  cat("  .:", orange("xt", bold = TRUE), " timeseries object\n", sep = "")
  if (!is.null(x[["description"]])) {
    cat("  Description:", highlight(x[["description"]]), "\n")
  }
  length_x <- length(x[["x"]])
  cat(
    "  ",
    highlight(length_x),
    " x time ",
    ngettext(length_x, "vector", "vectors"),
    " of ",
    ngettext(length_x, "length ", "lengths "),
    highlight(paste(sapply(x[["x"]], length), collapse = ", ")),
    "\n",
    sep = ""
  )

  length_x2 <- length(x[["x2"]])
  if (length_x2 > 0) {
    cat(
      "  ",
      highlight(length_x2),
      " x2 time ",
      ngettext(length_x2, "vector", "vectors"),
      " of ",
      ngettext(length_x2, "length ", "lengths "),
      highlight(paste(sapply(x[["x2"]], length), collapse = ", ")),
      "\n",
      sep = ""
    )
  }

  length_y <- length(x[["y"]])
  cat(
    "  ",
    highlight(length_y),
    " y timeseries: ",
    paste(highlight(names(x[["y"]])), collapse = ", "),
    "\n",
    sep = ""
  )

  if (!is.null(x[["yunits"]])) {
    cat(
      "    Units of y timeseries:",
      paste(highlight(x[["yunits"]]), collapse = ", "),
      "\n"
    )
  }

  length_y2 <- length(x[["y2"]])
  if (length_y2 > 0) {
    cat(
      "  ",
      highlight(length_y2),
      " y2 timeseries: ",
      paste(highlight(names(x[["y2"]])), collapse = ", "),
      "\n",
      sep = ""
    )
  }

  if (!is.null(x[["y2units"]])) {
    cat(
      "    Units of y2 timeseries:",
      paste(highlight(x[["y2units"]]), collapse = ", "),
      "\n"
    )
  }

  if (!is.null(x[["group"]])) {
    cat(
      "  ",
      highlight(length(x[["group"]])),
      ngettext(length(x[["group"]]), " grouping", " groupings"),
      ": ",
      paste(highlight(names(x[["group"]])), collapse = ", "),
      "\n",
      sep = ""
    )
  }
  if (!is.null(x[["reference"]])) {
    cat("  Reference:", highlight(x[["reference"]]), "\n")
  }
  invisible(x)
} # /rtemisbio::print.Xt


#' as_Xt
#'
#' @param x Object to convert to `Xt`.
#'
#' @return `Xt` object.
#'
#' @author EDG
#' @export
as_Xt <- new_generic("as_Xt", "x")

method(as_Xt, class_list) <- function(x) {
  # Check types
  check_inherits(x[["xunits"]], "character")
  check_inherits(x[["yunits"]], "character")
  check_inherits(x[["y2units"]], "character")
  check_inherits(x[["description"]], "character")
  check_inherits(x[["reference"]], "character")

  # Create `Xt` object
  xt <- create_Xt(
    x = x[["x"]],
    y = x[["y"]],
    y2 = x[["y2"]],
    zt = x[["zt"]],
    shade = x[["shade"]],
    group = x[["group"]],
    xunits = x[["xunits"]],
    yunits = x[["yunits"]],
    y2units = x[["y2units"]],
    description = x[["description"]],
    reference = x[["reference"]]
  )
  xt
} # /rtemisbio::as_Xt.list


#' Plot method for `Xt` object
#'
#' @param x `Xt` object.
#' @param ... Additional arguments passed to [rtemis::draw_xt].
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
plot.Xt <- method(plot, Xt) <- function(x, ...) {
  .x <- x[["x"]]
  draw_xt(
    x = .x,
    y = x[["y"]],
    x2 = x[["x2"]],
    y2 = x[["y2"]],
    zt = x[["zt"]],
    shade_bin = x[["shade"]],
    xunits = x[["xunits"]],
    yunits = x[["yunits"]],
    y2units = x[["y2units"]],
    ...
  )
} # /rtemisbio::plot.Xt


#' Aggregate method for `Xt` object
#'
#' @param x `Xt` object.
#' @param group Character: Grouping variable.
#' @param fn Function: Function to apply to each group.
#' @param backend Character: "base", "data.table", or "dplyr"; backend to use for aggregation.
#' @param ... Additional arguments passed to `fn`.
#'
#' @author EDG
#' @export
#'
#' @importFrom stats aggregate
aggregate.Xt <- method(aggregate, Xt) <- function(
  x,
  groupname,
  fn = mean,
  backend = getOption("rtemis_backend", "base"),
  ...
) {
  check_inherits(x, "xt")
  # Appease R CMD check
  y <- y2 <- NULL
  # Get name of fn
  fn_name <- deparse(substitute(fn))
  # Aggregate all y and y2 timeseries by grouping in `group`
  if (backend == "base") {
    # base
    y_agg <- lapply(seq_along(x[["y"]]), function(i) {
      out <- aggregate(
        list(y = x[["y"]][[i]]),
        by = list(x[["group"]][[groupname]]),
        FUN = fn,
        ...
      )
      names(out) <- c(groupname, fn_name)
      out
    })

    if (!is.null(x[["y2"]])) {
      y2_agg <- lapply(seq_along(x[["y2"]]), function(i) {
        out <- aggregate(
          list(y2 = x[["y2"]][[i]]),
          by = list(x[["group"]][[groupname]]),
          FUN = fn,
          ...
        )
        names(out) <- c(groupname, paste0(fn_name))
        out
      })
    }
  } else if (backend == "data.table") {
    # data.table
    y_agg <- lapply(seq_along(x[["y"]]), function(i) {
      data.table::data.table(y = x[["y"]][[i]])[,
        list(agg = fn(y)),
        by = x[["group"]][[groupname]]
      ] |>
        data.table::setorder() |>
        data.table::setnames(c(groupname, fn_name))
    })
    if (!is.null(x[["y2"]])) {
      y2_agg <- lapply(seq_along(x[["y2"]]), function(i) {
        data.table::data.table(y2 = x[["y2"]][[i]])[,
          list(agg = fn(y2)),
          by = x[["group"]][[groupname]]
        ] |>
          data.table::setorder() |>
          data.table::setnames(c(groupname, fn_name))
      })
    }
  } else if (backend == "dplyr") {
    # dplyr
    y_agg <- lapply(seq_along(x[["y"]]), function(i) {
      dplyr::tibble(y = x[["y"]][[i]]) |>
        dplyr::group_by(Group = x[["group"]][[groupname]]) |>
        dplyr::summarize(!!fn_name := fn(y, ...))
    })
    if (!is.null(x[["y2"]])) {
      y2_agg <- lapply(seq_along(x[["y2"]]), function(i) {
        dplyr::tibble(y2 = x[["y2"]][[i]]) |>
          dplyr::group_by(Group = x[["group"]][[groupname]]) |>
          dplyr::summarize(!!fn_name := fn(y2, ...))
      })
    }
  }

  out <- list(y = y_agg)
  if (!is.null(x[["y2"]])) {
    out[["y2"]] <- y2_agg
  }
  out
} # /rtemisbio::aggregate.Xt


#' Calculate light/dark ratio for `Xt` object
#'
#' Calculates light/dark ratio for each `y` and `y2` timeseries in an `Xt` object.
#'
#' @param x `Xt` object.
#' @param fn Function: Function to apply to each group.
#' @param backend Character: "base", "data.table", or "dplyr"; backend to use for aggregation.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return data.frame with columns for group and summary statistic.
#'
#' @author EDG
#' @export
light_dark_ratio <- function(
  x,
  groupname = "Lights",
  fn = mean,
  backend = getOption("rtemis_backend", "data.table"),
  ...
) {
  # Check types
  check_inherits(x, "xt")
  aggregate(x, groupname = groupname, fn = fn, backend = backend, ...)
} # /rtemisbio::light_dark_ratio
