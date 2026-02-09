# test_Xt.R
# ::rtemis.bio::

# devtools::load_all("~/Code/rtemis-org/rtemis/")
# library(testthat)

# %% Xt() ----
n <- 500
r <- sort(rnorm(n))
x <- list(
  time = seq_len(n)
)
y <- list(
  sinx = sin(r) + .05 * rnorm(n),
  cosx = cos(r) + .05 * rnorm(n)
)

obj <- Xt(
  x = x,
  y = y
)
test_that("Xt() creates Xt object", {
  expect_s7_class(obj, Xt)
})

# %% `[` method for Xt ----
test_that("`[` method for Xt works", {
  expect_equal(obj[c("x", "y")], list(x = x, y = y))
})

# %% `[[` method for Xt ----
test_that("`[[` method for Xt works", {
  expect_equal(obj[["x"]], x)
  expect_equal(obj[["y"]], y)
})

# %% create_Xt ----
obj <- create_Xt(
  x = x,
  y = y
)

# %% print Xt ----
test_that("Print Xt works", {
  expect_no_error(print(obj))
})

# %% as_Xt ----
test_that("as_Xt() creates Xt object", {
  obj <- list(
    x = x,
    y = y
  ) |>
    as_Xt()
  expect_s7_class(obj, Xt)
})

# %% plot Xt ----
plt <- plot(obj)
test_that("plot.Xt creates plotly object", {
  expect_s3_class(plt, "plotly")
})
