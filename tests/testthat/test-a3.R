# test-a3.R
# ::rtemisbio::

# %% a3() ----
x <- a3(
  seq = "ARNDCEQGHILKMFPSTWYV",
  site = list(
    Phosphorylation = c(2, 5),
    Glycosylation = c(3, 6)
  ),
  region = list(
    Transmembrane = c(5:10)
  ),
  ptm = list(
    Methylation = c(4, 8)
  ),
  clv = list(
    Signal_peptide = c(1, 20)
  ),
  variant = list(
    Mango = c(5, 7)
  ),
  uniprotid = "P12345",
  description = "Test protein",
  reference = "Test reference"
)

test_that("a3() creates a3 object", {
  expect_s3_class(x, "a3")
})
