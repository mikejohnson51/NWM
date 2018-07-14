context("utility functions")

test_that("cida server throws correct errors", {
  expect_error(query_cida(AOI::getAOI(clip = list("UCSB", 1, 1)), type = "test") , "Type not found.", fixed = TRUE)
  expect_error(query_cida(AOI::getAOI(clip = list("UCSB", .1, .1)), type = "nhdflowline_network") , "O features found in this AOI.", fixed = TRUE)
})


