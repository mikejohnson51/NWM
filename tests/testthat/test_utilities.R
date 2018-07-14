context("utility functions")

test_that("cida server throws correct errors", {
  expect_error(query_cida(AOI::getAOI(clip = list("UCSB", 1, 1)), type = "test") , "Type not found.", fixed = TRUE)
  expect_error(query_cida(AOI::getAOI(clip = list("UCSB", .1, .1)), type = "nhdflowline_network") , "O features found in this AOI.", fixed = TRUE)
})

test_that("utility_cida", {
  data_check = !is.null(nwm::comids_all)
  cida_sf = query_cida(AOI::getAOI(clip = list("UCSB", 10, 10)), type = "nhdflowline_network")
  cida_sp = query_cida(AOI::getAOI(clip = list("UCSB", 10, 10)), type = "nhdflowline_network", spatial = T)
  #error   = try(query_cida(AOI::getAOI(clip = list("UCSB", .1, .1))))

  vec = c(
        data_check,
        any(class(cida_sf) == 'sf'),
        any(class(cida_sp) == 'SpatialLinesDataFrame')
        #class(error) == 'try-error'
        )

  check = any(!isTRUE(vec))
  expect_true(check)
})

test_that("getBreaks", {

  index = c(1:30, 40:50, 200:210)
  test1 = getBreaks(index, gap = 50)
  test2 = getBreaks(index, gap = 5)


  vec = c(
   length(test1) == 2,
   length(test2) == 3
  )

  check = any(!isTRUE(vec))
  expect_true(check)
})

