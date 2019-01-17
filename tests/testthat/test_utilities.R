context("utility functions")

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


test_that("look throws correct errors", {
  expect_error(look("test") , "test not a valid configuraion. Use:\nshort_range\nmedium_range\nlong_range", fixed = TRUE)
  expect_error(look("short_range", type = "test") , "short_range not a valid configuraion. Use:\nchannel\nreservoir\nland\nterrain\nforcing\nmeta", fixed = TRUE)
})


test_that("look", {

  config = nwm::look("short_range")
  type = look("medium_range", type = "channel")

  vec = c(
    is.null(config),
    is.null(type)
  )

  check = any(!isTRUE(vec))
  expect_true(check)
})



