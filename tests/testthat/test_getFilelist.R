context("getFilelist")

test_that("getFilelist throws correct errors", {
  expect_error(getFilelist(startDate = "2010-08-09") , paste("Data only archived from",
                                                       as.POSIXlt(format(Sys.Date() - 40, tz = "GMT", usetz = TRUE)),
                                                       "on ..."), fixed = TRUE)
  expect_error(getFilelist(startDate = "2030-08-09") , "Resquested date(s) have not happend...", fixed = TRUE)

  expect_error(getFilelist(config = "test"), paste(toupper("test"),"not a valid configuration, select from:\n\n",paste(names(nwm), collapse = "\n ")), fixed = TRUE)

  expect_error(getFilelist(type = "test"), paste("TEST is not a valid output type, select from:\n\n channel\n land\n forcing \n\nIf having trouble, run look('medium_range')"), fixed = TRUE)

  expect_error(getFilelist(startDate = Sys.Date() - 1,
    config = 'short_range', t = 30), paste("30 not a valid t value for short_range configuration. Use: \n\n",
 	      paste(0:23, collapse = ", "), paste0("\n\nIf having trouble, run look('short_range')")), fixed = TRUE)

  expect_error(getFilelist(config = 'short_range', f = 30), paste("30 not a valid f value for short_range configuration. Use: \n\n",
                                                                paste(1:18, collapse = ", "), paste0("\n\nIf having trouble, run look('short_range')")), fixed = TRUE)


})

test_that("getFilelist returns", {

  base = getFilelist(t = 0)
  forcing = getFilelist(t = 0, type = 'forcing')
  today = getFilelist(t = 0, startDate = Sys.Date())


  vec =  c(
    length(base) == 1,
    length(forcing) == 1,
    length(today) == 1)

  check = any(!isTRUE(vec))
  expect_true(check)
})
