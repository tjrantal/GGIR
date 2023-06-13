library(GGIR)
context("osteogenic index")
test_that("Osteogenic index peak detection works", {
  skip_on_cran()
  metrics2do = load_params()$params_metric
  metrics2do$do.osteogenicindex = TRUE
  metrics2do$do.enmo = FALSE
  metrics2do$do.anglez = FALSE
  #Create dummy data
  sf = 100
	epochsize = 240
	t = seq(0,60*60*24*1-1/sf,by=1/sf)
	sineFreq = 2
	data = matrix(nrow = length(t), ncol = 3, dimnames = list(NULL,c('X','Y','Z')))
	data[,1] = 1.4*sin(2*pi*sineFreq*t)
	data[,2] = 0
	data[,3] = 0


	output = g.applymetrics(data, sf, epochsize, metrics2do)

  expect_that(length(output), equals(32))
  expect_that(length(output[[1]]), equals(length(t)/(epochsize*sf)))
  expect_that(sum(output[[1]]),equals((length(t)/sf)*(2*sineFreq)))
})

