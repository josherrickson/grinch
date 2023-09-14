test_that("aritmetic mean", {
  x <- rnorm(100)
  expect_identical(mean(x), arithmetic.mean(x))
  expect_identical(mean(x, trim = .1), arithmetic.mean(x, trim = .1))
  expect_identical(mean(x, trim = .6), arithmetic.mean(x, trim = .6))
  expect_identical(mean(x, na.rm = TRUE), arithmetic.mean(x, na.rm = TRUE))
  expect_identical(mean(x, trim = .2, na.rm = TRUE),
                   arithmetic.mean(x, trim = .2, na.rm = TRUE))

  x[1] <- NA
  expect_identical(mean(x), arithmetic.mean(x))
  expect_identical(mean(x, trim = .1), arithmetic.mean(x, trim = .1))
  expect_identical(mean(x, trim = .6), arithmetic.mean(x, trim = .6))
  expect_identical(mean(x, na.rm = TRUE), arithmetic.mean(x, na.rm = TRUE))
  expect_identical(mean(x, trim = .2, na.rm = TRUE),
                   arithmetic.mean(x, trim = .2, na.rm = TRUE))

})

test_that("geometric mean", {
  x <- c(7, 3, 1)
  expect_true(all.equal(geometric.mean(x), 21^(1/3)))

  x <- c(-2, 4, 1)
  expect_identical(geometric.mean(x), NaN)

  x <- c(0, 1, 5)
  expect_identical(geometric.mean(x, zero.action = "keep"), 0)
  expect_identical(geometric.mean(x, zero.action = "drop"), 5^(1/2))
  expect_true(all.equal(geometric.mean(x, zero.action = "ignore"), 5^(1/3)))
})

test_that("harmonic mean", {

  x <- c(1, 4, 4)
  expect_true(all.equal(harmonic.mean(x), 2))
})


test_that("quadratic mean", {

  x <- c(1, 4, 4)
  expect_true(all.equal(quadratic.mean(x), sqrt(11)))

})

test_that("cubic mean", {

  x <- c(1, 4, 4)
  expect_true(all.equal(cubic.mean(x), 43^(1/3)))

})

test_that("quasi_arithmetic.mean", {
  x <- c(5, 2, 6, 6)

  expect_identical(quasi_arithmetic.mean(x,
                                         function(x) x,
                                         function(x) x),
                   arithmetic.mean(x))
  expect_identical(quasi_arithmetic.mean(x,
                                         function(x) log(x),
                                         function(x) exp(x)),
                   geometric.mean(x))
  expect_identical(quasi_arithmetic.mean(x,
                                         function(x) 1/x,
                                         function(x) 1/x),
                   harmonic.mean(x))

  expect_identical(quasi_arithmetic.mean(x,
                                         function(x) x^2,
                                         function(x) x^{1/2}),
                   quadratic.mean(x))

  expect_identical(quasi_arithmetic.mean(x,
                                         function(x) x^3,
                                         function(x) x^{1/3}),
                   power.mean(x, p =3))

})
