#########################
# TEST 1: Simple problem #
#########################
set.seed(123)

# generate data with normal distributed error
n <- 1e3
y <- rnorm(n)
x <- y * .3 + rnorm(n, 0, .25)
x2 <- x + rnorm(n, 2, 3)
x <- cbind(x, x2)

# make missingness
y[sample(1:n, n * .3)] <- NA
ry <- !is.na(y)
wy <- !ry

# Use univariate imputation model
set.seed(123)
imps_t1 <- mice.impute.mars(y, ry, x)

test_that("Returns requested length", {
  expect_equal(length(imps_t1), sum(!ry))
})

#########################
# TEST 2: Use it within mice call #
#########################

boys_cont <- mice::boys[, 1:4]
imp <- mice(boys_cont,
                     m = 2, maxit = 2, method = "mars",
                     print = FALSE
)

test_that("mice call works", {
  expect_equal(class(imp), "mids")
})

