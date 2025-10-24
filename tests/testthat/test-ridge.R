library(MASS)

# --- Test Case 1: Compare Coefficients ---
test_that("ridgereg coefficients are consistent with lm.ridge", {
  data(iris)

  # Define a lambda value for the test
  test_lambda <- 0.5

  # Fit models using both functions
  model_custom <- ridgereg(Sepal.Length ~ Petal.Length + Petal.Width, data = iris, lambda = test_lambda)
  model_mass <- lm.ridge(Sepal.Length ~ Petal.Length + Petal.Width, data = iris, lambda = test_lambda)

  # Back-transform lm.ridge coefficients to be on the original scale
  coef_mass_scaled <- model_mass$coef
  scales <- model_mass$scales
  xm <- model_mass$xm
  coef_mass_unscaled <- coef_mass_scaled / scales
  intercept_mass <- model_mass$ym - sum(coef_mass_unscaled * xm)
  coef_mass_comparable <- c(intercept_mass, coef_mass_unscaled)
  names(coef_mass_comparable) <- names(coef(model_custom))

  # Expect that the coefficients are equal within a specified tolerance.
  # We test the intercept and predictors separately due to penalization differences.
  expect_equal(coef(model_custom)[1], coef_mass_comparable[1],
               tolerance = 0.5,
               label = "Intercept coefficients should be reasonably close")

  expect_equal(coef(model_custom)[-1], coef_mass_comparable[-1],
               tolerance = 0.5,
               label = "Predictor coefficients should be reasonably close")
})

# --- Test Case 2: Compare Predictions ---
test_that("ridgereg predictions are consistent with lm.ridge", {

  data(iris)

  # Define a lambda value for the test
  test_lambda <- 0.5

  # Fit models
  model_custom <- ridgereg(Sepal.Length ~ Petal.Length + Petal.Width, data = iris, lambda = test_lambda)
  model_mass <- lm.ridge(Sepal.Length ~ Petal.Length + Petal.Width, data = iris, lambda = test_lambda)

  # Back-transform lm.ridge coefficients for manual prediction
  coef_mass_scaled <- model_mass$coef
  scales <- model_mass$scales
  xm <- model_mass$xm
  coef_mass_unscaled <- coef_mass_scaled / scales
  intercept_mass <- model_mass$ym - sum(coef_mass_unscaled * xm)

  # A) Test predictions on original data (fitted values)
  fitted_custom <- predict(model_custom)

  X_original <- model.matrix(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
  fitted_mass <- as.vector(X_original %*% c(intercept_mass, coef_mass_unscaled))

  expect_equal(fitted_custom, fitted_mass,
               tolerance = 0.1,
               label = "Fitted values on original data should be close")

  # B) Test predictions on new data
  new_data <- data.frame(Petal.Length = c(5.1, 2.3), Petal.Width = c(1.8, 0.5))
  pred_custom <- predict(model_custom, newdata = new_data)

  X_new <- model.matrix(~ Petal.Length + Petal.Width, data = new_data)
  pred_mass <- as.vector(X_new %*% c(intercept_mass, coef_mass_unscaled))

  expect_equal(pred_custom, pred_mass,
               tolerance = 0.1,
               label = "Predictions on new data should be close")
})
