test_that("fixed parameter filter works", {
  params <- irace::readParameters(text = "
    fixedParam \"\" c (value)
    notFixed \"\" c (value, another_value)
  ")
  expect_setequal(fixed_parameters(params), c("fixedParam"))
})

test_that("variable filter works", {
  params <- irace::readParameters(text = "
    fixedParam \"\" c (value)
    notFixed \"\" c (value, another_value)
  ")
  expect_setequal(variable_parameters(params), c("notFixed"))
})

test_that("discrete filter works", {
  params <- irace::readParameters(text = "
    discreteCat \"\" c (a, b)
    discreteOrd \"\" o (1, 2)
    discreteInt \"\" i (1, 2)
    numericReal \"\" r (1, 2)
  ")
  expect_setequal(
    discrete_parameters(params), 
    c("discreteCat", "discreteOrd", "discreteInt")
  )
})

test_that("continuous filter works", {
  params <- irace::readParameters(text = "
    discreteCat \"\" c (a, b)
    discreteOrd \"\" o (1, 2)
    discreteInt \"\" i (1, 2)
    numericReal \"\" r (1, 2)
  ")
  expect_setequal(continuous_parameters(params), c("numericReal"))
})

test_that("non numeric filter works", {
  params <- irace::readParameters(text = "
    discreteCat \"\" c (a, b)
    discreteOrd \"\" o (1, 2)
    discreteInt \"\" i (1, 2)
    numericReal \"\" r (1, 2)
  ")
  expect_setequal(
    non_numeric_parameters(params),
    c("discreteCat", "discreteOrd")
  )
})

test_that("negate filter works", {
  params <- irace::readParameters(text = "
    discreteCat \"\" c (a, b)
    discreteOrd \"\" o (1, 2)
    discreteInt \"\" i (1, 2)
    numericReal \"\" r (1, 2)
  ")
  expect_setequal(
    parameters_not(non_numeric_parameters)(params),
    c("discreteInt", "numericReal")
  )
})

test_that("multiple filter where all filter is true works", {
  params <- irace::readParameters(text = "
    discreteCat \"\" c (a, b)
    fixed \"\" c (a)
    discreteOrd \"\" o (1, 2)
    discreteInt \"\" i (1, 2)
    numericReal \"\" r (1, 2)
  ")
  expect_setequal(
    parameters_all(discrete_parameters, variable_parameters)(params),
    c("discreteCat", "discreteOrd", "discreteInt")
  )
})

test_that("multiple filter where any filter is true works", {
  params <- irace::readParameters(text = "
    discreteCat \"\" c (a, b)
    discreteOrd \"\" o (1, 2)
    discreteInt \"\" i (1, 2)
    numericReal \"\" r (1, 2)
  ")
  expect_setequal(
    parameters_any(continuous_parameters, non_numeric_parameters)(params),
    c("discreteCat", "discreteOrd", "numericReal")
  )
})