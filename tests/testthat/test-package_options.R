pkg_name <- pkgload::pkg_name()

test_that("Create option envonment", {
  result <- create_option("test", value = new.env())
  expect_type(getOption("test"), "environment")
})

# Set ---------------------------------------------------------------------

test_that("Set options", {
  env <- new.env()
  result <- set_options(
    values = list(hello = "world", a = "b"),
    env = env
  )

  expect_identical(env$hello, "world")
  expect_identical(env$a, "b")
})

# Get ---------------------------------------------------------------------

test_that("Get options", {
  env <- new.env()
  set_options(
    list(hello = "world", a = "b"),
    env = env
  )

  result <- get_options(
    names = c("hello", "a"),
    env = env
  )

  expected <- list(hello = "world", a = "b")
  expect_identical(result, expected)

  result <- get_options(
    names = c("hello"),
    env = env
  )

  expected <- list(hello = "world")
  expect_identical(result, expected)
})

test_that("Get options: default env", {
  options("_pops" = NULL)
  init_package_options(
    pkg_name,
    values = list(hello = "world", a = "b", c = TRUE)
  )

  result <- get_options(
    names = c("hello", "a")
  )

  expected <- list(hello = "world", a = "b")
  expect_identical(result, expected)
})

test_that("Get option", {
  env <- new.env()
  set_options(
    list(hello = "world", a = "b"),
    env = env
  )

  result <- get_option(
    name = "hello",
    env = env
  )

  expected <- "world"
  expect_identical(result, expected)
})

test_that("Get option: default env", {
  options("_pops" = NULL)
  init_package_options(
    "pops",
    values = list(hello = "world", a = "b", c = TRUE)
  )
  # getOption("_test")
  # options() %>% names()

  result <- get_option(
    name = "hello"
  )

  expected <- "world"
  expect_identical(result, expected)
})

# Create ------------------------------------------------------------------

test_that("Create package options", {
  init_package_options()
  expect_type(getOption(option_env_name()), "environment")

  expect_identical(getOption(option_env_name()) %>% ls(), character())
})

test_that("package options: value list", {
  init_package_options(
    values = list(hello = "world", a = "b", c = TRUE)
  )
  env <- getOption(option_env_name())
  expect_type(env, "environment")
  expect_identical(env %>% ls(), c("a", "c", "hello"))
  expect_identical(env$hello, "world")
  expect_identical(env$a, "b")
  expect_identical(env$c, TRUE)
})
