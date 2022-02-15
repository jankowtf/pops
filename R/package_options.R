# Create ------------------------------------------------------------------

#' Create option
#'
#' Creates the desired option "directly" in the global options (as opposed to in
#' the environment created in the global options via
#' [pops::init_package_options]).
#'
#' @param name ([character]) Option name
#' @param value ([character]) Option value
#'
#' @return
#' @export
#'
#' @examples
#' create_option("hello", "world")
#' getOption("hello")
create_option <- function(
  name,
  value
) {
  args <- list(value)
  names(args) <- name
  rlang::call2("options", !!!args) %>%
    rlang::eval_tidy()
}

#' Initialize package options
#'
#' This creates/sets an [environment] in the global options that acts as an
#' "options container" for custom options/settings/preferences for your package.
#'
#' This function is typically called within your custom [.onLoad()] or
#' [.onAttach()] function. Argument `values` lets you customize the actual
#' option values you want to set/create within the option environment.
#'
#' @param pkg_name ([character]) Package name. This typically comes from the
#'   `pkgname` argument of `.onLoad()` or `.onAttach()`
#' @param values ([list]) Optional values to set. You can also set them later
#'   via [pops::set_options].
#' @param prefix ([character]) Prefix to be used for name of option environment.
#'
#' @return
#' @export
#'
#' @examples
#' init_package_options(values = list(hello = "world", test = TRUE))
#' option_env_name <- options() %>% names() %>% stringr::str_subset("__.__")
#' option_env <- option_env_name %>% getOption()
#' option_env %>% ls()
#' option_env$hello
#'
#' # Helper functions to access options inside the option environment
#' get_options()
#' get_options(c("hello")) # Always returns the actual option values wrapped into a list
#' get_option("hello") # Always returns the actual option value
init_package_options <- function(
  pkg_name = pops:::pkg_name(),
  values = list(),
  prefix = pops:::options_prefix()
) {
  pkg_name <- unname(pkg_name)

  # Create and populate env
  env <- new.env()
  # values <- c(
  #   list(pkg_name = pkg_name),
  #   values
  # )
  values %>% set_options(env = env)

  # Set option env
  create_option(
    name = stringr::str_c(prefix, pkg_name),
    value = env
  )

  invisible(TRUE)
}

# Set ---------------------------------------------------------------------

#' Set options (via names list)
#'
#' By default, the option is created **inside** the environment that is
#' created/set as an option via [pops::init_package_options] (see default
#' value of the `env` argument).
#'
#' @param values ([list]) Named list with name-value pairs
#' @param env ([environment]) Custom environment in options. See
#'   [pops::init_package_options]
#'
#' @return
#' @export
#'
#' @examples
#' init_package_options()
#' set_options(list(hello = "world", test = TRUE))
#' get_options()
#' get_option("hello")
#' # Same as
#' getOption(pops:::option_env_name())$hello
set_options <- function(
  values = list(),
  env = getOption(pops:::option_env_name())
) {
  values %>% purrr::iwalk(function(.x, .y) {
    assign(.y, .x, envir = env)
  })
}

# Get ---------------------------------------------------------------------

#' Get options (via names list)
#'
#' Get option values from option environment created via
#' [pops::init_package_options].
#'
#' @param names ([character]) Vector of names to get.
#' @param env ([environment]) Custom environment in options. See
#'   [pops::init_package_options]
#'
#' @return
#' @export
#'
#' @examples
#' init_package_options(values = list(hello = "world", test = TRUE))
#' get_options()
#' get_options(c("hello", "test"))
#' get_options(c("hello"))
get_options <- function(
  names = character(),
  env = getOption(pops:::option_env_name())
) {
  if (!length(names)) {
    names <- env %>% ls()
  }

  names %>% purrr::map(function(.x) {
    args <- list(
      x = .x,
      envir = env
    )
    rlang::call2("get", !!!args) %>%
      rlang::eval_tidy()
  }) %>%
    purrr::set_names(names)
}

#' Get options (via names list)
#'
#' Get atomic option value from option environment created via
#' [init_package_options].
#'
#' @param name ([character]) Name to get
#' @param env ([environment]) Custom environment in options. See
#'   [pops::init_package_options]
#'
#' @return
#' @export
#'
#' @examples
#' init_package_options(values = list(hello = "world", test = TRUE))
#' get_option("hello")
#' try(get_option())
#' # Same as
#' getOption(pops:::option_env_name())$hello
get_option <- function(
  name,
  env = getOption(pops:::option_env_name())
) {
  # Input validation
  stopifnot("Arg 'name' needs to be of length 1" = length(name) == 1)

  # name %>%
  #   get_options(env = env) %>%
  #   `[[`(1)
  get(name, envir = env, inherits = FALSE)
}

# Helpers -----------------------------------------------------------------

options_prefix <- function() {
  "__.__"
}

option_env_name <- function() {
  pkgload::pkg_name() %>% stringr::str_c(pops:::options_prefix(), .)
}

pkg_name <- function() {
  pkgload::pkg_name()
}

deparse_and_clip <- function(x) {
  x %>% deparse() %>% clipr::write_clip()
}
