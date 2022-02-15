
# Dev dependencies --------------------------------------------------------

renv::install("devtools")
renv::install("testthat")
renv::install("roxygen2")
renv::install("roxygen2md")
renv::install("rmarkdown")
renv::install("here")

# Dev preparations --------------------------------------------------------

# "Add the pipe"
usethis::use_pipe()

# Add package description
usethis::use_package_doc()

# Use {thestthat}
usethis::use_testthat()

usethis::use_package("testthat", type = "Suggests")

usethis::use_roxygen_md()
roxygen2md::roxygen2md()
usethis::use_mit_license()
usethis::use_lifecycle()
usethis::use_lifecycle_badge("experimental")
usethis::use_readme_rmd()
usethis::use_news_md()
usethis::use_version("dev")

usethis::use_build_ignore(
    c(
        "devops",
        "inst/examples",
        "tests"
    )
)

# Prod dependencies -------------------------------------------------------


# Tests -------------------------------------------------------------------

usethis::use_test("package_options")
