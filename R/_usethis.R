#  Add  License
usethis::use_gpl3_license()

# Add README
usethis::use_readme_rmd() # Add a README.Rmd
devtools::build_readme() # Render README.RMD to generate README.md

# add dependency
usethis::use_package("data.table")

# create r scripts
usethis::use_r("_usethis")

# hide script that deploys from package
usethis::use_build_ignore("R/_usethis.R")

# add roxygen2 to convert markdown to Rd files
usethis::use_roxygen_md()

# add continuous integration to
# github_actions {usethis}	R Documentation:
#   - Run R CMD check on various operating systems and R versions
#   - Build and deploy a pkgdown site
#   - Determine test coverage

usethis::use_github_actions()

# add package-level documentation
usethis::use_package_doc()

# setup package documentation as website
usethis::use_pkgdown()
usethis::use_article("FOCI",title = "FOCI")
usethis::use_article("FOCI",title = "FOCI")
usethis::use_article("CODEC",title = "CODEC")

# setup package documentation as website
pkgdown::build_site()


# setup spell checking
usethis::use_spell_check()
devtools::check()

library(rcmdcheck)
devtools::build()
rcmdcheck::rcmdcheck("../FOCI_0.1.3.tar.gz")

# Use a specific GitHub Actions workflow
usethis::use_github_action("pkgdown")
