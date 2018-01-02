library(usethis)
create_package()

## Create R file and Test
# Create R file
use_r("foo")
#> ● Edit 'R/foo.R'

# Create Test
# use_test("foo") will set up all the infrastructure you need for unit testing, 
# and create/edit a test file in tests/teststhat/
use_test("foo")

## Dependancies
use_package("dplyr")

## Documentation
use_roxygen_md() # sets up roxygen2 and enables markdown mode so you can use markdown in your roxygen2 comment blocks.
use_package_doc() #creates a skeleton documentation file for the complete package, taking the advantage of the latest roxygen2 features to minimise duplication between the DESCRIPTION and the documentation.
use_readme_rmd() # creates a README.Rmd: use this to describe what your package does and why people should care about it.
use_news_md() # creates a basic NEWS.md for you to record changes.
use_vignette("vignette-name")

## License
use_mit_license()

## Use Git
use_git()
use_github()
# git remote add origin git@github.com:hadley/r-pkgs.git
