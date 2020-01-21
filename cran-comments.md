This is a new submission for a package that had been archived on CRAN for a policy violation. 

Specifically, the rebuilding of vignettes was failing due to being reliant on Internet-based resources.

This release attempts to resolve issues by removing all internet based resources from vignettes, tests and examples which now all use internal package data  where available and are skipped when no-internet is detected or selectively skipped on CRAN. 

The package has passed all testing locally, on `rhub` and `travis-ci` using the following,  

`devtools::check()`
`devtools::check_win_develop()`
`rhub::check_for_cran()`


## Test environments
* local R installation, R 3.5.3
* ubuntu 16.04 (on travis-ci), R 3.5.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

There was 1 note regarding this being a new submission - as indicated, this is actually a re-submission of an archived package. Full note below. 

  New submission
  
  Package was archived on CRAN
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2020-01-09 for policy violation.
  
    Repeated problems with Internet-based resources.



