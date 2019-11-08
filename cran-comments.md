## Resubmission
Re-submitting after a failed initial submission.

The initial submission failed due to an automated test not passing. The specific failure message is below.
* checking package dependencies ... ERROR
  Package required but not available: 'readr'

  I have been unable to replicate this issue using any of the following. 
  `devtools::check()`
  `devtools::check_win_develop()`
` rhub::check_for_cran()`

  All of these pass fine with no errors. I have been advised to resubmit. 

## Test environments
* local OS X install, R 3.5.3
* ubuntu 14.04 (on travis-ci), R 3.5.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

There was 1 NOTE:
* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'James Day <jamesthomasday@gmail.com>'

  New submission
  
  This is my initial submission of the package
