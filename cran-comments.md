## Resubmission
Re-submitting in response to email from Uwe Ligges regarding the length testing run time. Specifically,

  "* checking tests ...
  ** running tests for arch 'i386' ... [10m] OK
  ** running tests for arch 'x64' ... [11m] OK

  This is together 21 min, but the overall threshold for a CRAN package is
  10 min check time."

  Most tests required downloading data from an API and so I have included testhtat::skip_on_cran() on all tests that require this to avoid running them on CRAN servers. Testing this locally reduces the test time from ~3mins to 1.7s.
  
  
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
