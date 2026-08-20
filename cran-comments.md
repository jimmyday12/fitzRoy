## Resubmission

This release fixes the ERROR shown on
<https://cran.r-project.org/web/checks/check_results_fitzRoy.html> for
r-devel-linux-x86_64-fedora-gcc, as requested by the CRAN team on 2026-08-14.

The cause was test code, not package code. `tests/testthat/test-fetch-player-stats.R`
computed a variable at the *top level* of the test file by calling
a function that required a network connection. Top-level code in a test file runs before the
`testthat::skip_on_cran()` and `testthat::skip_if_offline()` guards inside the
`test_that()` blocks, so the internet resource was contacted unconditionally on
check machines without network access, and the file failed with
"cannot open the connection".

The variable is now resolved lazily from inside the tests, after the skip guards,
so no network access occurs on CRAN. All tests in the package are now correctly
guarded by `skip_on_cran()`/`skip_if_offline()`.

In addition, and in line with the policy quoted in your message,
`fetch_results_afltables()` now fails gracefully with an informative message
naming the unavailable resource rather than propagating readr's bare
"cannot open the connection" error.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Local macOS (aarch64-apple-darwin20), R 4.2
* GitHub Actions: macOS (latest), R release
* GitHub Actions: Windows (latest), R release
* GitHub Actions: Ubuntu (latest), R devel
* GitHub Actions: Ubuntu (latest), R release
* GitHub Actions: Ubuntu (latest), R oldrel-1

## Downstream Dependencies

There are currently no downstream dependencies for this package
