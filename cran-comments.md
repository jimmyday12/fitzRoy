## Test environments
local OS X install, R 4.2.2
Ubuntu Linux 20.04.1 LTS (on R-hub), R 4.1.2
Fedora Linux (on R-hub) R-devel
win-builder (devel and release)


## R CMD check results

There were no ERRORs or WARNINGs. 

There was 2 NOTES:

* `checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'`
  
  This note is only seen on Windows Server 2022, R-devel, 64 bit. As noted in [R-Hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.
  

* `checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found`

  This note is only seen on Fedora Linux. HTML validation works fine locally, on Ubuntu and Windows. Can likely be ignored, as discussed [here](https://groups.google.com/g/r-sig-mac/c/7u_ivEj4zhM?pli=1).

## Downstream Dependancies
There are currently no downstream dependencies for this package


