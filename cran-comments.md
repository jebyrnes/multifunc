## Test environments
* local OS X install, R 3.3.0
* ubuntu 12.04 (on travis-ci), R 3.3.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
* getFuncsMaxed: no visible binding for global variable ‘thresholds’
    Undefined global functions or variables:

This is fine - it's due to an anonymous data frame in a plyr call
