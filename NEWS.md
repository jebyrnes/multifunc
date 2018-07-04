## multifunc 0.8.0 (Release date: 2017-09-25)
* Adding `getMF` and associated functions to incorporate ongoing work on new metrics
* See also `funcEven` and `funcDiv`


## multifunc 0.7.3 (Release date: 2017-04-07)
* Fixed `getCoefTab` to work better with categorical predictors
* `getCoefTab` will now return all coefficients unless asked to filter to one


## multifunc 0.7.2 (Release date: 2017-03-17)
* Fixed ASCII bug
* Fixed and added vignette


## 0.7.1 (Release date: 2017-03-15)
Changed from Depends (from older version of R) to Imports


## 0.6.2
* Changed to semantic versioning. Added catch for users trying to get indices from multiple threshold approach when none of the coefficients of interest are different from 0.


## 0.6-1
* Fixed error in getMaxFuncs that was leading to one additional sample being pulled to calculate max values

## 0.6.0
* Fixed error in getMaxFuncs that was leading to bad row indices and no max being calculated  
