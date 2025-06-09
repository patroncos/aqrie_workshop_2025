*******************************************
* Set up
*******************************************

use https://github.com/patroncos/aqrie_workshop_2025/raw/refs/heads/main/data.dta, clear

*******************************************
* reshape wide to long
*******************************************

gen id = _n /* create a unique id*/

reshape long t c, i(id) j(time) /* reshape wide to long*/

*We rename the variable t as outcome to avoid confusion with t for time
rename t outcome
rename c covar

/* for MLM, it is better to have time centred around a value */
/* this is usually the first occasion, so time 1 = 0 */

gen t = time - 1

* Create polynomial term
gen t_sq = t^2 /* we'll use this later */

drop time /* optional, but this is to avoid having redundant variables */

/* I would recommend saving the dataset at this point! */

save long_data

*******************************************
* summary of descriptives
*******************************************

tabstat outcome covar, by(t) stats(mean sd)

*******************************************
* plot trends over time
*******************************************

twoway connected outcome t, name(plot_outcome)

twoway connected covar t, name(plot_covar)

collapse (mean) outcome covar, by(t)

line outcome t || line covar t
