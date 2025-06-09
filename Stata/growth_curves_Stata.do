*******************************************
* Set up
*******************************************

use https://github.com/patroncos/aqrie_workshop_2025/raw/refs/heads/main/data.dta, clear

*******************************************
* Latent Growth Curve Model (LGCM)
*******************************************

* unconditional linear latent growth curve model
sem (t1 <- i@1 s@0 _cons@0) ///
	(t2 <- i@1 s@1 _cons@0) ///
	(t3 <- i@1 s@2 _cons@0) ///
	(t4 <- i@1 s@3 _cons@0), ///
	latent(i s) ///
	means(i s) 
	
est store lgcm1
est save lgcm1, replace

* unconditional non-linear latent growth curve model

sem (t1 <- i@1 s@0 q@0 _cons@0) ///
	(t2 <- i@1 s@1 q@1 _cons@0) ///
	(t3 <- i@1 s@2 q@4 _cons@0) ///
	(t4 <- i@1 s@3 q@9 _cons@0), ///
	latent(i s q) ///
	means(i s q)

est store lgcm2
est save lgcm2, replace

* Compare
est stats lgcm1 lgcm2

* What is the decision?

*******************************************
* LGCM with covariates
*******************************************

* a linear growth model with a time-varying covariate
sem (t1 <- i@1 s@0 _cons@0 c1) ///
    (t2 <- i@1 s@1 _cons@0 c2) ///
    (t3 <- i@1 s@2 _cons@0 c3) ///
    (t4 <- i@1 s@3 _cons@0 c4) ///
    (i <- _cons) ///
    (s <- _cons), ///
	latent(i s) 

est store lgcm3
est save lgcm3, replace

* a linear growth model with a time-varying and two time-invariant covariates
sem (t1 <- i@1 s@0 _cons@0 c1) ///
    (t2 <- i@1 s@1 _cons@0 c2) ///
    (t3 <- i@1 s@2 _cons@0 c3) ///
    (t4 <- i@1 s@3 _cons@0 c4) ///
    (i <- _cons x1 x2) ///
    (s <- _cons x1 x2), ///
	latent(i s) ///
	cov(e.i*e.s) ///
	var(e.t1 e.t2 e.t3 e.t4)
	
est store lgcm4
est save lgcm4, replace

* Compare models
est stats lgcm3 lgcm4 /* this doesn't work properly*/

* Rewrite model 3 with constraints
sem (t1 <- i@1 s@0 _cons@0 c1) ///
    (t2 <- i@1 s@1 _cons@0 c2) ///
    (t3 <- i@1 s@2 _cons@0 c3) ///
    (t4 <- i@1 s@3 _cons@0 c4) ///
    (i <- _cons x1@0 x2@0) ///
    (s <- _cons x1@0 x2@0), ///
	latent(i s) ///
    cov(e.i*e.s)
	
est store lgcm3b
est save lgcm3b, replace

* Compare models (again!)
est stats lgcm3b lgcm4

* What is the decision?

*******************************************
* MLM for change (GCM) 
*******************************************

use https://github.com/patroncos/aqrie_workshop_2025/raw/refs/heads/main/long_data.dta, clear

* GCM1: unconditional linear
mixed outcome t || id: t, cov(unstructured) 

est store gcm1
est save gcm1, replace

* GCM2: add quadratic time
mixed outcome t t_sq || id: t, cov(unstructured)

est store gcm2
est save gcm2, replace

* Compare GCM1 and GCM2
est stats gcm1 gcm2

*What is the conclusion?

*******************************************
* MLM for change (GCM) with covariates
*******************************************

* GCM3: add time-varying covariate
mixed outcome t covar || id: t, cov(unstructured)

est store gcm3
est save gcm3, replace

* GCM4: add time-invariant covariates
mixed outcome t x1 x2 covar || id: t, cov(unstructured)

est store gcm4
est save gcm4, replace

* Compare GCM3 vs GCM4
est stats gcm3 gcm4

*What is the conclusion?

*******************************************
* Compare LGCM and GCM
*******************************************

est use lgcm1 /* gather previously stored LGCM results */
est replay /* replay estimates to see the results */
est store lgcm1 /* store them in the memory to compare with GCM */
est use gcm1 /* gather previously stored GCM results */
est replay /* replay estimates to see the results */
est store gcm1 /* store them in the memory to compare with GCM */
est stats lgcm1 gcm1 /* compare AIC */


*******************************************
* Applying constraints to LGCM for simpler model closer to GCM
*******************************************

/* Get wide data back */

use data, clear

* constrain variances be constant across time

sem (t1 <- i@1 s@0 _cons@0) ///
	(t2 <- i@1 s@1 _cons@0) ///
	(t3 <- i@1 s@2 _cons@0) ///
	(t4 <- i@1 s@3 _cons@0), ///
	latent(i s) ///
	means(i s) ///
	var(e.t1@eqv e.t2@eqv e.t3@eqv e.t4@eqv)
	
est store lgcm1_alt
est save lgcm1_alt, replace

est use gcm1 /* gather previously stored GCM results */
est replay /* replay estimates to see the results */
est store gcm1 /* store them in the memory to compare with GCM */
est stats lgcm1_alt gcm1 /*compare AIC*/

* Models are mathematically equivalent

*******************************************
* Applying constraints to LGCM with covariates
*******************************************

sem (t1 <- i@1 s@0 _cons@0 c1@eqt) ///
	(t2 <- i@1 s@1 _cons@0 c2@eqt) ///
	(t3 <- i@1 s@2 _cons@0 c3@eqt) ///
	(t4 <- i@1 s@3 _cons@0 c4@eqt) ///
    (i <- _cons x1 x2) ///
    (s <- _cons x1@0 x2@0), ///
	latent(i s) ///
	cov(e.i*e.s) ///
	var(e.t1@eqv e.t2@eqv e.t3@eqv e.t4@eqv)
	
est store lgcm4_alt
est save lgcm4_alt, replace

est use gcm4 /* gather previously stored GCM results */
est replay /* replay estimates to see the results */

* Models give nearly the exact same coefficients
* When comparing AIC values in R, they are exactly the same.
* Stata doesn't give the same AIC values for these models.
* I don't know the reason why, apologies!

*******************************************
* Bringing GCM estimates closer to LGCM
*******************************************

* Bring the dataset in long format back

use long_data, clear

mixed outcome t x1 x2 covar ///
c.x1#c.t c.x2#c.t c.covar#c.t || /// /*interactions to get time-specific coefficients*/
id: t, cov(unstructured)

est store gcm5
est save gcm5, replace

* Compare with LGCM

est use lgcm4 /* gather previously stored LGCM results */
est replay /* replay estimates to see the results */

* Models are quite close. This is mostly due to GCM not having time-specific 
* variances. Furthermore, when comparing AIC values in R, this provides evidence
* (albeit weak) of GCM being better. There is a problem with the AIC values in 
* Stata that I haven't been able to figure out yet, apologies again!
