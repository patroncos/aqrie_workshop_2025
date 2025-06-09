#-------------------------------------------
# Set up
#-------------------------------------------

library(lavaan)
library(tidyverse)
library(lme4)

data = Demo.growth

#-------------------------------------------
# LGCM
#-------------------------------------------

# unconditional linear latent growth curve model

lgcm1 <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
           s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 '

lgcm1_fit <- growth(lgcm1, data = data)

summary(lgcm1_fit, standardized = T)

# unconditional non-linear latent growth curve model

lgcm2 <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
           s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
           q =~ 0*t1 + 1*t2 + 4*t3 + 9*t4'

lgcm2_fit <- growth(lgcm2, data = data)

summary(lgcm2_fit, standardized = T)

# Compare linear and non-linear models

anova(lgcm1_fit, lgcm2_fit)

# What is the decision?

#-------------------------------------------
# LGCM with covariates
#-------------------------------------------

# a linear growth model with a time-varying covariate

lgcm3 <- '
  # intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
  # time-varying covariates
    t1 ~ c1
    t2 ~ c2
    t3 ~ c3
    t4 ~ c4
'

lgcm3_fit <- growth(lgcm3, data = data)

summary(lgcm3_fit, standardized = T)

# a linear growth model with a time-varying and two time-invariant covariates

lgcm4 <- '
  # intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
  # regressions
    i ~ x1 + x2
    s ~ x1 + x2
  # time-varying covariates
    t1 ~ c1
    t2 ~ c2
    t3 ~ c3
    t4 ~ c4
'

lgcm4_fit <- growth(lgcm4, data = data)

summary(lgcm4_fit, standardized = T)

# compare models

anova(lgcm3_fit, lgcm4_fit) # Why doesn't this work?

# rewrite model 3 with constraints

lgcm3b <- '
  # intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
  # regressions
    i ~ 0*x1 + 0*x2
    s ~ 0*x1 + 0*x2
  # time-varying covariates
    t1 ~ c1
    t2 ~ c2
    t3 ~ c3
    t4 ~ c4
'

lgcm3b_fit <- growth(lgcm3b, data = data)

summary(lgcm3b_fit, standardized = T)

# compare models (again!)

anova(lgcm3b_fit, lgcm4_fit)

# What is the decision?

#-----------------------------------------
# reshape wide to long for MLM
#-----------------------------------------

data <- data %>%
  mutate(id = cur_group_rows())

data_long <- data %>%
  select(id, starts_with("t"), x1, x2) %>%
  pivot_longer(cols = starts_with("t"),
               names_to = "time",
               names_prefix = "t",
               values_to = "outcome") %>%
  left_join(data %>%
              select(id, starts_with("c")) %>%
              pivot_longer(cols = starts_with("c"),
                           names_to = "time",
                           names_prefix = "c",
                           values_to = "covar"), 
            by = c("id", "time")) %>%
  mutate(t = as.numeric(time) - 1)

#--------------------------------------------------------------
# MLM for change (GCM)
#--------------------------------------------------------------

# unconditional linear latent growth curve model

summary(gcm1 <- lmer(outcome ~ 1 + t +
                       (1 + t | id),
                     data = data_long, REML = F))

# unconditional non-linear latent growth curve model

summary(gcm2 <- lmer(outcome ~ 1 + t + I(t^2) +
                       (1 + t | id),
                     data = data_long, REML = F))

# compare models

anova(gcm1, gcm2) # What is the conclusion?

#--------------------------------------------------------------
# MLM for change (GCM) with covariates
#--------------------------------------------------------------

summary(gcm3 <- lmer(outcome ~ 1 + t + covar +
                       (1 + t | id),
                     data = data_long, REML = F))

summary(gcm4 <- lmer(outcome ~ 1 + t + x1 + x2 + covar +
                       (1 + t | id),
                     data = data_long, REML = F))

# compare models

anova(gcm3, gcm4) # What is the conclusion?

#--------------------------------------------------------------
# Compare LGCM and GCM
#--------------------------------------------------------------

AIC(lgcm1_fit)
AIC(gcm1)

AIC(lgcm2_fit)
AIC(gcm2)

#--------------------------------------------------------------
# Applying constraints to LGCM for simpler model closer to GCM
#--------------------------------------------------------------

# constrain variances be constant across time

lgcm1_alt <- '
  # intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
  # constraints for single level-1 variance
    t1 ~~ eqv*t1
    t2 ~~ eqv*t2
    t3 ~~ eqv*t3
    t4 ~~ eqv*t4
'

lgcm1_alt_fit <- growth(lgcm1_alt, data = data)

summary(lgcm1_alt_fit, standardized = T)

# compare with gcm1

summary(gcm1)

AIC(lgcm1_alt_fit)

AIC(gcm1)

# Models are mathematically equivalent

#--------------------------------------------------------------
# Applying constraints to LGCM with covariates
#--------------------------------------------------------------

lgcm4_alt <- '
  # intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
    
  # regressions
    i ~ x1 + x2
    s ~ 0*x1 + 0*x2 # constraints to remove time-specific slopes (0)
    
  # time-varying covariates with equality constraints (eqt)
    t1 ~ eqt*c1
    t2 ~ eqt*c2
    t3 ~ eqt*c3
    t4 ~ eqt*c4
    
  # constraints for single level-1 variance (eqv)
    t1 ~~ eqv*t1
    t2 ~~ eqv*t2
    t3 ~~ eqv*t3
    t4 ~~ eqv*t4
'

lgcm4_alt_fit <- growth(lgcm4_alt, data = data)

summary(lgcm4_alt_fit, standardized = T)

# compare with gcm4

summary(gcm4)

AIC(lgcm4_alt_fit)

AIC(gcm4)

# Models are mathematically equivalent

#--------------------------------------------------------------
# Bringing GCM estimates closer to LGCM
#--------------------------------------------------------------

summary(gcm5 <- lmer(outcome ~ 1 + t + t*x1 + t*x2 + t*covar +
                       (1 + t | id),
                     data = data_long, REML = F))


# compare with lgcm4

summary(lgcm4_fit, standardized = T)

AIC(lgcm4_fit)

AIC(gcm5)

# Models are quite close and even AIC comparison provides evidence
# (albeit weak) of GCM being better