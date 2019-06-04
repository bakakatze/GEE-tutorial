require(geepack)

# GEE tutorial using respiratory data
data(respiratory)

# Use exchangeable correlation structure as baseline comparison
t_mex = geeglm(outcome ~ baseline + center + sex + treat + age + I(age^2), data = respiratory, id = interaction(center, id),
               family = binomial, corstr = "exchangeable")


# set up correlation structure (unstructured)
t_zcor = genZcor(clusz = c(xtabs(~id + center, data = respiratory)), waves = respiratory$visit, corstrv = 4) # 4 = unstructured

# set up Toeplitz working correlation structure
t_toep = matrix(NA, nrow(t_zcor), 3)
t_toep[,1] = apply(t_zcor[,c(1,4,6)], 1, sum)
t_toep[,2] = apply(t_zcor[,c(2,5)], 1, sum)
t_toep[,3] = t_zcor[,3]

# Let's use the Toeplitz working correlation to the GEE model
t_mtoep = geeglm(outcome ~ baseline + center + sex + treat + age + I(age^2), data = respiratory, id = interaction(center, id),
                 family = binomial, corstr = "userdefined", zcor = t_toep)

summary(t_mtoep)

#
## let's see if we need the quadratic age term or not
t_mex0 = update(t_mex, . ~ . - age - I(age^2))

anova(t_mex, t_mex0) # it is significant
