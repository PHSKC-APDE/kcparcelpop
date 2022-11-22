library(data.table)
# library('mapview')
# library('ggplot2')
# library('rstanarm')
options(tigris_use_cache = TRUE)
tracts = tigris::tracts(state = 'WA', county = '033', cb = TRUE, year = 2020)
tracts = tracts[, c('GEOID', 'ALAND', 'AWATER')]

# beds_per_parcel
load('data/beds_per_parcel.rda')

# Population
version = 'v3'
popdir = file.path('//dphcifs/APDE-CDIP/Frankenpop', version ,'todoh')
pop = fread(file.path(popdir, "blk_2022.csv.gz"))
pop = pop[substr(CensusBlockCode2020,1,5) == 53033]
pop[, tract := substr(CensusBlockCode2020,1,11)]

# Aggregate to the tract level
# TODO: What to do about UW?
tpop = pop[, .(pop = sum(Population)), tract]
tbeds = beds_per_parcel[, lapply(.SD, sum, na.rm = T), by = .(tract = GEOID), .SDcols = c('apt_beds', 'res_beds', 'condo_beds', 'tot_beds')]

# merge
tdat = merge(tbeds, tpop, by = 'tract')

# Clean up some integer weirdness
st = tdat[, sum(pop)]
tdat[, tdat[, pop := as.integer(round(pop))]]
end = tdat[, sum(pop)] - st
print(end)

# Map the tracts by pop/tot_beds
tdat[, ppb := pop/tot_beds]

tracts = merge(tracts, tdat[, .(GEOID = tract, ppb, pop, tot_beds)], all.x = T, by = 'GEOID')

# mapview(tracts[, c('GEOID', 'ppb', 'pop', 'tot_beds')], zcol = 'ppb')

# simple models; one poisson one glm
m1.1 = glm(pop ~ apt_beds + res_beds + condo_beds, family = poisson, data = tdat)
m1.2 = lm(pop~ apt_beds + res_beds + condo_beds, data = tdat)
m1.3 = glm(pop ~ 0 + apt_beds + res_beds + condo_beds, family = poisson, data = tdat)
m1.4 = lm(pop~ 0 + apt_beds + res_beds + condo_beds, data = tdat)
# random effect on tract
# Getting effective sample size errors for 2.1 and 2.2 blows up. Probably because I'm trying to have a random effect per data point
# m2.1 = stan_glmer(pop ~ apt_beds + res_beds + condo_beds + (1|tract), family = poisson, data = tdat)
# m2.2 = stan_lmer(pop ~ apt_beds + res_beds + condo_beds + (1|tract), data = tdat)


# Estimate parcel pop
# Subtract the intercept which is maybe "accounting" for weirdness in the parcel data (e.g frats don't have pop). ideally, it'd be 0
make_preds = function(m, X){
  
  X = X[, .SD, .SDcols = intersect(names(X), names(coef(m)))]
  
  if(inherits(m, 'glm')){
    res = exp(predict(m, X)) - exp(predict(m, X[1,] *0))
  }else{
    res = predict(m, X) - predict(m, X[1,] *0)
  }
  
  res

}
beds_per_parcel[, pred1.1 := make_preds(m1.1, beds_per_parcel)]
beds_per_parcel[, pred1.2 := make_preds(m1.2, beds_per_parcel)]
beds_per_parcel[, pred1.3 := make_preds(m1.3, beds_per_parcel)] # this one seems wonky as hell
beds_per_parcel[, pred1.4 := make_preds(m1.4, beds_per_parcel)]

# The two glm models seem wonky. I guess in log scale the intercept is more important for determining position on the log scale
smol = which.min(abs(pop[, sum(Population)] - unlist(beds_per_parcel[, lapply(.SD, sum), .SDcols = patterns('pred1')])))

# Take the closest to the true population
beds_per_parcel[, selected := get(names(smol))]

# scale to match tract pop
beds_per_parcel[, parcel_tpop := sum(selected), by = GEOID]
beds_per_parcel = merge(beds_per_parcel, tpop, all.x = T, by.x = 'GEOID', by.y = 'tract')

# some new construction (I assume) doesn't have a geolocation.
# Drop these rows
beds_per_parcel = beds_per_parcel[!is.na(GEOID)]

beds_per_parcel[, parcel_pop := selected * pop/parcel_tpop]

# clean up
beds_per_parcel = beds_per_parcel[, .(GEOID, PIN, address, apt_beds, condo_beds, tot_beds, pop = parcel_pop)]

# add centriods
load('data/pcoords.rda')

parcel_pop = merge(pcoords, beds_per_parcel, all.x = T, by = 'PIN')

usethis::use_data(parcel_pop, overwrite = TRUE, version = 3)

# 
# scls = lapply(tdat[, .SD, .SDcols = c('apt_beds', 'res_beds', 'condo_beds')], scale)
# prams = lapply(scls, function(x) attributes(x)[2:3])
# 
# tdat[, c('apt_scl', 'res_scl', 'condo_scl') := scls]
# 
# # A simple model
# 
# m2.2 = lm(pop~ apt_scl + res_scl + condo_scl, data = tdat)
# m2.3 = lm(pop~ 0 + apt_beds + res_beds + condo_beds, data = tdat)
# 
# # Doesn't really vary much from m1
# m3 = stan_glm(pop ~ apt_beds + res_beds + condo_beds, family = poisson, data = tdat)
# 
# m4 = xgboost::xgboost(data = as.matrix(tdat[, .(apt_beds, res_beds, condo_beds)]),
#              label = tdat[, pop],
#              nrounds = 4000,
#              verbose = FALSE
#              # params = list(booster = 'gblinear')
#              )
# 
# blah = predict(m4, as.matrix(tdat[, .(apt_beds, res_beds, condo_beds)]))
# blah2 = predict(m4, as.matrix(tdat[, .(apt_beds, res_beds, condo_beds)]))
# 
# blah2.1 = predict(m4, as.matrix(beds_per_parcel[, .(apt_beds, res_beds, condo_beds)]))
# blah2.2 = predict(m4, as.matrix(beds_per_parcel[, .(apt_beds=0, res_beds=0, condo_beds=0)]))
# blahp2 = blah2.1-blah2.2
# 
# # Does not pass ESS muster
# # m5 = stan_glmer(pop ~ apt_beds + res_beds + condo_beds + (1|tract), family = poisson, data = tdat)
# 
# #superlearner for kicks
# sl_lib = c('SL.xgboost', 'SL.randomForest', 'SL.lm',
#            'SL.earth', 'SL.svm', 'SL.mean')
# # Gaussian
# sl = SuperLearner(tdat[, pop], 
#                   as.data.frame(tdat[, .(apt_beds, res_beds, condo_beds)]), family = 'gaussian', SL.library = sl_lib)
# sl2 = SuperLearner(log(tdat[, pop]), as.data.frame(tdat[, .(apt_beds, res_beds, condo_beds)]), family = 'gaussian', SL.library = sl_lib)
# 
# tdat[, base_glm := exp(predict(m1, tdat))]
# tdat[, rstanarm := exp(predict(m3, tdat))]
# tdat[, sl_gauss := sl$SL.predict[,1]]
# tdat[, sl_log := exp(sl2$SL.predict[,1])]
# rmse = function(obs, pred) sqrt(mean((obs-pred)^2))
# # In sample rmse for fun
# tdat[, .(rmse(pop, base_glm), rmse(pop, rstanarm), 
#          rmse(pop, sl_gauss), rmse(pop, sl_log))]
# 
# # preds = predict(sl, beds_per_parcel)
# 
# bpp = beds_per_parcel[, .(PIN, apt_beds, res_beds, condo_beds)]
# bpp[, grp := floor(.I %/% 10000)]
# bpp_pred = split(bpp, by = 'grp' )
# bpp_pred = lapply( bpp_pred, function(x) predict(sl,x[,.(apt_beds, res_beds, condo_beds)]))
# bpp_pred = lapply(bpp_pred, as.data.table)
# bpp_pred = rbindlist(bpp_pred)
# 
# bpp[, sl := bpp_pred[,pred.V1]]
# bpp[, glm := exp(predict(m1, bpp))]
# if(FALSE){
# # icar model with brm
# # load tracts
# tracts$hasdata = tracts$GEOID %in% tdat$tract
# 
# # TODO: Does sorting matter?
# 
# neigh = spdep::poly2nb(tracts,queen = TRUE)
# nmat = matrix(0, nrow = length(neigh), ncol = length(neigh) )
# for(i in seq_along(neigh)){
#   n = neigh[[i]]
#   nmat[i,n] <- 1
# }
# rownames(nmat) <- tracts$GEOID
# colnames(nmat) <- tracts$GEOID
# 
# tdat[, c('apt_scl', 'res_scl', 'condo_scl') := list(scale(apt_beds), scale(res_beds), scale(condo_beds))]
# 
# # Set some priors
# # lower bound of 0, upper bound of log(5)
# # prior_b = prior(normal(log(1),1/.5), class = 'b')
# priors = get_prior(pop ~ apt_scl + res_scl + condo_scl,
#                   family = poisson(), data = tdat,
#                   data2 = list(nm = nmat[tdat[,tract],tdat[,tract]]))
# priors$prior[2:4] <- 'normal(.3,.3)'
# m2 = brm(pop ~ apt_scl + res_scl + condo_scl, #apt_beds + res_beds + condo_beds
#          family = poisson(), data = tdat,
#          prior = priors,
#          init = 0)
# 
# tdat[, brmssimple := predict(m2, tdat)[,1]]
# 
# priors = get_prior(pop ~ apt_scl + res_scl + condo_scl + (1|tract),
#                    family = poisson(), data = tdat,
#                    data2 = list(nm = nmat[tdat[,tract],tdat[,tract]]))
# priors$prior[2:4] <- 'normal(.3,.3)'
# m4 = brm(pop ~ apt_scl + res_scl + condo_scl + (1|tract), #apt_beds + res_beds + condo_beds
#          family = poisson(), data = tdat,
#          data2 = list(nm = nmat[tdat[,tract],tdat[,tract]]),
#          prior = priors,
#          init = 0)
# 
# # m4 = brm(pop ~ apt_scl + res_scl + condo_scl + car(nm, gr = tract,type = 'icar'), #apt_beds + res_beds + condo_beds
# #          family = poisson(), data = tdat,
# #          data2 = list(nm = nmat[tdat[,tract],tdat[,tract]]),
# #          prior = priors,
# #          init = 0)
# # m4 = brm(pop ~ apt_scl + res_scl + condo_scl + car(nm, gr = tract,type = 'icar'),
# #          family = poisson, data = tdat,
# #          data2 = list(nm = nmat[tdat[,tract],tdat[,tract]]))
# }