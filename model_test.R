

crime$logprbarr = log(crime$prbarr)
crime$logprbconv = log(crime$prbconv)
crime$logprbpris = log(crime$prbpris)
crime$logavgsen = log(crime$avgsen)
crime$logpolpc = log(crime$polpc)
crime$logdensity = log(crime$density)
crime$logtaxpc = log(crime$taxpc)
crime$logpctmin80 = log(crime$pctmin80)
crime$logwcon = log(crime$wcon)
crime$logwtuc = log(crime$wtuc)
crime$logwtrd = log(crime$wtrd)
crime$logwfir = log(crime$wfir)
crime$logwfir = log(crime$wfir)
crime$logwser = log(crime$wser)
crime$logwmfg = log(crime$wmfg)
crime$logwfed = log(crime$wfed)
crime$logwsta = log(crime$wsta)
crime$logwloc = log(crime$wloc)
crime$logmix = log(crime$mix)
crime$logpctymle = log(crime$pctymle)


cn = colnames(crime)
cnlen = length(cn)-1

#cnlen = 20

results = data.frame()

for (i in 5:(cnlen-2)) {
  inner_results = data.frame()
  for (j in (i+1):(cnlen-3)) {
    print(paste(i, j, Sys.time()))
    for(k in (j+1):(cnlen - 4)) {
      for (l in (k+1):(cnlen - 5)) {
        #for (m in (l+1):cnlen) {
        mod = lm(
          as.formula(
            paste(
              "logcrmrte ~", 
              paste(cn[c(i,j,k,l)], collapse = " + ")
              
            )
          )
          , data = crime)  
        inner_results = rbind(inner_results, data.frame(terms=paste("logcrmrte ~ ", cn[c(i,j,k,l)], collapse = " + "), 
                                                        aic=AIC(mod), 
                                                        rsquared=summary(mod)[8],
                                                        adjrsquared=summary(mod)[9]
        )
        )
        #}
      }
    }
  }
  results = rbind(results, inner_results)
}

save(results, file="results4varlogcrmrte.rdata")

results$aic = as.numeric(results$aic)

sortedResults = results[order(results$aic), ]
head(sortedResults, 20)





load('results4var.rdata')
res4vars = results

load('results5var.rdata')
res5vars = results

allres = rbind(res4vars, res5vars)

summary(allres)
allSortedResults = allres[order(allres$aic), ]
head(allSortedResults, 20)



model = lm(logcrmrte ~ prbarr + prbconv + polpc + pctmin80 + logdensity, data = crime)
model$coefficients
summary(model)
AIC(model)

