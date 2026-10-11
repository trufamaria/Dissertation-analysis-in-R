suppressMessages(library(lavaan))
d <- read.csv("../data/items.csv"); d$Country <- factor(d$Country,levels=c("Brazil","US"))
# first appearance order is US first in data; keep explicit order for labelling
mm <- '
RD =~ RD1+RD2+RD3+RD4+RD5
Misg =~ Misg1+Misg2+Misg3+Misg4+Misg5+Misg6+Misg7+Misg8+Misg9+Misg10
SPV =~ SPV1+SPV2+SPV3+SPV4
Chaos =~ Chaos_1+Chaos_2+Chaos_3+Chaos_4+Chaos_5+Chaos_6+Chaos_7
SDO =~ SDO_1+SDO2+SDO3+SDOD4
AT =~ AT1+AT2+AT3+AT4
Racism =~ Racism_3+Racism_4+Racism_5+Racism_6
Ppl =~ PPl2+Ppl3_+Ppl4+Ppl5
Trust =~ Trust1+Trust2+Trust3
PSE =~ PSE1+PSE2
Belief =~ Belief1+Belief2+Belief3+Belief4
Share =~ Share1+Share2+Share3+Share4
RD1 ~~ RD2
Misg2 ~~ Misg3
Misg7 ~~ Misg8
SPV3 ~~ SPV4
Belief1 ~~ Share1
Belief2 ~~ Share2
Belief3 ~~ Share3
Belief4 ~~ Share4
'
pred <- c("RD","Misg","SPV","WEPV","Chaos","SDO","AT","Racism","Ppl","Trust","PSE","Belief","Share")
model <- paste0(mm,"\nPol_Tend ~ ",paste(pred,collapse=" + "),"\n")
t0 <- Sys.time()
f_all <- tryCatch(sem(model,data=d,estimator="MLR",missing="fiml",fixed.x=FALSE),error=function(e) {print(e);NULL})
cat("pooled time:",format(Sys.time()-t0),"\n")
if(!is.null(f_all)){ cat("converged:",lavInspect(f_all,"converged"),"\n"); print(round(fitMeasures(f_all,c("cfi.robust","tli.robust","rmsea.robust","srmr","chisq.scaled","df")),3)) }
saveRDS(f_all,"allsem_pooled.rds")
