suppressMessages({library(lavaan);library(haven)})
d <- read_sav("../data/Merged_BR_US_Updated.sav")
d <- d[nchar(as.character(d$Age)) <= 3, ]
d <- d[d$Gender %in% c(1,2), ]
d <- as.data.frame(zap_labels(d))
d$Country <- ifelse(d$Is_Br==1,"US","Brazil")
d$G <- ifelse(d$Gender==1,"Men","Women")
d$Group <- paste(d$Country,d$G,sep="_")
d$AgeGrp <- as.numeric(cut(d$Age,c(17,24,34,44,54,64,200),labels=FALSE))
cat("N =",nrow(d),"\n"); print(table(d$Group))
meas <- '
RD   =~ RD1+RD2+RD3+RD4+RD5
Misg =~ Misg1+Misg2+Misg3+Misg4+Misg5+Misg6+Misg7+Misg8+Misg9+Misg10
SPV  =~ SPV1+SPV2+SPV3+SPV4
RD1~~RD2
Misg2~~Misg3
Misg7~~Misg8
SPV3~~SPV4
'
tmp <- cfa(meas,data=d,group="Group",estimator="MLR",missing="fiml")
grp <- lavInspect(tmp,"group.label"); print(grp)
mk <- function(ctrl){
  lab <- function(p) paste0("c(",paste0(p,"_",grp,collapse=","),")")
  s <- paste0(meas,"\n",
   "Misg ~ ",lab("a"),"*RD",if(ctrl) " + Pol_Tend + Education + AgeGrp","\n",
   "SPV ~ ",lab("b"),"*Misg + ",lab("c"),"*RD",if(ctrl) " + Pol_Tend + Education + AgeGrp","\n")
  if(ctrl) s <- paste0(s,"RD ~ Pol_Tend + Education + AgeGrp\n")
  paste0(s,paste0("ind_",grp," := a_",grp,"*b_",grp,collapse="\n"),"\n")
}
fit <- function(ctrl) sem(mk(ctrl),data=d,group="Group",group.equal="loadings",estimator="MLR",missing="fiml",fixed.x=!ctrl)
f0 <- fit(FALSE); f1 <- fit(TRUE)
for(nm in c("f0","f1")){ f<-get(nm)
  cat("\n=====",nm,"\n"); print(round(fitMeasures(f,c("cfi.robust","tli.robust","rmsea.robust","srmr","chisq.scaled","df")),3))
  pe <- standardizedSolution(f); pe <- pe[pe$op=="~" & pe$lhs %in% c("Misg","SPV") & pe$rhs %in% c("RD","Misg"),c("lhs","rhs","group","est.std","pvalue")]
  pe$group <- grp[pe$group]; print(pe,digits=3)
  print(round(inspect(f,"r2")[[1]],3))
}
saveRDS(list(f0=f0,f1=f1,grp=grp),"semfits.rds")
