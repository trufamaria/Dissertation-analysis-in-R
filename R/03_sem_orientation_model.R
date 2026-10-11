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

mk <- function(){
  lab <- function(p) paste0("c(",paste0(p,"_",grp,collapse=","),")")
  s <- paste0(meas,"\n",
   "Misg ~ ",lab("a"),"*RD + Pol_Tend\n",
   "SPV ~ ",lab("b"),"*Misg + ",lab("c"),"*RD + Pol_Tend\n",
   "RD ~ Pol_Tend\n")
  paste0(s,paste0("ind_",grp," := a_",grp,"*b_",grp,collapse="\n"),"\n")
}
f2 <- sem(mk(),data=d,group="Group",group.equal="loadings",estimator="MLR",missing="fiml",fixed.x=FALSE)
print(round(fitMeasures(f2,c("cfi.robust","tli.robust","rmsea.robust","srmr","chisq.scaled","df")),3))
print(table(d$Group)); print(sum(is.na(d$Pol_Tend)))
pe <- parameterEstimates(f2); ss <- standardizedSolution(f2); V <- vcov(f2); pt <- parTable(f2)
set.seed(1); rows <- list()
for(g in grp){
  for(p in c("a","b","c")){
    lbl <- paste0(p,"_",g); i <- which(pe$label==lbl)[1]
    sidx <- which(ss$op=="~" & ss$group==match(g,grp) & ss$lhs==ifelse(p=="a","Misg","SPV") & ss$rhs==ifelse(p=="b","Misg","RD"))
    rows[[length(rows)+1]] <- data.frame(model="f2",group=g,path=p,B=pe$est[i],SE=pe$se[i],p=pe$pvalue[i],beta=ss$est.std[sidx],pstd=ss$pvalue[sidx],lo=NA,hi=NA)
  }
  na <- paste0("a_",g); nb <- paste0("b_",g)
  pa <- pe$est[which(pe$label==na)[1]]; pb <- pe$est[which(pe$label==nb)[1]]
  ida <- pt$free[which(pt$label==na)[1]]; idb <- pt$free[which(pt$label==nb)[1]]
  S <- V[c(ida,idb),c(ida,idb)]; m <- MASS::mvrnorm(20000,c(pa,pb),S); ind <- m[,1]*m[,2]
  ssi <- ss[ss$label==paste0("ind_",g),]
  rows[[length(rows)+1]] <- data.frame(model="f2",group=g,path="ind",B=pa*pb,SE=sd(ind),p=NA,beta=ssi$est.std[1],pstd=ssi$pvalue[1],lo=quantile(ind,.025),hi=quantile(ind,.975))
}
r <- do.call(rbind,rows); rownames(r) <- NULL; print(r,digits=3); write.csv(r,"sem_results_pol.csv",row.names=FALSE)
cv <- ss[ss$op=="~" & ss$rhs=="Pol_Tend",]; cv$group <- grp[cv$group]
print(cv[,c("lhs","rhs","group","est.std","pvalue")],digits=3); write.csv(cv[,c("lhs","rhs","group","est.std","pvalue")],"sem_covs_pol.csv",row.names=FALSE)
saveRDS(list(f2=f2,grp=grp),"semfits_pol.rds")
