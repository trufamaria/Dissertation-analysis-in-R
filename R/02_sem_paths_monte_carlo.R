suppressMessages(library(lavaan))
x <- readRDS("semfits.rds"); grp <- x$grp
set.seed(1)
out <- list()
for(nm in c("f0","f1")){
  f <- x[[nm]]
  pe <- parameterEstimates(f); ss <- standardizedSolution(f)
  V <- vcov(f); th <- coef(f)
  rows <- list()
  for(g in grp){
    for(p in c("a","b","c")){
      lbl <- paste0(p,"_",g); i <- which(pe$label==lbl)[1]
      sidx <- which(ss$op=="~" & ss$group==match(g,grp) & ss$lhs==ifelse(p=="a","Misg","SPV") & ss$rhs==ifelse(p=="b","Misg","RD"))
      rows[[length(rows)+1]] <- data.frame(model=nm,group=g,path=p,B=pe$est[i],SE=pe$se[i],p=pe$pvalue[i],beta=ss$est.std[sidx],pstd=ss$pvalue[sidx])
    }
    # Monte Carlo CI for unstandardized indirect
    na <- paste0("a_",g); nb <- paste0("b_",g)
    ia <- which(names(th)==na)[1]; ib <- which(names(th)==nb)[1]
    # th names may repeat labels across groups; find by label via parameterEstimates ids
    pa <- pe$est[which(pe$label==na)[1]]; pb <- pe$est[which(pe$label==nb)[1]]
    pt <- parTable(f); ida <- pt$free[which(pt$label==na)[1]]; idb <- pt$free[which(pt$label==nb)[1]]
    S <- V[c(ida,idb),c(ida,idb)]; m <- MASS::mvrnorm(20000,c(pa,pb),S); ind <- m[,1]*m[,2]
    ssi <- ss[ss$label==paste0("ind_",g),]
    rows[[length(rows)+1]] <- data.frame(model=nm,group=g,path="ind",B=pa*pb,SE=sd(ind),p=NA,beta=ssi$est.std[1],pstd=ssi$pvalue[1],lo=quantile(ind,.025),hi=quantile(ind,.975))
  }
  out[[nm]] <- do.call(rbind,lapply(rows,function(r){ if(is.null(r$lo)){r$lo<-NA;r$hi<-NA}; r}))
}
r <- do.call(rbind,out); rownames(r) <- NULL
print(r,digits=3); write.csv(r,"sem_results.csv",row.names=FALSE)
# covariate effects in f1
f<-x$f1; pe<-standardizedSolution(f); cv<-pe[pe$op=="~"&pe$rhs%in%c("Pol_Tend","Education","AgeGrp"),]; cv$group<-grp[cv$group]
print(cv[,c("lhs","rhs","group","est.std","pvalue")],digits=3)
write.csv(cv[,c("lhs","rhs","group","est.std","pvalue")],"sem_covs.csv",row.names=FALSE)
