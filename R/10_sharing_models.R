suppressMessages(library(lavaan))
d <- read.csv("../data/items.csv"); d$Country <- factor(d$Country,levels=c("Brazil","US"))
fit <- function(m,data,group=NULL,ge=NULL) tryCatch(suppressWarnings(sem(m,data=data,group=group,group.equal=ge,estimator="MLR",missing="fiml",fixed.x=FALSE)),error=function(e){print(e);NULL})
rows <- list()
# (a) per-post slopes
for(i in 1:4){
  it <- paste0("Share",i)
  for(s in c("All","Brazil","US")){
    dd <- if(s=="All") d else d[d$Country==s,]
    f <- fit(paste0(it," ~ Pol_Tend"),dd)
    pe <- parameterEstimates(f); ss <- standardizedSolution(f)
    r <- which(pe$op=="~")[1]
    rows[[length(rows)+1]] <- data.frame(part="post",item=it,sample=s,n=nrow(dd),B=pe$est[r],SE=pe$se[r],p=pe$pvalue[r],beta=ss$est.std[ss$op=="~"][1],r2=lavInspect(f,"rsquare")[it])
  }
  f2 <- fit(paste0(it," ~ c(sB,sU)*Pol_Tend\n diffUS := sU - sB"),d,"Country")
  pe <- parameterEstimates(f2); r <- pe[pe$label=="diffUS",]
  rows[[length(rows)+1]] <- data.frame(part="postdiff",item=it,sample="diffUS",n=NA,B=r$est,SE=r$se,p=r$pvalue,beta=NA,r2=NA)
}
# (b) latent share ~ Pol + Belief
mm <- '
Belief =~ Belief1+Belief2+Belief3+Belief4
Share =~ Share1+Share2+Share3+Share4
Belief1 ~~ Share1
Belief2 ~~ Share2
Belief3 ~~ Share3
Belief4 ~~ Share4
'
m1 <- paste0(mm,"Share ~ Pol_Tend + Belief\nBelief ~ Pol_Tend\n")
for(s in c("All","Brazil","US")){
  dd <- if(s=="All") d else d[d$Country==s,]
  f <- fit(m1,dd); pe <- parameterEstimates(f); ss <- standardizedSolution(f)
  for(rh in c("Pol_Tend","Belief")){
    r <- which(pe$op=="~"&pe$lhs=="Share"&pe$rhs==rh); q <- which(ss$op=="~"&ss$lhs=="Share"&ss$rhs==rh)
    rows[[length(rows)+1]] <- data.frame(part="latent",item=paste0("Share~",rh),sample=s,n=nrow(dd),B=pe$est[r],SE=pe$se[r],p=pe$pvalue[r],beta=ss$est.std[q],r2=lavInspect(f,"rsquare")["Share"])
  }
  r <- which(pe$op=="~"&pe$lhs=="Belief"&pe$rhs=="Pol_Tend"); q <- which(ss$op=="~"&ss$lhs=="Belief")
  rows[[length(rows)+1]] <- data.frame(part="latent",item="Belief~Pol_Tend",sample=s,n=nrow(dd),B=pe$est[r],SE=pe$se[r],p=pe$pvalue[r],beta=ss$est.std[q],r2=lavInspect(f,"rsquare")["Belief"])
  fm <- fitMeasures(f,c("cfi.robust","rmsea.robust","srmr")); cat(s,round(fm,3),"\n")
  cat(s,"belief-share latent cor:",round(lavInspect(f,"cor.lv")["Belief","Share"],2),"\n")
}
m2 <- paste0(mm,"Share ~ c(pB,pU)*Pol_Tend + c(bB,bU)*Belief\nBelief ~ Pol_Tend\ndP := pU - pB\ndBel := bU - bB\n")
f2 <- fit(m2,d,"Country","loadings"); pe <- parameterEstimates(f2)
cat(lavInspect(f2,"group.label"),"\n"); print(pe[pe$label%in%c("dP","dBel","pB","pU","bB","bU"),c("label","est","se","pvalue")])
for(l in c("dP","dBel")){r<-pe[pe$label==l,]; rows[[length(rows)+1]] <- data.frame(part="latentdiff",item=l,sample="diffUS",n=NA,B=r$est,SE=r$se,p=r$pvalue,beta=NA,r2=NA)}
R <- do.call(rbind,rows); rownames(R) <- NULL; write.csv(R,"share_sem.csv",row.names=FALSE); print(R,digits=3)
