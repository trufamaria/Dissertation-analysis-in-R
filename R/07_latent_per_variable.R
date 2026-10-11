suppressMessages(library(lavaan))
d <- read.csv("../data/items.csv"); d$Country <- factor(d$Country,levels=c("Brazil","US"))
sc <- list(
 RD=c("RD1","RD2","RD3","RD4","RD5"), Misg=paste0("Misg",1:10), SPV=paste0("SPV",1:4), WEPV="WEPV",
 Chaos=paste0("Chaos_",1:7), SDO=c("SDO_1","SDO2","SDO3","SDOD4"), SDOE=c("SDO_1","SDO2"), SDOD=c("SDO3","SDOD4"),
 AT=paste0("AT",1:4), Racism=paste0("Racism_",3:6), Ppl=c("PPl2","Ppl3_","Ppl4","Ppl5"),
 Trust=paste0("Trust",1:3), PSE=c("PSE1","PSE2"), Belief=paste0("Belief",1:4), Share=paste0("Share",1:4))
extra <- list(RD="RD1 ~~ RD2", Misg=c("Misg2 ~~ Misg3","Misg7 ~~ Misg8"), SPV="SPV3 ~~ SPV4")
mdl <- function(k, slope="b*Pol_Tend"){
  it <- sc[[k]]
  if(length(it)==1) return(paste0(it," ~ ",slope))
  paste(c(paste0("F =~ ",paste(it,collapse=" + ")), extra[[k]], paste0("F ~ ",slope)),collapse="\n")
}
fitf <- function(m,data,group=NULL,ge=NULL) tryCatch(suppressWarnings(sem(m,data=data,group=group,group.equal=ge,estimator="MLR",missing="fiml",fixed.x=FALSE)),error=function(e) NULL)
slope <- function(f,k,sample,n){
  if(is.null(f)||!lavInspect(f,"converged")) return(data.frame(scale=k,sample=sample,n=n,B=NA,SE=NA,p=NA,beta=NA,cfi=NA,rmsea=NA,srmr=NA))
  pe <- parameterEstimates(f); ss <- standardizedSolution(f)
  rr <- which(pe$op=="~" & pe$rhs=="Pol_Tend")[1]; rs <- which(ss$op=="~" & ss$rhs=="Pol_Tend")[1]
  fm <- fitMeasures(f,c("cfi.robust","rmsea.robust","srmr"))
  data.frame(scale=k,sample=sample,n=n,B=pe$est[rr],SE=pe$se[rr],p=pe$pvalue[rr],beta=ss$est.std[rs],cfi=fm[1],rmsea=fm[2],srmr=fm[3])
}
res <- list(); dif <- list(); inv <- list(); mg <- list()
for(k in names(sc)){
  m <- mdl(k)
  for(s in c("All","Brazil","US")){
    dd <- if(s=="All") d else d[d$Country==s,]
    res[[length(res)+1]] <- slope(fitf(m,dd),k,s,nrow(dd))
  }
  # multigroup: loadings equal, slopes free by country
  if(length(sc[[k]])>1) ge <- "loadings" else ge <- NULL
  m2 <- mdl(k,"c(sB,sU)*Pol_Tend")
  m2 <- paste0(m2,"\ndiffUS := sU - sB")
  f2 <- fitf(m2,d,"Country",ge)
  if(!is.null(f2) && lavInspect(f2,"converged")){
    pe <- parameterEstimates(f2); r <- pe[pe$label=="diffUS",]
    gl <- lavInspect(f2,"group.label")
    ss2 <- standardizedSolution(f2)
    for(gi in seq_along(gl)){
      rr <- which(pe$op=="~" & pe$rhs=="Pol_Tend" & pe$group==gi)[1]; rs <- which(ss2$op=="~" & ss2$rhs=="Pol_Tend" & ss2$group==gi)[1]
      mg[[length(mg)+1]] <- data.frame(scale=k,sample=gl[gi],B=pe$est[rr],SE=pe$se[rr],p=pe$pvalue[rr],beta=ss2$est.std[rs])
    }
    dif[[length(dif)+1]] <- data.frame(scale=k,diff=r$est,SE=r$se,p=r$pvalue,groups=paste(gl,collapse="/"))
    if(length(sc[[k]])>1){
      f3 <- fitf(mdl(k,"Pol_Tend"),d,"Country",NULL)
      f4 <- fitf(mdl(k,"Pol_Tend"),d,"Country","loadings")
      ok <- function(f) !is.null(f) && lavInspect(f,"converged")
      c3 <- if(ok(f3)) unname(fitMeasures(f3,"cfi.robust")) else NA
      c4 <- if(ok(f4)) unname(fitMeasures(f4,"cfi.robust")) else NA
      inv[[length(inv)+1]] <- data.frame(scale=k,cfi_config=c3,cfi_metric=c4,dcfi=c4-c3)
    }
  } else dif[[length(dif)+1]] <- data.frame(scale=k,diff=NA,SE=NA,p=NA,groups=NA)
  cat(k,"done\n")
}
R <- do.call(rbind,res); D <- do.call(rbind,dif); I <- do.call(rbind,inv)
rownames(R) <- rownames(D) <- rownames(I) <- NULL
MG <- do.call(rbind,mg); write.csv(MG,"latent_mg.csv",row.names=FALSE); write.csv(R,"latent_slopes.csv",row.names=FALSE); write.csv(D,"latent_diff.csv",row.names=FALSE); write.csv(I,"latent_inv.csv",row.names=FALSE)
print(R,digits=3); print(D,digits=3); print(I,digits=3)
