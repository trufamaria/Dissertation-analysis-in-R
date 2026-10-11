suppressMessages({library(lavaan);library(haven);library(parallel)})
args <- commandArgs(TRUE); B <- as.integer(args[1]); out <- args[2]; nc <- as.integer(args[3])
d <- read_sav("../data/Merged_BR_US_Updated.sav")
d <- d[nchar(as.character(d$Age)) <= 3, ]; d <- d[d$Gender %in% c(1,2), ]
d <- as.data.frame(zap_labels(d))
d$Country <- ifelse(d$Is_Br==1,"US","Brazil"); d$G <- ifelse(d$Gender==1,"Men","Women")
d$Group <- paste(d$Country,d$G,sep="_")
x <- readRDS("semfits_pol.rds"); f2 <- x$f2; grp <- x$grp
meas <- '
RD   =~ RD1+RD2+RD3+RD4+RD5
Misg =~ Misg1+Misg2+Misg3+Misg4+Misg5+Misg6+Misg7+Misg8+Misg9+Misg10
SPV  =~ SPV1+SPV2+SPV3+SPV4
RD1~~RD2
Misg2~~Misg3
Misg7~~Misg8
SPV3~~SPV4
'
lab <- function(p) paste0("c(",paste0(p,"_",grp,collapse=","),")")
mod <- paste0(meas,"\nMisg ~ ",lab("a"),"*RD + Pol_Tend\nSPV ~ ",lab("b"),"*Misg + ",lab("c"),"*RD + Pol_Tend\nRD ~ Pol_Tend\n")
keep <- c(paste0("a_",grp),paste0("b_",grp),paste0("c_",grp))
pe0 <- parameterEstimates(f2); est0 <- sapply(keep,function(l) pe0$est[which(pe0$label==l)[1]])
one <- function(i){
  set.seed(1000+i)
  bd <- do.call(rbind,lapply(grp,function(gn){ g <- d[d$Group==gn,]; g[sample(nrow(g),replace=TRUE),] }))
  f <- try(suppressWarnings(sem(mod,data=bd,group="Group",group.equal="loadings",estimator="ML",missing="fiml",fixed.x=FALSE,se="none",start=f2,warn=FALSE)),silent=TRUE)
  if(inherits(f,"try-error") || !lavInspect(f,"converged")) return(rep(NA_real_,length(keep)))
  pe <- parameterEstimates(f); sapply(keep,function(l) pe$est[which(pe$label==l)[1]])
}
t0 <- Sys.time()
res <- mclapply(seq_len(B),one,mc.cores=nc)
M <- do.call(rbind,res); colnames(M) <- keep
cat("reps:",B,"converged:",sum(complete.cases(M)),"minutes:",round(as.numeric(difftime(Sys.time(),t0,units="mins")),2),"\n")
saveRDS(list(M=M,est0=est0,grp=grp),out)
