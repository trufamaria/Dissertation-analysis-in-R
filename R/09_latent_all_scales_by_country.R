suppressMessages(library(lavaan))
src <- readLines("../R/08_latent_all_scales_pooled.R"); i <- grep("^t0 <-",src)
eval(parse(text=src[1:(i-1)]))
out <- list()
for(cn in c("Brazil","US")){
  dd <- d[d$Country==cn,]
  f <- tryCatch(suppressWarnings(sem(model,data=dd,estimator="MLR",missing="fiml",fixed.x=FALSE)),error=function(e){print(e);NULL})
  if(is.null(f)||!lavInspect(f,"converged")){cat(cn,"failed\n");next}
  saveRDS(f,paste0("allsem_",cn,".rds"))
  pe <- parameterEstimates(f); ss <- standardizedSolution(f)
  a <- pe[pe$op=="~"&pe$lhs=="Pol_Tend",c("rhs","est","se","pvalue")]
  b <- ss[ss$op=="~"&ss$lhs=="Pol_Tend",c("rhs","est.std")]
  o <- merge(a,b,by="rhs",sort=FALSE); o$country=cn; o$r2=lavInspect(f,"rsquare")["Pol_Tend"]
  o$cfi=fitMeasures(f,"cfi.robust"); o$rmsea=fitMeasures(f,"rmsea.robust"); o$srmr=fitMeasures(f,"srmr")
  out[[cn]] <- o; cat(cn,"done\n")
}
write.csv(do.call(rbind,out),"allsem_country_coefs.csv",row.names=FALSE)
