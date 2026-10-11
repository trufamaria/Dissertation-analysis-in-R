suppressMessages(library(lavaan)); suppressMessages(library(MASS))
x0 <- readRDS("semfits.rds"); x2 <- readRDS("semfits_pol.rds"); grp <- x2$grp
set.seed(7)
contr <- function(f,nm){
  pe <- parameterEstimates(f); pt <- parTable(f); V <- vcov(f)
  labs <- c(paste0("a_",grp),paste0("b_",grp),paste0("c_",grp))
  est <- sapply(labs,function(l) pe$est[which(pe$label==l)[1]])
  ids <- sapply(labs,function(l) pt$free[which(pt$label==l)[1]])
  S <- V[ids,ids]; D <- mvrnorm(40000,est,S); colnames(D) <- labs
  g <- function(p,grp1) D[,paste0(p,"_",grp1)]
  ind <- function(grp1) g("a",grp1)*g("b",grp1)
  out <- list(
   "Gender diff a, Brazil"=g("a","Brazil_Men")-g("a","Brazil_Women"),
   "Gender diff a, US"=g("a","US_Men")-g("a","US_Women"),
   "Gender diff b, Brazil"=g("b","Brazil_Men")-g("b","Brazil_Women"),
   "Gender diff b, US"=g("b","US_Men")-g("b","US_Women"),
   "Gender diff c, Brazil"=g("c","Brazil_Men")-g("c","Brazil_Women"),
   "Gender diff c, US"=g("c","US_Men")-g("c","US_Women"),
   "Ind gender diff, Brazil"=ind("Brazil_Men")-ind("Brazil_Women"),
   "Ind gender diff, US"=ind("US_Men")-ind("US_Women"),
   "Ind country diff, men"=ind("Brazil_Men")-ind("US_Men"),
   "Ind country diff, women"=ind("Brazil_Women")-ind("US_Women"),
   "Ind CountryxGender"=(ind("Brazil_Men")-ind("Brazil_Women"))-(ind("US_Men")-ind("US_Women")),
   "a CountryxGender"=(g("a","Brazil_Men")-g("a","Brazil_Women"))-(g("a","US_Men")-g("a","US_Women")),
   "b CountryxGender"=(g("b","Brazil_Men")-g("b","Brazil_Women"))-(g("b","US_Men")-g("b","US_Women")),
   "c CountryxGender"=(g("c","Brazil_Men")-g("c","Brazil_Women"))-(g("c","US_Men")-g("c","US_Women")))
  data.frame(model=nm,contrast=names(out),est=sapply(out,mean),SE=sapply(out,sd),lo=sapply(out,quantile,.025),hi=sapply(out,quantile,.975),row.names=NULL)
}
r <- rbind(contr(x0$f0,"no_ctrl"),contr(x2$f2,"pol_only"))
print(r,digits=3); write.csv(r,"sem_contrasts_pol.csv",row.names=FALSE)
