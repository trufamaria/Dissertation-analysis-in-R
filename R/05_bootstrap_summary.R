x <- readRDS("boot_pol.rds"); M <- x$M; est0 <- x$est0; grp <- x$grp
M <- M[complete.cases(M),,drop=FALSE]; cat("usable reps:",nrow(M),"\n")
ci <- function(v) quantile(v,c(.025,.975))
ind <- function(g,m=M,e=NULL) if(is.null(e)) m[,paste0("a_",g)]*m[,paste0("b_",g)] else e[paste0("a_",g)]*e[paste0("b_",g)]
rows <- list()
add <- function(nm,bv,e){ q <- ci(bv); rows[[length(rows)+1]] <<- data.frame(effect=nm,est=e,bootSE=sd(bv),lo=q[1],hi=q[2],sig=ifelse(q[1]>0|q[2]<0,"yes","no")) }
for(g in c("Brazil_Men","Brazil_Women","US_Men","US_Women")) add(paste("Indirect",g),ind(g),ind(g,e=est0))
D <- function(f) list(b=f(M,NULL),e=f(NULL,est0))
cont <- list(
 "Gender diff indirect, Brazil"=function(m,e) ind("Brazil_Men",m,e)-ind("Brazil_Women",m,e),
 "Gender diff indirect, US"=function(m,e) ind("US_Men",m,e)-ind("US_Women",m,e),
 "Country diff indirect, men"=function(m,e) ind("Brazil_Men",m,e)-ind("US_Men",m,e),
 "Country diff indirect, women"=function(m,e) ind("Brazil_Women",m,e)-ind("US_Women",m,e),
 "Country x Gender, indirect"=function(m,e) (ind("Brazil_Men",m,e)-ind("Brazil_Women",m,e))-(ind("US_Men",m,e)-ind("US_Women",m,e)))
for(n in names(cont)){ f <- cont[[n]]; add(n,f(M,NULL),f(NULL,est0)) }
pth <- function(p,g,m,e) if(is.null(e)) m[,paste0(p,"_",g)] else e[paste0(p,"_",g)]
pc <- list(
 "Gender diff a, Brazil"=function(m,e) pth("a","Brazil_Men",m,e)-pth("a","Brazil_Women",m,e),
 "Gender diff a, US"=function(m,e) pth("a","US_Men",m,e)-pth("a","US_Women",m,e),
 "Country x Gender, a"=function(m,e) (pth("a","Brazil_Men",m,e)-pth("a","Brazil_Women",m,e))-(pth("a","US_Men",m,e)-pth("a","US_Women",m,e)),
 "Country x Gender, b"=function(m,e) (pth("b","Brazil_Men",m,e)-pth("b","Brazil_Women",m,e))-(pth("b","US_Men",m,e)-pth("b","US_Women",m,e)),
 "Country x Gender, c"=function(m,e) (pth("c","Brazil_Men",m,e)-pth("c","Brazil_Women",m,e))-(pth("c","US_Men",m,e)-pth("c","US_Women",m,e)))
for(n in names(pc)){ f <- pc[[n]]; add(n,f(M,NULL),f(NULL,est0)) }
r <- do.call(rbind,rows); rownames(r) <- NULL; print(r,digits=3); write.csv(r,"boot_pol_summary.csv",row.names=FALSE)
