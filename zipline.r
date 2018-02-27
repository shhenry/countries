---
title: "Zip Line"
author: "Shae Henry"
date: ""
output: 
  html_document:
    fig_height: 3
    fig_width: 5
---
<!-- Don't edit in between this line and the one below -->
```{r include=FALSE}
# Don't delete this chunk if you are using the DataComputing package
library(DataComputing)
```
*Source file* 
```{r, results='asis', echo=FALSE}
includeSourceDocuments()
```

```{r}
initz <- c(5,10,0,10)
mySystem <- list(
  dz1 = function(t,zs) zs[2],
  dz2 = function(t,zs) zs[1]^2+zs[3]+2*zs[4]+2*t,
  dz3 = function(t,zs) zs[4],
  dz4 = function(t,zs) zs[1]+zs[2]+3*zs[3]^2+5*t
)

rungeKutta <- function(t0,z0,t1,steps,fns){
  h <- (t1-t0)/steps
  t <- numeric(steps+1)
  size <- length(fns)
  results <- matrix(0,nrow=steps+1,ncol=size+1)
  results[1,] <- c(t0,z0)
 
  for(i in 2:(steps+1)){
    old <- results [i-1,]
    args <- list(t_old=old[1], z_old=old[-1])
    k1 <- numeric(size)
    k2 <- numeric(size)
    k3 <- numeric(size)
    k4 <- numeric(size)
    
    #stage 1 loop 
      for(m in 1:size){
        k1[m] <- h*fns[[m]](args$t_old,args$z_old)}
    #stage 2 loop
      for(m in 1:size){
        k2[m] <- h*fns[[m]](args$t_old*h/2, args$z_old+k1/2)}
    #stage 3 loop
      for(m in 1:size){
      k3[m] <- h*fns[[m]](args$t_old*h/2, args$z_old+k2/2)}
    #stage 4 loop
      for(m in 1:size){
      k4[m] <- h*fns[[m]](args$t_old*h, args$z_old+k3)}
    
    results [i,1] <- old[1]+h
    results[i,-1] <- k
  }
}

colnames(results) <- c("t",paste0("z",1:size))
as.data.frame(results)
```