#Libraries

library(ggplot2)
library(foreach)
library(data.table)

#Function

WF_fun <- function(nGens, popSize, startingAlleleFreq,locus){
  tmp <- data.table(gen=c(1:nGens), af=-1, popSize=popSize, locus=locus)
  tmp[gen==1]$af <- startingAlleleFreq
  
  for(i in 2:nGens) {
    tmp[gen==i]$af <- rbinom(1, popSize, tmp[gen==(i-1)]$af)/popSize
  }
  tmp
}
tmp2 <-foreach(popSize.i = c(100, 500,1000,10000), .combine = "rbind")%do%{
  foreach(locus.i=c(1:10), .combine="rbind")%do%{
    WF_fun(popSize=popSize.i, nGens=10, startingAlleleFreq = .5, locus=locus.i)
  }
}

#Plot

ggplot(data=tmp2, aes(x=gen, y=af, group=locus)) + geom_line() + facet_grid(~popSize) +
  ylab("Allele Frequency") + xlab("Generation")
