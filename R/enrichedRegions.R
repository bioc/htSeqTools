setMethod("enrichedRegions", signature(sample1='missing',sample2='missing',regions='RangedData',mappedreads='numeric'),
function(sample1, sample2, regions, minReads=10, mappedreads, pvalFilter=0.05, exact=FALSE, p.adjust.method='none', twoTailed=FALSE, mc.cores=1) {
  counts <- regions[names(regions)[lapply(ranges(regions),length)>0]]
  if (nrow(counts)>1) {
    #LR test
    countsDF <- unlist(values(counts), use.names=FALSE)
    countsGroup <- as.matrix(countsDF)
    lrt <- rowLogRegLRT(counts=countsGroup, exact=exact, p.adjust.method=p.adjust.method)
    lrt$pvals[is.na(lrt$pvals)] <- 1 #NAs happen when counts in all samples are 0
    sel <- lrt$pvals<=pvalFilter
    countsDF[['pvalue']] <- lrt$pvals
    rpkm <- countsGroup / ((as.matrix(width(counts))/10^9) %*% t(as.matrix(mappedreads)))
    colnames(rpkm) <- paste('rpkm',colnames(rpkm),sep='.')
    values(counts) <- DataFrame(countsDF, rpkm)
    ans <- counts[sel,]
  } else {
    ans <- RangedData(IRanges(integer(0),integer(0)),space=character(0))
  }
  return(ans)
}
)

setMethod("enrichedRegions", signature(sample1='missing',sample2='missing',regions='RangedData',mappedreads='missing'),
function(sample1, sample2, regions, minReads=10, mappedreads, pvalFilter=0.05, exact=FALSE, p.adjust.method='none', twoTailed=FALSE, mc.cores=1) {
  mappedreads <- colSums(as.data.frame(values(regions))[,c('sample1','sample2')])
  enrichedRegions(regions=regions, minReads=minReads, mappedreads=mappedreads, pvalFilter=pvalFilter, exact=exact, p.adjust.method=p.adjust.method, twoTailed=twoTailed, mc.cores=mc.cores)
}
)

setMethod("enrichedRegions", signature(sample1='list',sample2='missing',regions='missing'),
function(sample1, sample2, regions, minReads=10, mappedreads, pvalFilter=0.05, exact=FALSE, p.adjust.method='none', twoTailed=FALSE,  mc.cores=1) {
  regions <- islandCounts(sample1, minReads=minReads, mc.cores=mc.cores)
  mappedreads <- sapply(sample1,nrow)
  enrichedRegions(regions=regions, pvalFilter=pvalFilter, exact=exact, p.adjust.method=p.adjust.method, mc.cores=mc.cores, mappedreads=mappedreads)
}
)

setMethod("enrichedRegions", signature(sample1='RangedData',sample2='RangedData',regions='missing'),
function(sample1, sample2, regions, minReads=10, mappedreads, pvalFilter=0.05, exact=FALSE, p.adjust.method='none', twoTailed=FALSE,  mc.cores=1) {
  regions <- islandCounts(list(sample1,sample2),minReads=minReads,mc.cores=mc.cores)
  mappedreads <- c(nrow(sample1),nrow(sample2))
  regions <- enrichedRegions(regions=regions, mappedreads=mappedreads, pvalFilter=1, exact=exact, p.adjust.method=p.adjust.method, mc.cores=mc.cores)
  if (nrow(regions)>0) {
    colnames(values(regions)) <- c('sample1','sample2','pvalue','rpkm1','rpkm2')
    if (!twoTailed) regions <- regions[regions[['sample1']]>regions[['sample2']],]
    regions <- regions[regions[['pvalue']]<pvalFilter,]
  }
  return(regions)
}
)

setMethod("enrichedRegions", signature(sample1='RangedData',sample2='missing',regions='missing'),
function(sample1, sample2, regions, minReads=10, mappedreads, pvalFilter=0.05, exact=FALSE, p.adjust.method='none', twoTailed=FALSE,  mc.cores=1) {
  regions <- islandCounts(sample1, minReads=minReads, mc.cores=mc.cores)
  colnames(values(regions)) <- 'n'
  if (nrow(regions)>0) {
    n <- sum(regions[['n']]); p <- 1/length(regions[['n']])
    pvals <- pbinom(regions[['n']] - 1, n, p, lower.tail = FALSE)
    regions[['rpkm']] <- regions[['n']] / ((width(regions)/10^9) * nrow(sample1))
    regions[['pvalue']] <- p.adjust(unlist(pvals),method=p.adjust.method)
    regions <- regions[regions[['pvalue']]<=pvalFilter,]
  }
  return(regions)
}
)

setMethod("enrichedRegions", signature(sample1='GRanges',sample2='missing',regions='missing'),
  function(sample1, sample2, regions, minReads=10, mappedreads, pvalFilter=0.05, exact=FALSE, p.adjust.method='none', twoTailed=FALSE,  mc.cores=1) {
    sample1 <- as(sample1,'RangedData')
    ans <- enrichedRegions(sample1=sample1,minReads=minReads,pvalFilter=pvalFilter,exact=exact,p.adjust.method=p.adjust.method,twoTailed=twoTailed,mc.cores=mc.cores)
    return(ans)
  }
)

setMethod("enrichedRegions", signature(sample1='GRanges',sample2='GRanges',regions='missing'),
  function(sample1, sample2, regions, minReads=10, mappedreads, pvalFilter=0.05, exact=FALSE, p.adjust.method='none', twoTailed=FALSE,  mc.cores=1) {
    sample1 <- as(sample1,'RangedData')
    sample2 <- as(sample2,'RangedData')
    ans <- enrichedRegions(sample1=sample1,sample2=sample2,minReads=minReads,pvalFilter=pvalFilter,exact=exact,p.adjust.method=p.adjust.method,twoTailed=twoTailed,mc.cores=mc.cores)
    ans <- as(ans,'GRanges')
    return(ans)
  }
)

setMethod("enrichedRegions", signature(sample1='missing',sample2='missing',regions='GRanges'),
  function(sample1, sample2, regions, minReads=10, mappedreads, pvalFilter=0.05, exact=FALSE, p.adjust.method='none', twoTailed=FALSE,  mc.cores=1) {
    regions <- as(regions,'RangedData')
    ans <- enrichedRegions(regions=regions,minReads=minReads,pvalFilter=pvalFilter,exact=exact,p.adjust.method=p.adjust.method,twoTailed=twoTailed,mc.cores=mc.cores)
    ans <- as(ans,'GRanges')
    return(ans)
  }
)

setMethod("enrichedRegions", signature(sample1='GRangesList',sample2='missing',regions='missing'),
  function(sample1, sample2, regions, minReads=10, mappedreads, pvalFilter=0.05, exact=FALSE, p.adjust.method='none', twoTailed=FALSE,  mc.cores=1) {
    sample1 <- list(lapply(sample1,function(y) as(y,'RangedData')))
    ans <- enrichedRegions(sample1=sample1,minReads=minReads,pvalFilter=pvalFilter,exact=exact,p.adjust.method=p.adjust.method,twoTailed=twoTailed,mc.cores=mc.cores)
    ans <- as(ans,'GRangesList')
    return(ans)
  }
)
