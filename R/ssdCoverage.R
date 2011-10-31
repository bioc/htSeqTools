setMethod("ssdCoverage", signature(x="IRangesList"), function(x, mc.cores=1) {
  covx <- coverage(x)
  #cvs6 <- sd(covx)/mean(covx)
  cvs6 <- weighted.mean(sd(covx),w=sapply(covx,length))
  cvs6 <- 1000*cvs6/sqrt(sum(sapply(x,length)))
  return(cvs6)
}
)

setMethod("ssdCoverage", signature(x='RangedData'), function(x, mc.cores=1) { ssdCoverage(ranges(x)) } )

setMethod("ssdCoverage", signature(x='RangedDataList'), function(x, mc.cores=1) {
  if (mc.cores>1) {
    if ('multicore' %in% loadedNamespaces()) {
      ans <- multicore::mclapply(as.list(x), ssdCoverage, mc.cores=mc.cores, mc.preschedule=FALSE)
    } else stop('multicore library has not been loaded!')
  } else {
    ans <- lapply(x, ssdCoverage)
  }
  return(unlist(ans))
}
)