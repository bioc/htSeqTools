setMethod("extendRanges",signature(x='RangedData'),
  function(x,seqLen=200,chrlength,mc.cores=1) {
    if (sum(width(x)>seqLen)>0) warning('Some ranges had width > seqLen. They have been shortened to seqLen.')
    chrnames <- names(x)
    if (sum(!(chrnames %in% names(chrlength)))>0) stop('Chromosome names in x do not match those in chrlength')
    minusStrand <- x[['strand']]=='-'
    end(x)[!minusStrand] <- start(x)[!minusStrand] + (seqLen-1)
    start(x)[minusStrand] <- end(x)[minusStrand] - (seqLen-1)
    start(x)[start(x)<1] <- 1
    chrlength <- chrlength[chrnames]
    nreps <- table(space(x))
    nreps <- nreps[chrnames]
    chrlength <- rep(chrlength,nreps)
    width(x) <- ifelse(end(x)>chrlength, chrlength-start(x)+1, width(x))
    return(x)
  }
)


setMethod("extendRanges",signature(x='RangedDataList'),
  function(x,seqLen=200,chrlength,mc.cores=1) {
    x <- as.list(x)
    if (mc.cores>1) {
      if ('multicore' %in% loadedNamespaces()) {
        ans <- multicore::mclapply(x, function(z) extendRanges(z, seqLen=seqLen, chrlength=chrlength), mc.cores=mc.cores, mc.preschedule=FALSE)
      } else stop('multicore library has not been loaded!')
    } else ans <- lapply(x, function(z) extendRanges(z, seqLen=seqLen, chrlength=chrlength))
    ans <- RangedDataList(ans)
    return(ans)
  }
)