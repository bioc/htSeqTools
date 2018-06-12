### R code from vignette source 'htSeqTools.Rnw'

###################################################
### code chunk number 1: import1
###################################################
options(width=70)
library(htSeqTools)
data(htSample)
htSample
sapply(htSample,length)


###################################################
### code chunk number 2: import2
###################################################
Ctrl=as.data.frame(htSample[[1]])
IP1=as.data.frame(htSample[[2]])
head(Ctrl)
makeGRangesFromDataFrame(Ctrl)
htSample2 <- GRangesList(Ctrl=makeGRangesFromDataFrame(Ctrl),
                         IP1=makeGRangesFromDataFrame(IP1))
htSample2
htSample2[['Ctrl']]


###################################################
### code chunk number 3: getmds
###################################################
cmds1 <- cmds(htSample,k=2)
cmds1


###################################################
### code chunk number 4: figmds
###################################################
plot(cmds1)


###################################################
### code chunk number 5: figmds
###################################################
plot(cmds1)


###################################################
### code chunk number 6: qcontrol1
###################################################
ssdCoverage(htSample)
giniCoverage(htSample,mc.cores=1)


###################################################
### code chunk number 7: figgini1
###################################################
giniCoverage(htSample[['ctrlBatch2']],mc.cores=1,mk.plot=TRUE)


###################################################
### code chunk number 8: figgini2
###################################################
giniCoverage(htSample[['ipBatch2']],mc.cores=1,mk.plot=TRUE)


###################################################
### code chunk number 9: figgini1
###################################################
giniCoverage(htSample[['ctrlBatch2']],mc.cores=1,mk.plot=TRUE)


###################################################
### code chunk number 10: figgini2
###################################################
giniCoverage(htSample[['ipBatch2']],mc.cores=1,mk.plot=TRUE)


###################################################
### code chunk number 11: qcontrol2
###################################################
contamSample <- GRanges('chr2L',IRanges(rep(1,200),rep(36,200)),strand='+')
contamSample <- c(htSample[['ctrlBatch1']],contamSample)
nrepeats <- tabDuplReads(contamSample)
nrepeats


###################################################
### code chunk number 12: qcontrol3
###################################################
q <- which(cumsum(nrepeats/sum(nrepeats))>.999)[1]
q
fdrest <- fdrEnrichedCounts(nrepeats, use=1:q, components=1)
numRepeArtif <- rownames(fdrest[fdrest$fdrEnriched<0.01,])[1]
numRepeArtif


###################################################
### code chunk number 13: figrepeats1
###################################################
plot(fdrest$fdrEnriched,type='l',ylab='',xlab='Number of repeats (r)')
lines(fdrest$pdfOverall,col=4)
lines(fdrest$pdfH0,col=2,lty=2)
legend('topright',c('FDR','P(r | no artifact)','P(r)'),lty=c(1,2,1),col=c(1,2,4))


###################################################
### code chunk number 14: figrepeats1
###################################################
plot(fdrest$fdrEnriched,type='l',ylab='',xlab='Number of repeats (r)')
lines(fdrest$pdfOverall,col=4)
lines(fdrest$pdfH0,col=2,lty=2)
legend('topright',c('FDR','P(r | no artifact)','P(r)'),lty=c(1,2,1),col=c(1,2,4))


###################################################
### code chunk number 15: figpreprocess1
###################################################
covbefore <- coverage(htSample[['ipBatch2']])
covbefore <- window(covbefore[['chr2L']],295108,297413)
plot(as.integer(covbefore),type='l',ylab='Coverage')


###################################################
### code chunk number 16: preprocess1
###################################################
ip2Aligned <- alignPeaks(htSample[['ipBatch2']],strand='strand', npeaks=100)
covafter <- coverage(ip2Aligned)
covafter <- window(covafter[['chr2L']],295108,297413)
covplus <- coverage(htSample[['ipBatch2']][strand(htSample[['ipBatch2']])=='+'])
covplus <- window(covplus[['chr2L']],295108,297413)
covminus <- coverage(htSample[['ipBatch2']][strand(htSample[['ipBatch2']])=='-'])
covminus <- window(covminus[['chr2L']],295108,297413)


###################################################
### code chunk number 17: figpreprocess2
###################################################
plot(as.integer(covafter),type='l',ylab='Coverage')
lines(as.integer(covplus),col='blue',lty=2)
lines(as.integer(covminus),col='red',lty=2)


###################################################
### code chunk number 18: figpreprocess1
###################################################
covbefore <- coverage(htSample[['ipBatch2']])
covbefore <- window(covbefore[['chr2L']],295108,297413)
plot(as.integer(covbefore),type='l',ylab='Coverage')


###################################################
### code chunk number 19: figpreprocess2
###################################################
plot(as.integer(covafter),type='l',ylab='Coverage')
lines(as.integer(covplus),col='blue',lty=2)
lines(as.integer(covminus),col='red',lty=2)


###################################################
### code chunk number 20: islandCounts
###################################################
ip <- c(htSample[[2]],htSample[[4]])
ctrl <- c(htSample[[1]],htSample[[3]])
pool <- GRangesList(ip=ip, ctrl=ctrl)
counts <- islandCounts(pool,minReads=10)
head(counts)


###################################################
### code chunk number 21: enrichedRegions
###################################################
mappedreads <- c(ip=length(ip),ctrl=length(ctrl))
mappedreads
regions <- enrichedRegions(sample1=ip,sample2=ctrl,minReads=10,mappedreads=mappedreads,p.adjust.method='BY',pvalFilter=.05,twoTailed=FALSE)
head(regions)
nrow(regions)


###################################################
### code chunk number 22: enrichedPeaks
###################################################
peaks <- enrichedPeaks(regions, sample1=ip, sample2=ctrl, minHeight=100)
peaks


###################################################
### code chunk number 23: mergeRegions
###################################################
mergeRegions(peaks, maxDist=300, score='height', aggregateFUN='median')


###################################################
### code chunk number 24: analysis2
###################################################
chrLength <- 500000
names(chrLength) <- c('chr2L')
chrregions <- enrichedChrRegions(peaks, chrLength=chrLength, windowSize=10^4-1, fdr=0.05, nSims=1)
chrregions


###################################################
### code chunk number 25: figanalysis2
###################################################
chrregions <- GRanges(c('chr2L','chr2R'), IRanges(start=c(100000,200000),end=c(100100,210000)))
plotChrRegions(regions=chrregions, chrLength=c(chr2L=500000,chr2R=500000))


###################################################
### code chunk number 26: figanalysis2
###################################################
chrregions <- GRanges(c('chr2L','chr2R'), IRanges(start=c(100000,200000),end=c(100100,210000)))
plotChrRegions(regions=chrregions, chrLength=c(chr2L=500000,chr2R=500000))


###################################################
### code chunk number 27: figplots1
###################################################
set.seed(1)
peaksanno <- peaks
mcols(peaksanno)$start_position <- start(peaksanno) + runif(length(peaksanno),-500,1000)
mcols(peaksanno)$end_position <- mcols(peaksanno)$start_position + 500
mcols(peaksanno)$distance <- mcols(peaksanno)$start_position - start(peaksanno)
strand(peaksanno) <- sample(c('+','-'),length(peaksanno),replace=TRUE)
PeakLocation(peaksanno,peakDistance=1000)


###################################################
### code chunk number 28: figplots1
###################################################
set.seed(1)
peaksanno <- peaks
mcols(peaksanno)$start_position <- start(peaksanno) + runif(length(peaksanno),-500,1000)
mcols(peaksanno)$end_position <- mcols(peaksanno)$start_position + 500
mcols(peaksanno)$distance <- mcols(peaksanno)$start_position - start(peaksanno)
strand(peaksanno) <- sample(c('+','-'),length(peaksanno),replace=TRUE)
PeakLocation(peaksanno,peakDistance=1000)


###################################################
### code chunk number 29: regionscov
###################################################
cover <- coverage(ip)
rcov <- regionsCoverage(seqnames(regions),start(regions),end(regions),cover=cover)
names(rcov)
rcov[['views']]
gridcov <- gridCoverage(rcov)
dim(getCover(gridcov))
getViewsInfo(gridcov)


###################################################
### code chunk number 30: figregCoverage
###################################################
ylim <- c(0,max(getViewsInfo(gridcov)[['maxCov']]))
plot(gridcov, ylim=ylim,lwd=2)
for (i in seq_along(regions)) lines(getCover(gridcov)[i,], col='gray', lty=2)


###################################################
### code chunk number 31: figregCoverage
###################################################
ylim <- c(0,max(getViewsInfo(gridcov)[['maxCov']]))
plot(gridcov, ylim=ylim,lwd=2)
for (i in seq_along(regions)) lines(getCover(gridcov)[i,], col='gray', lty=2)


