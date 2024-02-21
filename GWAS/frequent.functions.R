#qqplot
qqp<-function(x,ci=TRUE,add=FALSE,ylab="Observed log10(p-value)",xlab="Expected log10(p-value)",maxLogP,col=1,...){
    if(length(col)>1){
        col<-col[!is.na(x)]
    }
    x<-x[!is.na(x)]
    if(!missing(maxLogP)){
        x[x<10^-maxLogP]<-10^-maxLogP
    }
    N<-length(x)
    ord<-order(x)
    x<-x[ord]
    if(length(col)>1){
        col<-col[ord]
    }
    e<- -log((1:N-0.5)/N,10)
    if(add){
        points(e,-log(x,10),col=col,...)
    } else{
        plot(e,-log(x,10),ylab=ylab,xlab=xlab,col=col,...)
        abline(0,1,col=2,lwd=2)
    }
    if(ci){
        c97.5<-qbeta(0.975,1:N,N-(1:N)+1)
        c02.5<-qbeta(0.025,1:N,N-(1:N)+1)
        lines(e,-log(c97.5,10))
        lines(e,-log(c02.5,10))
    }
}
#qqp(p_scores, main='', las=1, bty='L', pch=19, cex=0.6)

#manhattan plot (basic)
function(pvalues, chrs){  #chrs shuould be only number
  palette(c("#67a9cf","#2166ac"))
  plot(0,0,col="transparent", xaxt="n", las=1, xaxt='n', bty='L',
    xlim=c(0, nrow(assoc)), ylim=c(0, -log10(min(assoc$p))),
    xlab='Chromosome',ylab='Observed log10(p-value)',pch=19, cex=0.4,
    main='', sep=' ')
  points(-log10(pvalues), col=chrs, pch=19, cex=0.5) #  pvalues and chrs have to be sorted by chr. As points() will plot the values of the vector against their indices.
  chroms_count = table(chrs)  #chrs shuould be only numbers
  axis(1, at = round(cumsum(chroms_count)-chroms_count/2), labels=names(chroms_count),cex.axis=0.8)
  abline(h=-log10(5e-8), lty=2)
}

