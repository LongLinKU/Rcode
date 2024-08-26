forest_plot_by_data_frame <- function(data,BETAs,SEs=c(),CIs=c(),Ylabels=c(),Ns=c(),cols=c(),showXtick=T,showBox=F,highlight=0){
	n = length(BETAs)
	n1 = length(SEs)
	n2 = length(Ylabels)
	if ( ! is.null(SEs) & n!=n1){
		stop("Different numbers of BETA and SE")
	}
	if(!is.null(Ylabels) & n2 != n){
		stop('Different numbers of Y labels and Beta')
	}
	if(!is.null(CIs)){
		n3 = length(CIs)
		if (n3 != n*2){
			stop('Numbers of CI should be twice as Betas')
		}
	}
	no_Ns=FALSE
	if(is.null(Ns)){
		no_Ns = TRUE 
		Ns = rep(20000,n)
	}
	if(is.null(cols)){ cols = seq(1:n)}
	N = dim(data)[1]
	beta_range = range(data[,BETAs],na.rm=T)
	beta_diff = beta_range[2] - beta_range[1]
#	ci_range = range(sapply(1:n, function(x) return(c(data[,BETAs[x]] - qt(0.975,data[,Ns[i]]) * data[,SEs[x]], data[,BETAs[x]] + qt(0.975,data[,Ns[i]]) * data[,SEs[x]]))))
	xlims = c(beta_range[1] - beta_diff*0.3,beta_range[2] + beta_diff*0.3)
	truncate_anchors = c(beta_range[1] - beta_diff,beta_range[2] + beta_diff)
	plot(1, type="n",xlim=xlims,xlab='Effect size and CI',ylab='',yaxt='n',ylim=c(1,N*n+0),cex.lab=1.5)
	truncate_left = c()
	truncate_right = c()
	for (i in 1:n){
		if (no_Ns){
			df = Ns
		}else{
			df = data[,Ns[i]]
		}
		row_num = seq(i,n*N,n)
	    points(data[,BETAs[i]],row_num,pch=16,col=cols[i])
	    if(!is.null(CIs)){
		arrows(data[,CIs[2*i-1]],seq(i,n*N,n),data[,CIs[2*i]],seq(i,n*N,n),angle = 90, code = 3, length=0.03,col=cols[i],lwd=1)
		truncate_left = c(truncate_left,row_num[which(CIs[2*i-1]] < par('usr')[1])])
	        truncate_right = c(truncate_right,row_num[which(CIs[2*i]] > par('usr')[2])])
	    }else{
	    	arrows(data[,BETAs[i]]-qt(0.975,df)*data[,SEs[i]],seq(i,n*N,n),data[,BETAs[i]]+qt(0.975,df)*data[,SEs[i]],seq(i,n*N,n),angle = 90, code = 3, length=0.03,col=cols[i],lwd=1)
	    	truncate_left = c(truncate_left,row_num[which(data[,BETAs[i]]-qt(0.975,df)*data[,SEs[i]] < par('usr')[1])])
	    	truncate_right = c(truncate_right,row_num[which(data[,BETAs[i]]+qt(0.975,df)*data[,SEs[i]] > par('usr')[2])])
	    }
	    if (!is.null(Ylabels)){
			axis(2,at=seq(i,n*N,n),data[,Ylabels[i]],las=1,lwd.ticks = 0, lty = "blank",cex.axis=1,line=1.5)
	    }
	}
	for (i in truncate_left){
		lines(c(truncate_anchors[1],xlims[1]),c(i,i),col='white',lty=2)
	}
	for (i in truncate_right){
		lines(c(xlims[2],truncate_anchors[2]),c(i,i),col='white',lty=2)
	}
	if(showXtick){
		xticks = axTicks(1)
		vColor = rep('grey90',length(xticks))
		vColor[xticks==highlight] = 'grey50'
		abline(v=xticks,col=vColor)
	}
	if(showBox){
		abline(h=seq(n,n*(N-1),n)+0.5)
	}
}


#bitmap('test.png',res=400)
#forest_plot_by_data_frame(res2,BETAs=c('WT','HET','HOM'),SEs=c('se0','se1','se1'),Ns=c('N0','N1','N2'),showBox=T)
# forest_plot_by_data_frame(res2,BETAs=c('WT'),SEs=c('se0'),Ns=c('N0'))
#add y-axis and lables.
#dev.off()
