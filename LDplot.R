snp.sq <- function(i, j, ld, direction, zoomX, zoomY, xadjust, yadjust, write.ld.val, color, polygon.par, cex.ld) {
  if(i == j) return(); # pas de plot d'un SNP avec lui même
  if(j < i) { tmp <- j; j <- i; i <- tmp } # i < j
  d <- (j-i)/2-0.5-1
  cx <- (i+d)*zoomX+xadjust
  cy <- (direction*(d+1))*zoomY+yadjust
  do.call(polygon, c(list(x = cx+c(-1,0,1,0)/2*zoomX, y = cy + c(0,1,0,-1)/2*zoomY, col = color), polygon.par))
  if(write.ld.val) text(cx, cy, ld, cex = cex.ld)
}

#direction: 1 for pointing up
#default radius is 0.5, zoom = New size*2/N  
EvalPlot <- function(EvalMatrix,direction=1,zoomX=1,zoomY=1,xadjust=0,yadjust=0,write.ld.val=TRUE,cex.ld=1,polygon.par = list(border = "white"),
                    colorMatrix=NA,color.scheme = function(ld) rgb(1,1-abs(ld),1-abs(ld)),write.ld = function(ld) sprintf("%.2f", ld)){
	n <- nrow(EvalMatrix)
	if (is.null(n)){
		print('n<=2, skipping...')
		return()
	}
	positions = rep(0,n)
	depth = n
	max.dist = Inf

	for(i in seq(1,n-1))
	  for(j in seq(i+1, min(n,i+depth))) {
	    if(positions[j] - positions[i] > max.dist) next;
	    if(write.ld.val) ld <- write.ld(EvalMatrix[i,j])
	    snp.sq(i, j, ld, direction, zoomX, zoomY, xadjust, yadjust, write.ld.val, ifelse(is.na(colorMatrix),color.scheme(EvalMatrix[i,j]),colorMatrix[i,j]), polygon.par, cex.ld)
	  }
}
