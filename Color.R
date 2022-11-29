
#if the inputVec has NA, the boundary of the color scheme won't be reached, due to the length of inputVec. 
continuousToColors <- function(inputVec,color_palette=c("#001260", "#EAEDE9")){
  pal = colorRampPalette(color_palette)
  order = findInterval(inputVec, sort(inputVec))
  cols = pal(length(inputVec))[order]
  return(cols)
}

#colors for values including nagetive, zero and positive in matrix
#NA for NA, black for diagonal
matrixToColor <- function(inputMatrix,Zmin=NA,Zmax=NA,color_palette=c("#001260", "#EAEDE9", "#601200")){
  if(is.na(Zmin)) Zmin = min(inputMatrix)
  if(is.na(Zmax)) Zmax = max(inputMatrix)
  colMatrx = inputMatrix
  NaIndex = is.na(colMatrx)
  upIndex = (!is.na(colMatrx)) & colMatrx>Zmax
  lowIndex = (!is.na(colMatrx)) &  colMatrx<Zmin
  upBetwIndex = (!is.na(colMatrx)) &  0 < colMatrx & colMatrx <= Zmax
  lowBetwIndex = (!is.na(colMatrx)) &  Zmin <= colMatrx & colMatrx < 0
  Index0 = (!is.na(colMatrx)) &  colMatrx==0
  #colMatrx[NaIndex] = 'lightgrey'
  if(any(upIndex)) colMatrx[upIndex] = color_palette[3]
  if(any(lowIndex)) colMatrx[lowIndex] = color_palette[1]
  if(any(upBetwIndex)) colMatrx[upBetwIndex] = head(continuousToColors(c(as.numeric(colMatrx[upBetwIndex]),Zmax),color_palette[2:3]),-1)
  if(any(lowBetwIndex)) colMatrx[lowBetwIndex] = tail(continuousToColors(c(Zmin,as.numeric(colMatrx[lowBetwIndex])),color_palette[1:2]),-1)
  if(any(Index0)) colMatrx[Index0] = color_palette[2]
  diag(colMatrx) <- 'black'
  return(colMatrx)
}
