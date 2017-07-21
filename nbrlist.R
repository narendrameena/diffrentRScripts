##' author: narendra
##' descritpion: Produce a Pseudo-Dorling Cartogram.
##' 
##' @param name A vector of region names.
##' @param centroidx A vector of x-coordinates of the regions.
##' @param centroidy A vector of y-coordinates of the regions.
##' @param density A vector of the variable of interest. It will be used as the radii of the circles.
##' @param nbr A list of the neighbors of every region. Each element is a vector of all the neighbor names of a region. If nbr=NULL, then it is assumed that no region has any neighbors. If nbr is not NULL, then names should be given to all the elements of the list, for matching the neighbors with the host region name, otherwise the parameter "name" (a character vector) will be used as the element names of nbr. Besides, any values in nbr that are not in "name" will be removed. The length of nbr could be different from the length of "name", but any element in nbr whose name is not in "name" will be removed too.
##' @param shared.border A matrix of the counts of shared borders, typically generated from the function \code{border_summary_length()}. It is used to scale the attract force.
##' @param color a vector of color to fill in the circles or polygons. Auto-completed if the length does not match with name.
##' @param tolerance Tolerant value for the sum of overlapped radii.
##' @param dist.ratio The threshold to determine whether an attract force is added. It is applied to the ratio of the distance between two centroids and the sum of the two radii.
##' @param iteration The limit of the number of iterations. Default to be 9999.
##' @param polygon.vertex The number of vertice of the circle. Default to be 100. If polygon.vertex=4 then diamands applies. If polygon.vertex=6, then hexagon applies.
##' @param animation Whether to show the movements of centroids.
##' @param sleep.time Only works when animation=TRUE.
##' @param nbredge whether to draw the lines between neighbor regions.
##' @param name.text whether to print the region names on the circles or polygons.
##' @param ggplot2 whether to use ggplot2 to draw the cartogram.
##' @example inst/ex_dorling.R
##' @export
##' 
dorling = function(name, centroidx, centroidy, density, nbr=NULL, shared.border=NULL, color=NULL, tolerance=0.1, dist.ratio=1.2, iteration=9999, polygon.vertex=100, animation=TRUE, sleep.time=0.3, nbredge=ifelse(is.null(nbr),FALSE,TRUE), name.text=TRUE, ggplot2=FALSE, ...){
  n=length(name)
  stopifnot(n==length(centroidx), n==length(centroidy), n==length(density), is.numeric(iteration))
  
  # color vector
  if (!is.null(color)) color=complete_color(color, n)
  
  # identify all the names
  name=as.character(name)
  if (is.null(names(nbr)) && sum(duplindex <- duplicated(name))) {
    name[duplindex]=paste(name[duplindex],name[duplindex],1:sum(duplindex),sep="_")
  }
  
  # clean "nbr"
  if (!is.null(nbr)) {        
    if (is.null(names(nbr))) {
      stopifnot(n==length(nbr))
      names(nbr)=name
    } else {
      nbr=nbr[names(nbr) %in% name]
    }
    if (any(!unlist(nbr) %in% name)) {
      nbr = lapply(nbr, function(s){s[s %in% name]})
    }
    edge = data.frame(A=rep(names(nbr),times=sapply(nbr,length)),B=unname(unlist(nbr)))
    edge = t(apply(edge,1,sort))
    edge = edge[!duplicated(edge),]
  }
  
  # Set up the data
  dat=data.frame(name=name,x=centroidx,y=centroidy,density=density,stringsAsFactors=FALSE)
  rownames(dat)=dat$name
  
  # original distance
  origindist=as.matrix(dist(dat[,2:3]))
  
  # rescale the density (the radius) # 3/5/parameter here?
  dat$density=dat$density/max(dat$density)*mean(origindist)/5
  
  # the closest distance for paired centroids
  circleDist=outer(dat$density,dat$density,"+")
  diag(circleDist)=0
  colnames(circleDist)=rownames(circleDist)=name
  
  # set up initial values
  # crtloc is the current centroid locations
  # frc is the force, including repel force and attract force
  crtloc=frc=dat[,2:3]
  crtDist=origindist
  s=0
  err=circleDist-crtDist
  
  while (sum(sapply(err,max,0))>tolerance) {
    s = s + 1
    if (!is.null(iteration) && s>iteration) {
      warning("Reach the largest iteration limit.")
      break
    }
    if (s%%10==0) cat("Iteration: ",s,"\n")
    if (animation) {
      circle(crtloc$x,crtloc$y,dat$density,vertex=polygon.vertex,
             border=if(is.null(color) | all(color==color[1])){1}else{color},
             col=color,add=FALSE,xaxt='n',yaxt='n', ...)
      points(crtloc$x,crtloc$y,col=if(is.null(color)){2}else{color},pch=20)
      if (nbredge) segments(crtloc[edge[,1],1],crtloc[edge[,1],2],
                            crtloc[edge[,2],1],crtloc[edge[,2],2],col='grey70')
      if (name.text) text(crtloc$x,crtloc$y,dat$name,cex=0.8)
      Sys.sleep(sleep.time)
    }
    
    frc$xforce=frc$yforce=frc$xattract=frc$yattract=frc$xrepel=frc$yrepel=0.00000
    
    # Calculate the repel force
    idx = circleDist > crtDist
    #idx = idx & lower.tri(idx)
    for (i in which(rowSums(idx)>0)){
      #if (length(nbr[[name[i]]])==0) next
      for (j in which(idx[i,])){
        #for (j in na.omit(which(idx[i,])[nbr[[name[i]]]])){
        ratio=err[i,j]/crtDist[i,j]/4
        frc$xrepel[i]=frc$xrepel[i]+ratio*(crtloc$x[i]-crtloc$x[j])
        frc$xrepel[j]=frc$xrepel[j]+ratio*(crtloc$x[j]-crtloc$x[i])
        frc$yrepel[i]=frc$yrepel[i]+ratio*(crtloc$y[i]-crtloc$y[j])
        frc$yrepel[j]=frc$yrepel[j]+ratio*(crtloc$y[j]-crtloc$y[i])
      }
    }
    
    # Calculate the attract force
    for (i in 1:length(name)){
      if (length(nbr[[name[i]]])==0) next
      for (j in which(name %in% nbr[[name[i]]])){
        distratio=crtDist[i,j]/circleDist[i,j]
        if (distratio > dist.ratio & crtDist[i,j]>origindist[i,j]){
          border_ratio=ifelse(is.null(shared.border),1/(round(s/10)+15),shared.border[name[i],name[j]]/shared.border[name[i],name[i]]/2)
          ratio=err[i,j]/crtDist[i,j]*border_ratio
          frc$xattract[i]=frc$xattract[i]+ratio*(crtloc$x[i]-crtloc$x[j])
          frc$xattract[j]=frc$xattract[j]+ratio*(crtloc$x[j]-crtloc$x[i])
          frc$yattract[i]=frc$yattract[i]+ratio*(crtloc$y[i]-crtloc$y[j])
          frc$yattract[j]=frc$yattract[j]+ratio*(crtloc$y[j]-crtloc$y[i])
        }
      }
    }
    
    # Find the final force
    frc$xforce=frc$xrepel+frc$xattract
    frc$yforce=frc$yrepel+frc$yattract
    
    # Reduce the force if it changes the relative direction of the neighbors
    for (i in order(sapply(nbr,length),decreasing=TRUE)){
      closest = frc[c(name[i],nbr[[name[i]]]),]
      closest$newx = closest$x + closest$xforce
      closest$newy = closest$y + closest$yforce
      oldrloc = dat[nbr[[name[i]]], c('x','y')]
      oldrloc$x = sign(oldrloc$x - closest$x[1])
      oldrloc$y = sign(oldrloc$y - closest$y[1])
      newrloc = oldrloc
      newrloc$x = sign(closest$newx[-1] - closest$newx[1])
      newrloc$y = sign(closest$newy[-1] - closest$newy[1])
      dif = oldrloc != newrloc
      if (any(dif)) {
        problemx = rownames(dif)[dif[,1]]
        if (length(problemx)) {
          problemx = problemx[which.min(abs(closest[problemx,'newx']-closest$newx[1]))]
          frc[i,'xforce'] = (closest[problemx,'newx'] - frc[i,'x']) * 0.999999
          #cat(i,'\tx\t',name[i],'\t',problemx,'\n')
        }
        problemy = rownames(dif)[dif[,2]]
        if (length(problemy)) {
          problemy = problemy[which.min(abs(closest[problemy,'y']-closest$y[1]))]
          frc[i,'yforce'] = (closest[problemy,'newy'] - frc[i,'y']) * 0.999999
          #cat(i,'\ty\t',name[i],'\t',problemy,'\n')
        }
      }
    }
    
    ###### comment block 
    closest=data.frame(cbind(rownames(crtDist),rownames(crtDist)[nnbr(crtDist,k=1)]))
    closest$xdist=apply(closest,1,function(xv){abs(crtloc[xv[1],1]-crtloc[xv[2],1])})
    closest$ydist=apply(closest,1,function(xv){abs(crtloc[xv[1],2]-crtloc[xv[2],2])})
    closest$dist=sqrt(closest$xdist^2+closest$ydist^2)
    closest$xforce=abs(frc$xforce)
    closest$yforce=abs(frc$yforce)
    closest$force=sqrt(closest$xforce^2+closest$yforce^2)
    closest$xratio=closest$xdist/closest$xforce
    closest$yratio=closest$ydist/closest$yforce
    closest$ratio=closest$dist/closest$force
    closest$xratio[closest$xratio<tolerance]=1
    closest$yratio[closest$yratio<tolerance]=1
    closest$ratio[closest$ratio<tolerance]=1
    closest$r=pmin(closest$xratio,closest$yratio,closest$ratio,na.rm=TRUE)
    closest$r[closest$r>1]=1
    frc$xforce=frc$xforce*closest$r
    frc$yforce=frc$yforce*closest$r
    
    #### end 
    crtloc=crtloc+frc[,8:7]
    crtDist=as.matrix(dist(crtloc))
    err = circleDist-crtDist
  }
  
  circle(crtloc$x,crtloc$y,dat$density,vertex=polygon.vertex,
         border=if(is.null(color) | all(color==color[1])){1}else{color},
         col=color,add=FALSE,xaxt='n',yaxt='n', ...)
  points(crtloc$x,crtloc$y,col=if(is.null(color)){2}else{color},pch=20)
  if (nbredge) segments(crtloc[edge[,1],1],crtloc[edge[,1],2],
                        crtloc[edge[,2],1],crtloc[edge[,2],2],col='grey70')
  if (name.text) text(crtloc$x,crtloc$y,dat$name,cex=0.8)
  return(data.frame(region=name,crtloc,radius=dat$density))
}





##' Find the nearest k neighbors
##' 
##' @param dismtrx The matrix of distance, typically generated by dist().
##' @param k The number of nearest neighbors that are chosen.
##' @return A data frame of indice. For each row, the i th cell is its i th nearest neighbor.
##' @export
##' @examples
##' data(usGeoInfo)
##' capital_dist=as.matrix(dist(usCapitals[,4:5]))
##' capital_neighbor_3=nnbr(capital_dist)
##' capital_neighbor_5=nnbr(capital_dist, 5)
##' 
nnbr = function(distmtrx,k=3){
  stopifnot(k < nrow(distmtrx))
  res=apply(distmtrx,1,function(xv){order(xv)[1:(k+1)]})
  res=t(res)[,2:(k+1)]
  if (k>1) colnames(res)=paste('N',1:k,sep='')
  return(res)
}

##' Find the neighbors in a range
##' 
##' @param dismtrx The matrix of distance, typically generated by dist().
##' @param r The range that is used as a threshold to define the neighborhood. The unit is not mile or km, but the unit for longitude and latitude.
##' @return A list of neighbors. In each element(region/area), the neighbors are sorted by the distance from the shortest to longest.
##' @export
##' @examples
##' data(usGeoInfo)
##' capital_dist=as.matrix(dist(usCapitals[,4:5]))
##' capital_neighbor=rnbr(capital_dist)
##' 
rnbr = function(distmtrx,r=1){
  stopifnot(r>0)
  res=list()
  for (i in 1:nrow(distmtrx)){
    nb=rownames(distmtrx)[order(distmtrx[i,])]
    res[[i]]=nb[1:sum(distmtrx[i,]<=r)][-1]
  }
  names(res)=rownames(distmtrx)
  return(res)
}

##' Find all the neighbors with shared border.
##' 
##' @param region Region names.
##' @param x X-coordinates of all the vertexes
##' @param y Y-coordinates of all the vertexes
##' @param corner whether the sharing of only one corner point is also defined as "neighbor".
##' @return A list of neighbors. In each element(region/area), the neighbors are sorted by the length of shared vertexes (from the longest to the shortest).
##' @export
##' @examples
##' data(usGeoInfo)
##' state_nbrs=nbrlist(state$abbr,state$x,state$y)
##' state_nbrs=nbrlist(state$abbr,state$x,state$y,corner=FALSE)
##' 
nbrlist = function(region,x,y,corner=TRUE){
  stopifnot(length(x)==length(region) && length(y)==length(region))
  digitx = max(min(nchar(as.character(x))),5)
  digity = max(min(nchar(as.character(y))),5)
  dat=data.frame(r=region,x=x,y=y,stringsAsFactors=FALSE)
  #dat$p=paste(round(dat$x,digitx),round(dat$y,digity))
  dat$p=paste(round(as.numeric(dat$x),digitx),round(as.numeric(dat$y),digity))
  dat=dat[!duplicated(dat),]
  censordat=dat[duplicated(dat$p)|duplicated(dat$p,fromLast=TRUE),]
  uniregion=if (is.factor(region)) {levels(region)} else {sort(unique(region))}
  k=length(uniregion)
  res=list()
  for (i in 1:k){
    ins=censordat[censordat$r %in% uniregion[i],]
    out=censordat[! censordat$r %in% uniregion[i],]
    tmp=sort(table(out[out$p %in% ins$p,1]),decreasing=TRUE)
    if (corner) {res[[i]]=names(tmp)} else {res[[i]]=names(tmp[tmp>1])}        
  }
  names(res)=uniregion
  return(res)
}


##' Auto complete (or cut) a vector to a fixed length
##' 
##' @param cl a vector
##' @param targetlen the target length
##' @return a vector of completed cl with length n
##' @export
##' @examples
##' complete_color('red',5)
##' complete_color(c('red','blue'),5)
##' complete_color(c('red','blue','green','yellow','pink','grey'),5)
##' 
complete_color = function(cl,targetlen){
  l = length(cl)
  if (l==0) return()
  if (l < targetlen){
    cl = rep(cl,ceiling(targetlen/l))
  }
  cl=cl[1:targetlen]
  return(cl)
}


##' Draw a circle
##' 
##' This function is used to compute the locations of the circle 
##' border and draw multiple circles. 
##' It borrows the code from plotrix::draw.circle
##' 
##' @param xvec X-coordinates
##' @param yvec Y-coordinates
##' @param rvec Radii
##' @param vertex The number of vertices of the circle
##' @param border Color of border
##' @param col Color to render in circle
##' @param add Whether the circles are added to another plot.
##' @param square A logical value to determine whether to draw squares.
##' @export
##' @examples
##' x=y=1:5
##' r=5:1/5
##' circle(x,y,r,add=FALSE,asp=1)
##' circle(x,y,r,vertex=6,add=TRUE)  # hexagon
##' circle(x,y,r,vertex=4,add=TRUE)  # diamond
##' circle(x,y,r,square=TRUE,add=TRUE)  # square
##'
circle = function(xvec,yvec,rvec,vertex=100,border=1,col=NULL,add=TRUE, square=FALSE,...){
  n=length(xvec)
  stopifnot(length(yvec)==n && n==length(rvec))
  if (length(border) < n)  border = rep(border, length.out = n)
  if (!is.null(col) && length(col) < n) col = rep(col, length.out = n)
  # xylim = par("usr")
  # plotdim = par("pin")
  # ymult = (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
  if (square) {
    angles = seq(pi / 4, 7 * pi / 4, by = pi / 2)
  } else {
    angle.inc = 2 * pi / vertex
    angles = seq(0, 2 * pi - angle.inc, by = angle.inc)
  }
  if (!add) plot(c(min(xvec-rvec),max(xvec+rvec)),c(min(yvec-rvec),max(yvec+rvec)),type='n', ...)
  for (i in 1:n){
    xv <- cos(angles) * rvec[i] + xvec[i]
    yv <- sin(angles) * rvec[i] + yvec[i]
    polygon(xv, yv, border = border[i], col = col[i])
  }
}
