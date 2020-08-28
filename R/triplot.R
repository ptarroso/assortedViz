.tricolor <- function(x, col.FUN, breaks) {
	value <- cut(x, breaks)
	col.FUN(length(levels(value)))[as.integer(value)]
}

triplot <- function(x1, x2, y, z, size=0.025, ps=1, FUN=mean,
                    col.FUN=hcl.colors, breaks=100, 
                    x1.at = NULL, x1.lab = x1.at,
                    x2.at = NULL, x2.lab = x2.at, 
                    y.at = NULL, y.lab = y.at, 
                    displayNA=TRUE, axis.labels = NULL, ...) {

	if (length(breaks) == 1) {
		breaks <- seq(min(z), max(z), length.out=breaks)
	}

    hs <- size/2

    ct <- expand.grid(x = seq(size/2, 1-size/2, size), 
                             y = seq(size/2, 1-size/2, size))

    dt <- matrix(NA, nrow(ct), 2)

    # scale vars to range 0-1
    vals <- matrix(c(min(x1, na.rm=T), min(x2, na.rm=T), min(y, na.rm=T),
                     diff(range(x1, na.rm=T)), diff(range(x2, na.rm=T)), 
                     diff(range(y, na.rm=T))), 3)
    x1.s <- (x1 - vals[1,1]) / vals[1,2]
    x2.s <- (x2 - vals[2,1]) / vals[2,2]
    y.s <- (y - vals[3,1]) / vals[3,2]

    plot.new()
    plot.window(c(0,1), c(0,1), asp=1)
   
    for (i in 1:nrow(ct)) {
        x1mask <- x1.s >= ct[i,1]-hs & x1.s < ct[i,1]+hs
        x2mask <- x2.s >= ct[i,1]-hs & x2.s < ct[i,1]+hs
        ymask <- y.s >= ct[i,2]-hs & y.s < ct[i,2]+hs
        pntx1 <- FUN(z[x1mask & ymask])
        pntx2 <- FUN(z[x2mask & ymask])
        dt[i,] <- c(pntx1, pntx2)
        
		if (!is.na(pntx1) | displayNA) {
			polygon(x=c(rep(ct[i,1] - hs*ps, 2), ct[i,1] + hs*ps), 
				    y=c(ct[i,2]+hs*ps, rep(ct[i,2] - hs*ps, 2)),
				    col=.tricolor(pntx1, col.FUN, breaks), ...)  
		}
		
		if (!is.na(pntx2) | displayNA) {
			polygon(x=c(ct[i,1]-hs*ps, rep(ct[i,1] + hs*ps, 2)),
				    y=c(rep(ct[i,2] + hs*ps, 2), ct[i,2] - hs*ps), 
				    col=.tricolor(pntx2, col.FUN, breaks), ...)  
	    }
    }
    if (!is.null(x1.at)) {
    	axis(1, at=(x1.at - vals[1,1]) / vals[1,2], labels=x1.lab, pos=0)
	}
    if (!is.null(x2.at)) {
        axis(3, at=(x2.at - vals[2,1]) / vals[2,2], labels=x2.lab, pos=1)
	}
    if (!is.null(y.at)) {
        axis(2, at=(y.at - vals[3,1]) / vals[3,2], labels=y.lab, pos = 0)
    }
  
	if (!is.null(axis.labels)) {
		mtext(axis.labels[1], 1, 1)
		mtext(axis.labels[2], 3, 1)
		mtext(axis.labels[3], 2, 1)
	}
    rect(0,0,1,1)
}
