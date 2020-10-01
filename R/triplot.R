.tricolor <- function(x, col.FUN, breaks) {
  value <- cut(x, breaks, include.lowest=TRUE)
  col.FUN(length(levels(value)))[as.integer(value)]
}

triplot <- function(x1, x2, y, z, size = 0.025, ps = 1, FUN = mean,
  col.FUN = hcl.colors, breaks = 100, x1.at = NULL, x1.lab = x1.at,
  x2.at = NULL, x2.lab = x2.at, y.at = NULL, y.lab = y.at, displayNA = TRUE,
  axis.labels = NULL, scale=TRUE, scale.at=NULL, scale.lab=scale.at, ...) {

  hs <- size/2

  ct <- expand.grid(x = seq(size/2, 1 - size/2, size),
                    y = seq(size/2, 1 - size/2, size))

  # scale vars to range 0-1
  vals <- matrix(c(min(x1, na.rm = T), min(x2, na.rm = T), min(y, na.rm = T),
                   diff(range(x1, na.rm = T)), diff(range(x2, na.rm = T)),
                   diff(range(y, na.rm = T))), 3)
  x1.s <- (x1 - vals[1, 1])/vals[1, 2]
  x2.s <- (x2 - vals[2, 1])/vals[2, 2]
  y.s <- (y - vals[3, 1])/vals[3, 2]

  # Prepare grid on original coordinates to return
  ct.o <- matrix(NA, nrow(ct), 3)
  colnames(ct.o) <- c("x1", "x2", "y")
  ct.o[,1] <- ct[,1] * vals[1,2] + vals[1,1]
  ct.o[,2] <- ct[,1] * vals[2,2] + vals[2,1]
  ct.o[,3] <- ct[,2] * vals[3,2] + vals[3,1]

  if (scale) {
    # divide the plot in two areas for displaying z color scale
    layout(matrix(1:2), heights=c(1, 0.25))
  }

  plot.new()
  plot.window(c(0, 1), c(0, 1), asp = 1)

  # Prepara a table of triangle values to display and return
  tri <- matrix(NA, nrow(ct), 6)
  colnames(tri) <- c("x1.y", "x2.y", "minx", "maxx", "miny", "maxy")

  #TODO: speed up this loop
  for (i in 1:nrow(ct)) {
    minx <- ct[i, 1] - hs
    maxx <- ct[i, 1] + hs
    miny <- ct[i, 2] - hs
    maxy <- ct[i, 2] + hs

    x1mask <- x1.s >= minx & x1.s < maxx
    x2mask <- x2.s >= minx & x2.s < maxx
    ymask <- y.s >= miny & y.s < maxy
    tri[i,1] <- FUN(z[x1mask & ymask])
    tri[i,2] <- FUN(z[x2mask & ymask])
    tri[i,3:6] <- c(minx, maxx, miny, maxy)
  }

  rng.z <- range(tri, na.rm=T)
  # Prepare breaks for z, if only integer is given
  if (length(breaks) == 1) {
    breaks <- seq(rng.z[1], rng.z[2], length.out = breaks)
  }

  # Plot triangles
  for (i in 1:nrow(ct)) {
    if (!is.na(tri[i,1]) | displayNA) {
        polygon(x = c(rep(tri[i,3] * ps, 2), tri[i,4] * ps),
                y = c(tri[i,6] * ps, rep(tri[i,5] * ps, 2)),
                col = .tricolor(tri[i,1], col.FUN, breaks), ...)
    }

    if (!is.na(tri[i,2]) | displayNA) {
      polygon(x = c(tri[i,3] * ps, rep(tri[i,4] * ps, 2)),
              y = c(rep(tri[i,6] * ps, 2), tri[i,5] * ps),
              col = .tricolor(tri[i,2], col.FUN, breaks), ...)
    }
  }

  # NA prevents labels being displayed; NULL add 'pretty' labels.
  # The NA test is not particularly correct wit side effect no NAs on axis.at.
  if (is.null(x1.at)) {
    x1.at <- seq(min(x1, na.rm=T), max(x1, na.rm=T), diff(range(x1, na.rm=T))/5)
    x1.at <- round(x1.at, floor(2.5/log1p(diff(range(x1.at)))))
  }
  if (!anyNA(x1.at)) {
    axis(1, at = (x1.at - vals[1, 1])/vals[1, 2], labels = x1.lab, pos = 0)
  }

  if (is.null(x2.at)) {
    x2.at <- seq(min(x2, na.rm=T), max(x2, na.rm=T), diff(range(x2, na.rm=T))/5)
    x2.at <- round(x2.at, floor(2.5/log1p(diff(range(x2.at)))))
  }
  if (!anyNA(x2.at)) {
    axis(3, at = (x2.at - vals[2, 1])/vals[2, 2], labels = x2.lab, pos = 1)
  }

  if (is.null(y.at)) {
    y.at <- seq(min(y, na.rm=T), max(y, na.rm=T), diff(range(y, na.rm=T))/5)
    y.at <- round(y.at, floor(2.5/log1p(diff(range(y.at)))))
  }
  if (!anyNA(y.at)) {
    axis(2, at = (y.at - vals[3, 1])/vals[3, 2], labels = y.lab, pos = 0)
  }

  if (!is.null(axis.labels)) {
    mtext(axis.labels[1], 1, 1)
    mtext(axis.labels[2], 3, 1)
    mtext(axis.labels[3], 2, 1)
  }
  rect(0, 0, 1, 1)

  if (scale) {
    n <- 100 # Pass this as a function argument?
    # scale only displays values in the plot after FUN, not original z range.
    scl.z <- seq(rng.z[1], rng.z[2], length.out=n)
    scl.col <- .tricolor(scl.z, col.FUN, breaks)
    op <- par(mar=rep(3, 4), "mfg", "usr", "xaxp", "yaxp")
    plot.new()
    plot.window(rng.z, c(0,1))
    par(usr=c(rng.z, 0, 1))
    image(scl.z, 1, matrix(scl.z, ncol=1), col=scl.col, add=T)
    box()

    if (is.null(scale.at)) {
      # Place 5 labels (including min and max) equally spaced in scale bar
      scale.at <- seq(rng.z[1], rng.z[2], diff(rng.z)/5)
      scale.at <- round(scale.at, floor(2.5/log1p(diff(range(scale.at)))))
    }
    axis(1, at=scale.at, labels=scale.lab)
    if (length(axis.labels)==4) {
        mtext(axis.labels[4], 3, 0)
    }

    # Forces to return to 1st plot for simplifing adding additional items later
    par(op)
  }

  # Returns data if user wants to collect it
  tri <- list(tri=tri, original.crd=ct.o, plot.crd=ct)
}
