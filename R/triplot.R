.tricolor <- function(x, col.FUN, breaks) {
  value <- cut(x, breaks, include.lowest=TRUE)
  col.FUN(length(levels(value)))[as.integer(value)]
}

triplot <- function(x1, x2, y, z, size = 0.025, ps = 1, FUN = mean, col.FUN = hcl.colors,
  breaks = 100, x1.at = NULL, x1.lab = x1.at, x2.at = NULL, x2.lab = x2.at, y.at = NULL,
  y.lab = y.at, displayNA = TRUE, axis.labels = NULL, scale=TRUE, scale.at=NULL,
  scale.lab=scale.at, ...) {

  if (length(breaks) == 1) {
    breaks <- seq(min(z, na.rm=T), max(z, na.rm=T), length.out = breaks)
  }

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

  op <- par()

  if (scale) {
    n <- 100
    scl.z <- seq(min(z, na.rm=T), max(z, na.rm=T), length.out=n)
    scl.col <- .tricolor(scl.z, col.FUN, breaks)
    layout(matrix(2:1), heights=c(1, 0.25))
    par(mar=rep(3, 4))
    plot.new()
    plot.window(range(z, na.rm=T), c(0,1))
    par(usr=c(min(z, na.rm=T), max(z, na.rm=T), 0, 1))
    image(scl.z, 1, matrix(scl.z, ncol=1), col=scl.col, add=T)
    box()

    if (is.null(scale.at)) {
      # Place 5 labels (including min and max) equally spaced in scale bar
      scale.at = seq(min(z, na.rm=T), max(z, na.rm=T), diff(range(z, na.rm=T)/5))
    }
    axis(1, at=scale.at, labels=scale.lab)
    if (length(axis.labels)==4) {
        mtext(axis.labels[4], 3, 0)
    }
  }

  plot.new()
  plot.window(c(0, 1), c(0, 1), asp = 1)

  for (i in 1:nrow(ct)) {
    minx <- ct[i, 1] - hs
    maxx <- ct[i, 1] + hs
    miny <- ct[i, 2] - hs
    maxy <- ct[i, 2] + hs

    x1mask <- x1.s >= minx & x1.s < maxx
    x2mask <- x2.s >= minx & x2.s < maxx
    ymask <- y.s >= miny & y.s < maxy
    pntx1 <- FUN(z[x1mask & ymask])
    pntx2 <- FUN(z[x2mask & ymask])

    if (!is.na(pntx1) | displayNA) {
      polygon(x = c(rep(minx * ps, 2), maxx * ps),
              y = c(maxy * ps, rep(miny * ps, 2)),
              col = .tricolor(pntx1, col.FUN, breaks), ...)
    }

    if (!is.na(pntx2) | displayNA) {
      polygon(x = c(minx * ps, rep(maxx * ps, 2)),
              y = c(rep(maxy * ps, 2), miny * ps),
              col = .tricolor(pntx2, col.FUN, breaks), ...)
    }
  }

  # NA prevents labels being displayed; NULL add 'pretty' labels.
  # The NA test is not particularly correct wit side effect no NAs on axis.at.
  if (is.null(x1.at)) {
    x1.at <- seq(min(x1, na.rm=T), max(x1, na.rm=T), diff(range(x1, na.rm=T)/5))
  }
  if (!anyNA(x1.at)) {
    axis(1, at = (x1.at - vals[1, 1])/vals[1, 2], labels = x1.lab, pos = 0)
  }

  if (is.null(x2.at)) {
    x2.at <- seq(min(x2, na.rm=T), max(x2, na.rm=T), diff(range(x2, na.rm=T)/5))
  }
  if (!anyNA(x2.at)) {
    axis(3, at = (x2.at - vals[2, 1])/vals[2, 2], labels = x2.lab, pos = 1)
  }

  if (is.null(y.at)) {
    y.at <- seq(min(y, na.rm=T), max(y, na.rm=T), diff(range(y, na.rm=T)/5))
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
}
