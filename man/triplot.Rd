\name{triplot}
\alias{triplot}
\title{ A single plot with 4 variables}
\description{
	Plots 4 variables in a single plot. Two independent variables are plotted
	horizontally and a third one is plotted vertically, allowing to display data
	in a scatter plot. Data from the fourth variables is extracted at the points
	within user-defined squares in the plotted area and aggregated with a 
	function (default average). Aggregated data is displayed with different
	color accordingly to the color scale defined. Squares are divided in two 
	triangles with the top one referring to values aggregated in relation to the
	variable on top axis, and lower triangle in relation to bottom axis 
	variable.
}
\usage{
triplot(x1, x2, y, z, size = 0.025, ps = 1, FUN = mean, col.FUN = hcl.colors, breaks = 100, x1.at = NULL, x1.lab = x1.at, x2.at = NULL, x2.lab = x2.at, y.at = NULL, y.lab = y.at, displayNA = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
	A vector of data for the bottom variable.
}
  \item{x2}{
	A vector of data (same length as x1) for the top variable.
}
  \item{y}{
	A vector of data (same length as x1) for the vertical variable.
}
  \item{z}{
	A vector of data (same length as x1) to be aggregated for the x1 vs. y and
	for x2 vs. y relations.
}
  \item{size}{
	Size of square to aggregate data. See description.
}
  \item{ps}{
	Size of plotted squares/triangles.
}
  \item{FUN}{
	Function to aggregate data (default is mean)
}
  \item{col.FUN}{
	Function for color gradient (default hcl.colors).
}
  \item{breaks}{
	Number of color breaks of sequence of breaks.
}
  \item{x1.at}{
	Locations for tick marks on bottom axis.
}
  \item{x1.lab}{
	Labels for tick marks on bottom axis.
}
  \item{x2.at}{
	Locations for tick marks on top axis.
}
  \item{x2.lab}{
	Labels for tick marks on top axis.
}
  \item{y.at}{
	Locations for tick marks on left axis.
}
  \item{y.lab}{
	Labels for tick marks on left axis.
}
  \item{displayNA}{
	Display squares with NA values?
}
  \item{axis.labels}{
	A vector of length 3 with axis labels for X1, X2 and Y.
}
  \item{\dots}{
	Additional arguments for plotting.
}
}
\details{
	This function allows to plot four variables. Originally was intended for
	plotting variables that shared a spatial location (for instance, spatial 
	raster data) but can be used with other type of data.
	
	Both X variables and Y are internally scaled to the range form zero to one
	for having a full squared plotting area. The plotting area is divided in 
	smaller squares of side of length *size*. If the *size* argument is one, 
	than only one square is fitted. 
	
	Two independent scatter plots are computed internally: one is X1 vs. Y and
	the	other is X2 vs. Y. The values of Z are found for the points within each
	smaller square and aggregated with *FUN* (default to mean value). The
	aggregated value for the X1/Y is displayed in the bottom triangle of the 
	divided square and the value for X2/Y in the top triangle. Values are color
	coded accordingly to the color function given.
}
\value{
	A plot!
}
\references{
	(recursive reference)
}
\author{
	Pedro Tarroso
}
\seealso{
	\code{\link{tile}}
}
\examples{
	# Just demonstration of simple functionality
	triplot(iris$Sepal.Length, iris$Petal.Length, iris$Sepal.Width, iris$Petal.Width)
	
	# With a slightly better display
	lbls <- c("Sepal Length", "Petal Length", "Sepal Width")
	triplot(iris$Sepal.Length, iris$Petal.Length, iris$Sepal.Width, iris$Petal.Width, 				size=0.1, displayNA=F, border="white", 
			x1.at = c(5, 6, 7), x2.at=c(2, 4, 6), y.at=c(2, 3, 4), 
			axis.labels=lbls)

}

