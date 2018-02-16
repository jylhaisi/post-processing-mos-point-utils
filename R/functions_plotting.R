### PLOTTING FUNCTIONS ###



#' Plots points to a map on a user defined lat-lon box, colorscale, limits and legend location
#'
#' @description This function plots points to a geographical map, with points having values which are indicated by a colorscale
#' @usage function(plot_data,plot_data_area,breaks,colorscale,point_size,plotting_title)
#' @details add here
#' @param plot_data a data matrix (first column data, second lon, third lat)
#' @param plot_data_area defines the data points which constitute the plotting area
#' @param breaks a vector defining breaks for plotting
#' @param colorscale the colorscale that is used in plot (if colorscale==points, do not plot any scale)
#' @param point_size size of point to be plotted
#' @param plotting_title plotting title
#' @return A plot
#' @export
#'
plot_points_on_map <- function(plot_data,plot_data_area,breaks,colorscale,point_size,plotting_title) {

  # Removing missing values from plotting data
  plot_data <- plot_data[which(rowSums(is.na(plot_data))==0),]
  plot_data_area <- plot_data_area[which(rowSums(is.na(plot_data_area))==0),]
  if (is.null(breaks)) {
    breaks <- c( -2000, -500, -300, -200, -100, 100, 200, 300, 500, 2000)
  }
  par(mar=c(1.7,1,4,1),xpd=FALSE)
  plotvar <- plot_data[,1]
  tplot <- plot(plot_data_area[,2], plot_data_area[,3], axes=FALSE, cex=.5, type="n", xlab="", ylab="", main=plotting_title)
  map("world", add=TRUE, lwd=.5, col="grey95", fill=TRUE)
  if (colorscale != "points") {
    # Define number of colours to be used in plot
    nclr <- (length(breaks) - 1)
    brk <- breaks
    # Define colour palette to be used
    plotclr <- RColorBrewer::brewer.pal(nclr,colorscale)
    # Define colour intervals and colour code variable for plotting
    class <- classInt::classIntervals(plotvar, nclr, fixedBreaks= brk, style = "fixed")
    colcode <- classInt::findColours(class, plotclr)
    graphics::points(plot_data[,2], plot_data[,3], pch = 16, col= colcode, cex = point_size)
    legend("bottomleft", legend = names(attr(colcode, "table")), fill = attr(colcode, "palette"), cex = 0.7, bty = "n")
    rm(nclr)
    rm(brk)
    rm(plotclr)
    rm(class)
    rm(colcode)
  } else {
    graphics::points(plot_data[,2], plot_data[,3], cex = point_size, pch = 16, col="red")
  }
  rm(plotvar)

  return(tplot)
}






#' Plots conditional quantile plots from two datasets
#'
#' @description This function plots quantile-quantile plot from two datasets. This is an improved version from conditional.quantile() -function in "verification"-package
#' @usage function(pred, obs, bins = NULL, thrs = c(10,20), main, na.rm=TRUE)
#' @details add here
#' @param pred Forecasted value. ([n,1] vector, n = No. of forecasts)
#' @param obs Observed value.([n,1] vector, n = No. of observations)
#' @param bins Bins for forecast and observed values. A minimum number of values are required to calculate meaningful statistics. So for variables that are continuous, such as temperature, it is frequently convenient to bin these values. ([m,1] vector, m = No. of bins)
#' @param thrs The minimum number of values in a bin required to calculate the 25th and 75th quantiles and the 10th and 90th percentiles respectively. The median+min+max values will always be displayed. ( [2,1] vector)
#' @param main Plot title
#' @param ... plotting options
#' @return A plot
#' @export
#'
conditional.quantile.extremes <- function(pred, obs, bins = NULL, thrs = c(10,20), main = "Conditional quantiles", ylab = "Observed Value", xlab = "Forecast Value", na.rm=TRUE, ...) {

  # minimum of 10 values are needed for this function to work!
  if ((min(length(obs),length(pred)))<10) {
    stop("a minimum sample size of 10 is required!")
  }
  # Remove those values which are missing values from either of the datasets!
  if (na.rm==TRUE) {
    indices <- intersect(which(!is.na(pred)),which(!is.na(obs)))
    obs <- obs[indices]
    pred <- pred[indices]
    rm(indices)
  }

  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  if (!is.null(bins)) {
    if (min(bins) > min(obs) | max(bins) < max(obs)) {
      warning("Observations outside of bin range. \n")
    }
    if (min(bins) > min(pred) | max(bins) < max(pred)) {
      warning("Forecasts outside of bin range. \n")
    }
  }
  else {
    dat <- c(obs, pred)
    min.d <- min(dat)
    max.d <- max(dat)
    bins <- seq(floor(min.d), ceiling(max.d), length = 11)
  }
  lo <- min(bins)
  hi <- max(bins)
  b <- bins[-length(bins)]
  labs <- b + 0.5 * diff(bins)
  obs.cut <- cut(obs, breaks = bins, include.lowest = TRUE, labels = labs)
  obs.cut[is.na(obs.cut)] <- labs[1]
  obs.cut <- as.numeric(as.character(obs.cut))
  frcst.cut <- cut(pred, breaks = bins, include.lowest = TRUE, labels = labs)
  frcst.cut[is.na(frcst.cut)] <- labs[1]
  frcst.cut <- as.numeric(as.character(frcst.cut))
  n <- length(labs)
  lng <- aggregate(obs, by = list(frcst.cut), length)
  med <- aggregate(obs, by = list(frcst.cut), median)
  q1 <- aggregate(obs, by = list(frcst.cut), quantile, 0.25)
  q2 <- aggregate(obs, by = list(frcst.cut), quantile, 0.75)
  q1$x[lng$x <= thrs[1]] <- NA
  q2$x[lng$x <= thrs[1]] <- NA
  q3 <- aggregate(obs, by = list(frcst.cut), quantile, 0.1)
  q4 <- aggregate(obs, by = list(frcst.cut), quantile, 0.9)
  q3$x[lng$x <= thrs[2]] <- NA
  q4$x[lng$x <= thrs[2]] <- NA
  q5 <- aggregate(obs, by = list(frcst.cut), quantile, 0)
  q6 <- aggregate(obs, by = list(frcst.cut), quantile, 1)
  # q5$x[lng$x <= thrs[2]] <- NA
  # q6$x[lng$x <= thrs[2]] <- NA
  dev.off()
  par(mar = c(5, 5, 5, 5))
  plot(frcst.cut, obs.cut, xlim = c(lo, hi), ylim = c(lo, hi), main = main, ylab = ylab, xlab = xlab, type = "n", ...)
  mtext(paste0("Sample Size ",length(pred)), side = 4, adj = 0)
  legend.txt <- c("Median", "25th/75th Quantiles", "10th/90th Quantiles","0th/100th Quantiles")
  legend(min(pred) + 0.50 * diff(range(pred)), min(obs) + 0.25 * diff(range(obs)), legend.txt, col = c(2, 3, 4, 24), lty = c(1, 2, 3, 4), lwd = 3, cex = 0.7)
  abline(0, 1)

  X <- as.numeric(as.character(med$Group.1))
  lines(X, med$x, col = 2, lwd = 3)
  lines(X, q1$x, col = 3, lty = 2, lwd = 3)
  lines(X, q2$x, col = 3, lty = 2, lwd = 3)
  lines(X, q3$x, col = 4, lty = 3, lwd = 3)
  lines(X, q4$x, col = 4, lty = 3, lwd = 3)
  lines(X, q5$x, col = "grey", lty = 4, lwd = 2)
  lines(X, q6$x, col = "grey", lty = 4, lwd = 2)
  # do not plot the histogram to the bottom
  # pp <- par("plt")
  # par(plt = c(pp[1], pp[2], 0.1, 0.2))#0.2))
  # par(new = TRUE)
  # hist(frcst.cut, breaks = bins, col = "blue", main = "", axes = FALSE, xlim = c(lo, hi), xlab = " ", ylab = " ")
  # axis(4, line = 0)


}


## PLOTTING FUNCTIONS TO BE ADDED LATER
# 1) plot_points_on_a_map2 The same function with histogram included and legend outside the plotting area.
# 2) Boxplot with distribution (here, piirrettava3 is a 2d data frame with dimension names of variable and probability)
# 3) Plotting two distributions as a function of forecast length (unfortunately the values in different columns are calculated outside this plotting routine). Here "asemajoukko" separates two datasets with a unique name)
# 4) This is the same but without distribution around the plotted lines (only mean/median/etc), data frames other dimension has names of "dataset" and other "fcst_period"
# 5) selection probability heatmap of mos coefficients




