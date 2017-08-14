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


## PLOTTING FUNCTIONS TO BE ADDED LATER
# 1) plot_points_on_a_map2 The same function with histogram included and legend outside the plotting area.
# 2) Boxplot with distribution (here, piirrettava3 is a 2d data frame with dimension names of variable and probability)
# 3) Plotting two distributions as a function of forecast length (unfortunately the values in different columns are calculated outside this plotting routine). Here "asemajoukko" separates two datasets with a unique name)
# 4) This is the same but without distribution around the plotted lines (only mean/median/etc), data frames other dimension has names of "dataset" and other "fcst_period"
# 5) selection probability heatmap of mos coefficients




