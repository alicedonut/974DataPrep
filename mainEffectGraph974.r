###############################################################################
############### graphs for main effects genes and caffeine study ##############
###############################################################################

#### read in 

rm(list=ls())

setwd("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R")

source("Descriptives.R")

caff974 <- read.csv("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/factoredMaster974.csv", stringsAsFactors = FALSE)


# create function for graphing whatever two DVs you wish with Main effects. Note: the graph filename has to be a .png

graphCaf974Main <- function (DV1, DV2, graphTitle, yLabel, graphFilename) {
  
  ### create frame just for graph. NB: we use 'substitute' function here because we are using non-character arguments (i.e. not in inverted commas) called from the function call and passing them into a function that requires inverted commas (i.e. the creation of the smallCaf974 dataframe)
  
  graph974Cols <- c("ID", "Gene_Y1N0", "ToldCaf_Y1N0", substitute(DV1), substitute(DV2)) 
  
  smallCaf974 <- caff974[, graph974Cols]
  
  ### now rename columns. We use setnames instead of rename from 'plyr' package because in the setnames     function you can use the substitute function. So we are renaming columns to the names we want displayed on the graph. This includes renaming the grouping variables
  
  require(data.table)
  
  setnames(smallCaf974, old = c("ID", "Gene_Y1N0", "ToldCaf_Y1N0", substitute(DV1), substitute(DV2)),
           new = c("ID", "gene", "info", "pre", "post"))
  
  
  
  # turn into long dataframe. We are going to use the melt function from reshape2 instead of the gather function from 'tidyr' because, again, the melt function accepts the substitute function.
  
  require(reshape2)
  
  long974 <- melt(smallCaf974, measure.vars = c("pre", "post"), var = "prePost", value.name = "Score")
  
  # change level names of each IV so they are displayed on the Axes properly
  
  require(plyr)
  
  long974$gene <- mapvalues(long974$gene, from = c("negative", "positive"), to = c("Gene \u2013", "Gene +"))
  
  long974$info <- mapvalues(long974$info, from = c("toldCaf", "toldDecaf"), to = c("Told Caffeine", "Told Decaf"))
  
  long974$prePost <- mapvalues(long974$prePost, from = c("pre", "post"), to = c("Pre-Beverage", "Post-Beverage"))
  
  
  
  # work out cell means for gene main effect
  
  cellMeans974gene <- aggregate(Score ~ gene + prePost, data = long974, mean)
  
  
  # work out cell means for info main effect
  
  cellMeans974info <- aggregate(Score ~ info + prePost, data = long974, mean)
  
  
  
  # work out SE for error bars
  
  # first write SE calculating function. This uses tapply. tapply groups the first argument by the groups in the second argument and applies the function in the third argument to each of those groups. In this function we apply the sd function to the DV, grouped by the IV, then we divide it by the square root of the n for the same DV-grouped-by-IV. Thus we get the standard error of the DV at all levels of the IV. This is similar to the aggregate function.
  
  
  groupSEs <- function (DVName, IVName) {
    se <- tapply(caff974[, DVName], caff974[,IVName], sd)/sqrt(tapply(caff974[, DVName], caff974[,IVName], length))
    return(se)
  }
  
  DVs <- c(substitute(DV1), substitute(DV2)) # here is where you might use substitute
  
  
  # sapply passes the list/vector in the first argument through one of the arguments in the function in the second. So here we pass the two DVs in turn through the standard error function we created above. The as.vector() function surrounding the sapply merely converts the matrix that comes from the sapply to a vector (sorted by columns). If you want to convert to a vector sorted by rows use 'geneSEs <- as.vector(t(sapply(DVs, function (x) groupSEs(x, "Gene_Y1N0"))))' (this is same except with t() added)
  
  geneSEs <- as.vector(sapply(DVs, function (x) groupSEs(x, "Gene_Y1N0")))
  infoSEs <- as.vector(sapply(DVs, function (x) groupSEs(x, "ToldCaf_Y1N0")))
  
  # next add the SE vectors as a column to each cellMeans data frame
  
  cellMeans974gene$SE <- geneSEs
  cellMeans974info$SE <- infoSEs
  
  
  # define the top and bottom of the error bars
  
  limits <- aes(ymax = Score + SE, ymin = Score - SE)
  
  # we need to specify how wide the objects we are dodging are
  dodge <- position_dodge(width = 0.9)
  
  # graph cell means for gene
  
  require(ggplot2)
  
  graphClustered974Gene <- ggplot(cellMeans974gene, aes(gene, Score, fill = prePost)) + #plots info as abscissa and bars grouped by pre and post
    geom_bar(stat = "identity", position = "dodge", size = 0.1) +
    geom_errorbar(limits, position = dodge, width = 0.25) +
    scale_fill_manual(values = c("#999999", "#666666")) +
    scale_y_continuous(limits = c(0, max(cellMeans974gene$Score) + max(cellMeans974gene$SE)), breaks = pretty) +
    xlab("") +
    ylab(substitute(yLabel)) + # here we are substituting the y-axis label with the argument from function above
    theme_bw()
  
  # this sets the attributes for graph elements so you can use them later for different graphs
  
  titleFont <- element_text(face = "bold", color = "black", size = 16, vjust = 1.5)
  titleFontX <- element_text(face = "bold", color = "black", size = 16, vjust = 0.01)
  axisTextFont <- element_text(face = "plain", color = "black", size = 15) # y-axis numbers size
  axisTextFontX <- element_text(face = "plain", color = "black", size = 15, angle = 45, hjust = 1, vjust = 1) # x-axis text attributes
  legendTitle <- element_text(face = "bold", color = "black", size = 14)
  legendText <- element_text(face = "plain", color = "black", size = 15)
  
  
  # now add attributes to graph object
  graphClustered974Gene2 <- graphClustered974Gene + theme(title = titleFont, 
                                                          axis.title = titleFont,
                                                          axis.title.x = titleFontX,
                                                          axis.text  = axisTextFont,
                                                          axis.text.x = axisTextFontX,
                                                          legend.position = "none",
                                                          legend.title = element_blank(),
                                                          legend.text = legendText,
                                                          panel.grid.minor = element_blank(), #gets rid of gridlines
                                                          panel.grid.major = element_blank()) 
  
  
  
  
  
  
  
  
  
  # graph cell means for info
  
  graphClustered974Info <- ggplot(cellMeans974info, aes(info, Score, fill = prePost)) + #plots info as abscissa and bars grouped by pre and post
    geom_bar(stat = "identity", position = "dodge", size = 0.1) +
    geom_errorbar(limits, position = dodge, width = 0.25) +
    scale_fill_manual(values = c("#999999", "#666666")) +
    scale_y_continuous(limits = c(0, max(cellMeans974info$Score)+ max(cellMeans974info$SE)), breaks = pretty) +
    xlab("") +
    ylab("") + 
    theme_bw()
  
  # add attributes using objects created in graph above
  graphClustered974Info2 <- graphClustered974Info + theme(title = titleFont, 
                                                          axis.title = titleFont,
                                                          axis.title.x = titleFontX,
                                                          axis.text  = axisTextFont,
                                                          axis.text.x = axisTextFontX,
                                                          legend.position = "none",
                                                          legend.title = element_blank(),
                                                          legend.text = legendText,
                                                          panel.grid.minor = element_blank(), #gets rid of gridlines
                                                          panel.grid.major = element_blank()) 
  
  
  
  # Now we want to have a common legend for both the graphs. We have already 'switched off' their legends so that now they are the same width. There are several steps in creating the legend. We are going to treat the legend as a separate grob (an graphical object). 
  
  
  # Now you are making a dummy graph whose legend you will 'steal' to turn into a grob
  
  graphClustered974Dummy <- ggplot(cellMeans974info, aes(info, Score, fill = prePost)) + #plots info as abscissa and bars grouped by pre and post
    geom_bar(stat = "identity", position = "dodge", size = 0.1) +
    scale_fill_manual(values = c("#999999", "#666666")) +
    scale_y_continuous(limits = c(0, max(cellMeans974info$Score)+ max(cellMeans974info$SE)), breaks = pretty) +
    xlab("") +
    ylab("") + 
    theme_bw()
  
  # add attributes using objects created in graph above
  graphClustered974Dummy <- graphClustered974Dummy + theme(title = titleFont, 
                                                           axis.title = titleFont,
                                                           axis.title.x = titleFontX,
                                                           axis.text  = axisTextFont,
                                                           axis.text.x = axisTextFontX,
                                                           legend.position = "right",
                                                           legend.title = element_blank(),
                                                           legend.text = legendText,
                                                           panel.grid.minor = element_blank(), #gets rid of gridlines
                                                           panel.grid.major = element_blank()) 
  
  
  
  #### 1. Save the legend of one of the dummy graph as a grob. The important bit here is that the legend.position = "right' instead of "none' so we can steal this grob. Secondly the legend.title = element-blank(), which means we 'switched off' the title of the legend.
  
  get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") # this gets the list index for the legend grob by searching through all the grobs in the tmp object for the name 'guide-box'
    legend <- tmp$grobs[[leg]] #this calls that list index
    return(legend) # this returns it
  }
  
  # now we assign the legend to an object
  legend <- get_legend(graphClustered974Dummy) # noticw we are using the original, not graphClustered974Info2, because in graphClustered974Info2 we switched off the legend. So we need to get out grob from an object where we didn't do that.
  
  
  
  
  #### Now we're going to make a side by side graph using the gridExtra package
  require(grid)
  require(gridExtra)
  
  
  # put two cellmean objects together to make sure the common scale doesn't exclude anything
  
  meansMain974 <- c(cellMeans974info$Score, cellMeans974gene$Score) 
  SEMain974 <- c(cellMeans974info$SE, cellMeans974gene$SE)
  
  # define y-axis attributes that will be common to both graphs
  
  library(plyr) # so we can use the round_any function
  
  # now we write a function for defining limits and breaks for axis based on what the maximum height of the top SE bar is
  
  axisFunct <- function (x) {
    if (x <= 1) {
      limitMain <- c(0,1)
      breaks <- c(seq(from = 0, to = 1, by = 0.2))
    } else if (x >= 1 & x < 5) {
      limitMain <- c(0, round_any(x, 1, f = ceiling))
      breaks <- c(seq(from = 0, to = round_any(x, 1, f = ceiling), by = 1)) 
    } else if (x >= 5 & x < 10) {
      limitMain <- c(0, round_any(x, 2, f = ceiling))
      breaks <- c(seq(from = 0, to = round_any(x, 2, f = ceiling), by = 2))
    } else if (x >= 10 & x < 15) {
      limitMain <- c(0,15)
      breaks <- c(0, 5, 10, 15)
    } else if (x >= 15 & x < 20) {
      limitMain <- c(0,20)
      breaks <- c(seq(from = 0, to = 20, by = 5))
    } else if (x >= 20 & x < 35) {
      limitMain <- c(0, round_any(x, 5, f = ceiling))
      breaks <- c(seq(from = 0, to = round_any(x, 5, f = ceiling), by = 5))
    } else {
      limitMain <- c(0, round_any(x, 10, f = ceiling))
      breaks <- pretty
    }
    
    maxBreaks <- list(limitMain, breaks) # you can't return two objects at the end of a function so you need to put them in a list
    
    return(maxBreaks) # then call the list
    
  } # end of axisFunct
  
  #### now we need to work out the maximum value/height of the error bars in the graphs so that we can define the limits and breaks in the y-axis
  
  maxVal <- round_any( max(meansMain974) + max(SEMain974), 1, f = ceiling)
  
  # assign common axis to both plots. The values subsituted in the limits and breaks arguments of scale_y_continuous are the first and second elements of the list returned at the end of the axis scaling function above (when indexing a list use [[]] instead of [])
  
  geneCommonY <- graphClustered974Gene2 + scale_y_continuous(limits = axisFunct(maxVal)[[1]], breaks = axisFunct(maxVal)[[2]])
  infoCommonY <- graphClustered974Info2 + scale_y_continuous(limits = axisFunct(maxVal)[[1]], breaks = axisFunct(maxVal)[[2]])
  
  #now build the plots
  geneCommonY <- ggplot_gtable(ggplot_build(geneCommonY))
  infoCommonY <- ggplot_gtable(ggplot_build(infoCommonY))
  
  # copy the plot height from one graph to the other to make sure they're the same
  infoCommonY$heights <- geneCommonY$heights
  
  # arranges the graphs into a grid using gridExtra. Notice the grob is an object just like the graphs and must be listed as a separate column
  
  png(paste("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/graphs/mainEffectsGraphs/", graphFilename, sep = ""), type = "quartz", height = 480, width = 700) # can't use ggsave here because it doesn't accept gridExtra objects
  gridComboMain974 <- grid.arrange(geneCommonY, infoCommonY, legend, ncol = 3, widths = c(1,1,.5),
                                   top = textGrob(substitute(graphTitle), gp=gpar(fontsize=20,font=2)))
  dev.off()

  
  return(gridComboMain974) 
  
  
  
 
} # close function

 
