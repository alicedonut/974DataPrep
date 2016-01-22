###############################################################################
###################### graphs for genes and caffeine study ####################
###############################################################################

#### read in 

rm(list=ls())

setwd("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R")

source("Descriptives.R")

caff974 <- read.csv("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/factoredMaster974.csv", stringsAsFactors = FALSE)


# create function for graphing whatever two DVs you wish


graphCaf974Interaction <- function (DV1, DV2, graphTitle, yLabel, graphFilename) {
  
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
  
  
  
  # work out cell means 
  
  cellMeans974 <- aggregate(Score ~ gene + info + prePost, data = long974, mean)
  
  # now we are going to pass the se for each cell into the object cellMeans

  
  # next we will work out the se for each of the 8 cells (2 x 2 x 2). The following command comes up with a standard error for each of the 8 cells. The numerator here gives applies the with function. The with function's first argument gives the name of the dataframe. The second argument is a function to be applied to that database. Here the second argument/function is 'tapply(Score, list(gene, info, prePost), sd)'. This divides the dataframe along three dimensions and gives the sd (on Score) for all 8 unique groups (note: 'tapply(long974[,"Score"], list(long974[,"gene"], long974[,"info"], long974[,"prePost"]), sd))' does exactly the same thing). The denominator is the same form of function except it calculates the square root of the number in each cell. 
  
  threeIVSE <- with(long974, tapply(Score, list(gene, info, prePost), sd))/sqrt(with(long974, tapply(Score, list(gene, info, prePost), length)))
  

# so first dim is gene(1 = ?, 2 = ?), second dim is info, third is prePost(1 = pre, 2 = post). So the first call 'preSEs' vectorises the pre-bev matrix by col (which is achieved by inlcuding only the 1 as the third argument in the 3d matrix), which is how the first four cells of the cellMeans974 frame is set up. The second call does the same thing for the post-bev frame.
  preSEs <- as.vector(threeIVSE[1:2,1:2,1])
 postSEs <- as.vector(threeIVSE[1:2,1:2,2])
 
#  # put them together and add them to the cellMeans dataframe
 
 cellMeans974$SEs <- c(preSEs, postSEs)
 
  
  #  # define the top and bottom of the error bars
  
  limits <- aes(ymax = Score + SEs, ymin = Score - SEs)
  
  # we need to specify how wide the objects we are dodging are
  dodge <- position_dodge(width = 0.9)
  
  # graph cell means
  
  require(ggplot2)
  
  graphClustered974 <- ggplot(cellMeans974, aes(info, Score, fill = prePost)) + #plots info as abscissa and bars grouped by pre and post
    geom_bar(stat = "identity", position = dodge, size = 0.1) +
    facet_wrap(~gene, nrow = 1) + #splits into two graphs: gene negative and gene positive
    geom_errorbar(limits, position = dodge, width = 0.25) +
    scale_fill_manual(values = c("#999999", "#666666")) +
    scale_y_continuous(limits = c(0, max(cellMeans974$Score)+ max(cellMeans974$SEs)), breaks = pretty) +
    ggtitle(substitute(graphTitle)) +
    xlab("") +
    ylab(substitute(yLabel)) + # here we are substituting the y-axis label with the argument from function above
    theme_bw()
  
  
  titleFont <- element_text(face = "bold", color = "black", size = 16, vjust = 1.5)
  titleFontX <- element_text(face = "bold", color = "black", size = 16, vjust = 0.01)
  axisTextFont <- element_text(face = "plain", color = "black", size = 13) # y-axis numbers size
  axisTextFontX <- element_text(face = "plain", color = "black", size = 14, angle = 45, hjust = 1, vjust = 1) # x-axis text attributes
  legendTitle <- element_text(face = "bold", color = "black", size = 14)
  legendText <- element_text(face = "plain", color = "black", size = 13)
  facetHeadingText <- element_text(face = "plain", color = "black", size = 15)
 

   

  
  # now we call the graph with all the attributes attached
  graphClustered974 <- graphClustered974 + theme(title = titleFont, 
                                                 axis.title = titleFont,
                                                 axis.title.x = titleFontX,
                                                 axis.text  = axisTextFont,
                                                 axis.text.x = axisTextFontX,
                                                 legend.position = "right",
                                                 legend.title = element_blank(),
                                                 legend.text = legendText,
                                                 panel.grid.minor = element_blank(), #gets rid of gridlines
                                                 panel.grid.major = element_blank(),
                                                 strip.text.x = facetHeadingText) # strip.text.x determines the attributes of the group split determined by facet_wrap
  # now we save the graphs
  ggsave(paste("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/graphs/interactionGraphs/", graphFilename, sep = ""), plot = graphClustered974, device = "png", height = 5, width = 7)
  dev.off()
  
  return(graphClustered974)
  
} # close function


