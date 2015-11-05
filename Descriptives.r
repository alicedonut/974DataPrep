setwd("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R")

source("974Master.R")

# calculate means per row


# means for gene info

groupMeans974Gene <- aggregate(cbind(factoredMaster974$B1Total,
                                        factoredMaster974$T1Total,
                                        factoredMaster974$T2Total) ~ factoredMaster974$Gene_Y1N0, FUN = mean)



# means for caff info

groupMeans974Caf <- aggregate(cbind(factoredMaster974$B1Total,
                                        factoredMaster974$T1Total,
                                        factoredMaster974$T2Total) ~ factoredMaster974$ToldCaf_Y1N0, FUN = mean)


groupMeans974Gene
groupMeans974Caf

# calculate numbers in each group using ddply from plyr package

groupRows974 <- ddply( factoredMaster974, "Group", nrow)

groupRows974

#################### Graph Gene Group Means ################################
#-------------------------------------------------------------------------------#
# now lets reassign column names
colnames(groupMeans974Gene) <- c("Gene",
                                 "Baseline",
                                 "Pre_Beverage",
                                 "Post_Beverage")


# Now we need to turn it into a long-form dataframe so we can graph it

require(tidyr)

geneCWSQMeans <- gather(groupMeans974Gene, Day, totalScore,
                                Baseline:
                                Pre_Beverage:
                                Post_Beverage)


# Now we graph it

require(ggplot2)


par(mgp = c(1,1,1))
pGroup2 <- ggplot(data = geneCWSQMeans, aes(x = Day, y = totalScore, group = Gene, shape = Gene)) +
  geom_line(aes(linetype = Gene), size = 1) +
  geom_point(size = 5, fill = "white") +
  expand_limits(y = 0) +
  scale_color_hue(name = "Gene", l = 30) +
  scale_shape_manual(name = "Gene", values = c(23,22)) +
  scale_linetype_discrete(name = "Gene") +
  scale_y_continuous(limits = c(0,60), breaks = c(0,10,20,30,40,50,60)) +
  xlab("Day") + ylab ("CWSQ Scores") +
  ggtitle("Total CWSQ Scores \n By Test Results") +
  theme_bw() 




# Fixing up graph. First create variables for each element you want to change
titleFont <- element_text(face = "bold", color = "black", size = 16, vjust = 1.5)
titleFontX <- element_text(face = "bold", color = "black", size = 16, vjust = 0.01)
axisTextFont <- element_text(face = "plain", color = "black", size = 12)
axisTextFontX <- element_text(face = "plain", color = "black", size = 14, angle = 45, vjust = 1, hjust = 1)
legendTitle <- element_text(face = "bold", color = "black", size = 14)
legendText <- element_text(face = "plain", color = "black", size = 14)


#Call up pGroup2 (which is the variable assigned to the graph) and add fix-ups
pGroup2 + theme(title = titleFont, 
                axis.title = titleFont,
                axis.title.x = titleFontX,
                axis.text  = axisTextFont,
                axis.text.x = axisTextFontX,
                legend.title = legendTitle,
                legend.text = legendText,
                ##### remove #'s to put legend inside area of graph, otherwise will automatically create legend
                #                legend.justification = c(0,0), #anchors legend in bottom left
                #                legend.position = c(.7,.1), # uses anchor point to place legend
                panel.grid.minor = element_blank(), #gets rid of gridlines
                panel.grid.major = element_blank())  #+ #(uncomment '+' if you want to add the annotate argument below) 


# adds text
annotate("text", x = 1:7,
         y = c(17, 36, 40, 36, 37, 44.5, 43),
         label = c("italic(p) == .63", # these are plotmath arguments, p in italics 
                   "italic(p) == .61",
                   "italic(p) == .72",
                   "italic(p) == .30",
                   "italic(p) == .46",
                   "italic(p) == .80",
                   "italic(p) == .29"),
         size = 4.2,
         family = "Helvetica", # specifies the font type 
         parse = T) # the parse = TRUE argument allows you to add plotmath
# arguments for mathematical notation, such as the italics


dev.print(cairo_ps, "image.eps") #### sends open plot window to current working directory


#################### Graph Instruction Group Means ################################
#-------------------------------------------------------------------------------#
# now lets reassign column names
colnames(groupMeans974Caf) <- c("Instruction",
                                 "Baseline",
                                 "Pre_Beverage",
                                 "Post_Beverage")


# Now we need to turn it into a long-form dataframe so we can graph it

require(tidyr)

cafCWSQMeans <- gather(groupMeans974Caf, Day, totalScore,
                        Baseline:
                          Pre_Beverage:
                          Post_Beverage)


# Now we graph it

require(ggplot2)


par(mgp = c(1,1,1))
pGroup2 <- ggplot(data = cafCWSQMeans, aes(x = Day, y = totalScore, group = Instruction, shape = Instruction)) +
  geom_line(aes(linetype = Instruction), size = 1) +
  geom_point(size = 5, fill = "white") +
  expand_limits(y = 0) +
  scale_color_hue(name = "Instruction", l = 30) +
  scale_shape_manual(name = "Instruction", values = c(23,22)) +
  scale_linetype_discrete(name = "Instruction") +
  scale_y_continuous(limits = c(0,60), breaks = c(0,10,20,30,40,50,60)) +
  xlab("Day") + ylab ("CWSQ Scores") +
  ggtitle("Total CWSQ Scores \n By Instruction") +
  theme_bw() 




# Fixing up graph. First create variables for each element you want to change
titleFont <- element_text(face = "bold", color = "black", size = 16, vjust = 1.5)
titleFontX <- element_text(face = "bold", color = "black", size = 16, vjust = 0.01)
axisTextFont <- element_text(face = "plain", color = "black", size = 12)
axisTextFontX <- element_text(face = "plain", color = "black", size = 14, angle = 45, vjust = 1, hjust = 1)
legendTitle <- element_text(face = "bold", color = "black", size = 14)
legendText <- element_text(face = "plain", color = "black", size = 14)


#Call up pGroup2 (which is the variable assigned to the graph) and add fix-ups
pGroup2 + theme(title = titleFont, 
                axis.title = titleFont,
                axis.title.x = titleFontX,
                axis.text  = axisTextFont,
                axis.text.x = axisTextFontX,
                legend.title = legendTitle,
                legend.text = legendText,
                ##### remove #'s to put legend inside area of graph, otherwise will automatically create legend
                #                legend.justification = c(0,0), #anchors legend in bottom left
                #                legend.position = c(.7,.1), # uses anchor point to place legend
                panel.grid.minor = element_blank(), #gets rid of gridlines
                panel.grid.major = element_blank())  #+ #(uncomment '+' if you want to add the annotate argument below) 


# adds text
annotate("text", x = 1:7,
         y = c(17, 36, 40, 36, 37, 44.5, 43),
         label = c("italic(p) == .63", # these are plotmath arguments, p in italics 
                   "italic(p) == .61",
                   "italic(p) == .72",
                   "italic(p) == .30",
                   "italic(p) == .46",
                   "italic(p) == .80",
                   "italic(p) == .29"),
         size = 4.2,
         family = "Helvetica", # specifies the font type 
         parse = T) # the parse = TRUE argument allows you to add plotmath
# arguments for mathematical notation, such as the italics


dev.print(cairo_ps, "image.eps") #### sends open plot window to current working directory


