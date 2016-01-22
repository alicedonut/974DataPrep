


###################################################################################
##------------------start with the main effect graph-----------------------------##
###################################################################################


setwd("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R")

source("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/mainEffectGraph.R")


# Now we call the graph function. Arguments are:
# a) pre-beverage DV
# b) post-beverage DV
# c) title for the graph as a whole
# d) y-axis label
# e) filename you want graph saved as (must have .png suffix)

# NOTE: all arguments must be in inverted commas

graphCaf974Main("T1Total", "T2Total", "Change in Total Caffeine Withdrawal" ,"CWSQ Score", "totCaffWithdrawalMainEffects974.png")

graphCaf974Main("T1DrowsyFac", "T2DrowsyFac", "Change in CWSQ Drowsy Factor", "CWSQ Drowsy", "drowsyMainEffects.png")

graphCaf974Main("T1DecAlertFac", "T2DecAlertFac", "Change in CWSQ Decreased Alertness Factor", "CWSQ Alertness", "alertnessMainEFfects.png")

graphCaf974Main("T1MoodFac", "T2MoodFac", "Change in CWSQ Mood Factor", "CWSQ Mood", "mood MainEffects.png")

graphCaf974Main("T1DecreasedSocMotivFac", "T2DecreasedSocMotivFac", "Change in CWSQ Decreased\n Desire to Socialise/Motivation Factor", "CWSQ Social/Motiv", "socialMotivationalMainEffects.png")

graphCaf974Main("T1NauseaFac", "T2NauseaFac", "Change in CWSQ Nausea Factor", "CWSQ Nausea", "nauseaMainEffects.png")

graphCaf974Main("T1FluLikeFac", "T2FluLikeFac", "Change in CWSQ Flu-like Feelings Factor", "CWSQ Flu-Like Feelings", "fluLikeMainEffects.png")

graphCaf974Main("T1HeadacheFac", "T2HeadacheFac", "Change in CWSQ Headache Factor", "CWSQ Headache", "headacheMainEffects.png")

graphCaf974Main("T1AcuteFac", "T2AcuteFac", "Change in CWSQ Acute Effects of Caffeine Factor", "CWSQ Acute EFfects", "acuteMainEffects.png")

graphCaf974Main("T1CravingFac", "T2CravingFac", "Change in CWSQ Craving", "CWSQ Craving", "cravingMainEffects.png")

graphCaf974Main("Pre_Systolic", "Post_Systolic", "Change in Systolic BP", "mmHg", "systolicMainEffects.png")

graphCaf974Main("Pre_Diastolic", "Post_Diastolic", "Change in Diastolic BP", "mmHg", "diastolicMainEffects.png")

graphCaf974Main("T1sumhit", "T2sumhit", "Change in RVIP Hits", "Hits", "RVIPHitMainEffects.png")

graphCaf974Main("T1sumfa", "T2sumfa", "Change in RVIP False Alarm Rate", "False Alarms", "RVIPFalsealarm.png")

graphCaf974Main("T1sumacc", "T2sumacc", "Change in RVIP Accuracy Score", "Accuracy", "RVIPAccuracyMainEffects.png")

g2IVMainrt <- graphCaf974Main("T1rt", "T2rt", "Change in RVIP Reaction Time", "ms", "RVIPrtMainEFfects.png")




# Now we call the interactions. Enter whatever two variables (pre-post) you want in inverted commas and come up with a title for the y-axis label (also in inverted commas)

rm(list=ls())

source("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/interactionGraph.R")


graphCaf974Interaction("T1Total", "T2Total", "Change in Total Caffeine Withdrawal" ,"CWSQ Score", "totWithdrawalInt.png")

graphCaf974Interaction("T1DrowsyFac", "T2DrowsyFac", "Change in CWSQ Drowsy Factor", "CWSQ Drowsy", "drowsyInt974.png")

graphCaf974Interaction("T1DecAlertFac", "T2DecAlertFac", "Change in CWSQ Decreased Alertness Factor", "CWSQ Alertness", "alertInt974.png")

graphCaf974Interaction("T1MoodFac", "T2MoodFac", "Change in CWSQ Mood Factor", "CWSQ Mood", "moodInt974.png")

graphCaf974Interaction("T1DecreasedSocMotivFac", "T2DecreasedSocMotivFac", "Change in CWSQ Decreased Desire to Socialise/Motivation Factor", "CWSQ Social/Motiv", "socMovInt974.png")

graphCaf974Interaction("T1NauseaFac", "T2NauseaFac", "Change in CWSQ Nausea Factor", "CWSQ Nausea", "nauseaInt974.png")

graphCaf974Interaction("T1FluLikeFac", "T2FluLikeFac", "Change in CWSQ Flu-like Feelings Factor", "CWSQ Flu-Like Feelings", "fluLikeInt974.png")

graphCaf974Interaction("T1HeadacheFac", "T2HeadacheFac", "Change in CWSQ Headache Factor", "CWSQ Headache", "headacheInt974.png")

graphCaf974Interaction("T1AcuteFac", "T2AcuteFac", "Change in CWSQ Acute Effects of Caffeine Factor", "CWSQ Acute EFfects", "acuteInt974.png")

graphCaf974Interaction("T1CravingFac", "T2CravingFac", "Change in CWSQ Craving Factor Due to Info About \n Caffeine Content and Presence of Gene", "CWSQ Craving", "cravingInt974.png")

graphCaf974Interaction("Pre_Systolic", "Post_Systolic", "Change in Systolic BP", "mmHg")

graphCaf974Interaction("Pre_Diastolic", "Post_Diastolic", "Chang in Diastolic BP", "mmHg", "diastolicInt974.png")

graphCaf974Interaction("T1sumhit", "T2sumhit", "Change in RVIP Hits", "Hits", "RVIPhitInt974.png")

graphCaf974Interaction("T1sumfa", "T2sumfa", "Change in RVIP False Alarm Rate", "False Alarms", "RVIPfaInt974.png")

graphCaf974Interaction("T1sumacc", "T2sumacc", "Change in RVIP Accuracy Score", "Accuracy", "RVIPacc974.png")

graphCaf974Interaction("T1rt", "T2rt", "Change in RVIP Reaction Time", "ms", "RVIPrtInt974.png")

