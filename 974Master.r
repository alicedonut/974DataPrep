setwd("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R")

library(reshape2)
library(plyr)
library(lubridate)
library(stringr)
library(data.table)
library(pathological)


############################ Building data files for 974 ##################### 


##################### 1. Master file, taken from the xlsx master file. We want to turn it into an object we can attach.
master974 <- read.csv("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/Caff974Allocation.csv")

# Remove the rows that have incomplete data (as defined by whether I have excluded them or not)
master974 <- master974[!is.na(master974$Exclude_Y1N0),]

# remove the superfluous 'X' column which R wants to read in for some reason even though it's blank
# master974 <- master974[, -which(names(master974) %in% "X")]

#### turn grouping variables into factors # 
master974$Paid_Y1N0 <- factor(master974$Paid_Y1N0, 
                             levels = c(1, 2),
                             labels = c("paid", "coursecredit"),
                             ordered = FALSE)

master974$Group <- factor(master974$Group,
                          levels = c(1, 2, 3, 4),
                          labels = c("negative_decaf", "negative_caff", "positive_decaf", "positive_caff"),
                          ordered = F)

master974$Gene_Y1N0 <- factor(master974$Gene_Y1N0,
                              levels = c(1,0),
                              labels = c("positive", "negative"),
                              ordered = F)

master974$ToldCaf_Y1N0 <- factor(master974$ToldCaf_Y1N0,
                                 levels = c(1,0),
                                 labels = c("toldCaf", "toldDecaf"),
                                 ordered = F)

master974$Screened_Y1N0 <- factor(master974$Screened_Y1N0,
                                 levels = c(1, 0),
                                 labels = c("screened", "notScreened"),
                                 ordered = F)

master974$CoffeeType_BNS1BWS2WNS3WWS4 <- factor(master974$CoffeeType_BNS1BWS2WNS3WWS4,
                                                levels = c(1,2,3,4),
                                                labels = c("blackNoSugar", "blackWithSugar", "whiteNoSugar", "whiteWithSugar"),
                                                ordered = F)

master974$Gender_F1M0 <- factor(master974$Gender_F1M0, levels = c(1,0),
                                labels = c("female", "male"),
                                ordered = F)

master974$Verbal_CompliedY1N0 <- factor(master974$Verbal_CompliedY1N0,
                                        levels = c(1,0),
                                        labels = c("complied", "didNotComply"),
                                        ordered = F) 

master974$Verbal_KnowSuspectY1N0 <- factor(master974$Verbal_KnowSuspectY1N0,
                                        levels = c(1,0),
                                        labels = c("suspicious", "notSuspicious"),
                                        ordered = F) 

master974$Exclude_Y1N0 <- factor(master974$Exclude_Y1N0,
                                 levels = c(1, 0),
                                 labels = c("exclude", "include"),
                                 ordered = F)


# write.csv(master974, "~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/master974.csv", row.names = FALSE)

###############################################################################################
############## Script for multiple imputation of blood pressure data for 974 ##################
###############################################################################################

# we are going to impute variables for the missing variables in the blood pressure data. Then we are going to replace those variables in for the missing values. We are going to do this at the compilation stage in the master compile so that the original data (see 'CaffAllocation974.csv') with missing values remains intact. We are going to use knnImputation from the DMwR package rather than multiple imputation using MICE. Multiple imputation is great but you can only do it using lm and it would be better to analyse this data using a mixed model with lmer utilising random intercepts. While MICE does offer a complete function like the knnImputation function below you are forced to choose one of the five imputed datasets and there is no definitive way of choosing which of the five (at least that I have been taught). So instead we are going to replace the missing values with imputed values and run the mixed-effects model on that data. 

# We could probably just have run the knnImputation function on the relevant columns of the master974 dataframe but I thought it better to convert it to long first and do it that way. Should check whether that made any difference.

bpCols <- expand.grid( session = c("Baseline_", "Pre_", "Post_"), hr = c("Systolic", "Diastolic"))

bpColsKeep <- paste(bpCols$session, bpCols$hr, sep = "") 

bpColsKeep <- append(bpColsKeep, c("ID", "substr_ID_974"), 0)

# We get a summary of how many blood pressure variables have NAs. First we subset the data by the columns we want. %in% means 'match'. So the '(names(factoredMaster974) %in% bpColsKeep)' returns a logical vector of the column names in the factoredMaster974 dataframe that match the names listed in the bpColsKeep. The advantage of this method is that it orders the subsetted dataframe in the order that the columns appear in the original, rather than 'data2 <- factoredMaster974[,bpColsKeep] which reorder the columns alphabetically left to right

wideBP <- master974[, which(names(master974) %in% bpColsKeep)]

#_______________________________________NOTE_____________________________________________#

row.names(wideBP) <- wideBP$ID # This allocated the ID number for each participant to the rownames for this mini dataset. This is super important because otherwise it will order things numerically by alphabetised surname instead of according to ID number. This will hurt us downstream unless we're on top of it because the imputed data is based on row number and we need the right one. 

#________________________________________________________________________________________#

# now we're going to convert to a person-period dataframe from the wide person-level one
library(plyr)

# so we need to create person-level data frames. Each one we get by removing the variables we don't want (essentially so we can keep the ID and substr_ID_974 variables). The reason we want to do this is that we would like the imputation to take into account the timepoint that each missing value should have been measured at.
bpSys <- wideBP[, -which(names(wideBP) %in% c("Baseline_Diastolic", "Pre_Diastolic", "Post_Diastolic"))]
bpDia <- wideBP[, -which(names(wideBP) %in% c("Baseline_Systolic", "Pre_Systolic", "Post_Systolic"))]


# now we are going to melt them
bpSysLong <- melt(bpSys, measure.vars = c("Baseline_Systolic", "Pre_Systolic", "Post_Systolic"), var = "Systolic", value.name = "bpm_Sys") 

bpDiaLong <- melt(bpDia, measure.vars = c("Baseline_Diastolic", "Pre_Diastolic", "Post_Diastolic"), var = "Diastolic", value.name = "bpm_Dia")  

# we need to introduce a time variable
bpSysLong$timeSys <- ifelse(bpSysLong$Systolic == "Baseline_Systolic", 0, ifelse(bpSysLong$Systolic == "Pre_Systolic", 1, 2)) 

bpDiaLong$timeDia <- ifelse(bpDiaLong$Diastolic == "Baseline_Diastolic", 0, ifelse(bpDiaLong$Diastolic == "Pre_Diastolic", 1, 2)) 


# now merge the dataframes
bpLong <- data.frame(bpSysLong, bpDiaLong)

# remove the duplicate rows
bpLong <-  bpLong[, -which(names(bpLong) %in% c("ID.1", "substr_ID_974.1"))]


#########################################################################################################
######################################## Imputation #####################################################
#########################################################################################################

# these rows are missing from each
listBPNAs <- sapply(c("bpm_Sys", "bpm_Dia"), function (x) which(is.na(bpLong[,x]))); listBPNAs

# load the package DMwR, which is the package we will use for imputing the missing values using knnImputation. This imputation is going to be no-flab. If you want to see about how to do all the decision making about what rows to remove and which to keep, and about output tables for decisions, see the multiple imputation folder in the Statistics folder under PhD
library(DMwR)

# the knnImputation function uses a weighted mean of the closest neighbouring variables. As the distance from the datapoint of interest increases each neighbouring variable is given a lesser weight in the imputation, so the closer the neighbour the more weight it is given. Note: if you select the method argument you can choose median instead of mean e.g. bpLong <- knnImputation(bpLong, k = 10, meth = "median").

bpLongImp <- knnImputation(bpLong, k = 10) # this is the imputed dataframe

bpLongImp == bpLong # this is for checking. The 'NA's returned here should be in the same place as the NAs in the original dataframe, indicating that they have been replaced


# this will list the imputed values for all the missing value (the t transposes the dataframe). We will eventually use this as a reference dataframe, contining the replacement values themselves and the row numbers of the bpLong dataset where these replacement values will go. 
imputedBPVals <- data.frame(t(sapply(1:nrow(listBPNAs), function (x) bpLongImp[listBPNAs[x,1], colnames(listBPNAs)]))); imputedBPVals

# this will replace the row.names of the dataframe above with the ID numbers of the people who had the missing data. So now we have a data frame that has the ID numbers of the missing values and the actual replacement values themselves. We could replace them into the data frame. Note we can only do this this way, listing a single column as the column reference for both, because blood pressure data was delivered, or NOT delivered in the case of the NAs, in systolic/diastolic pairs.
row.names(imputedBPVals) <- listBPNAs[,"bpm_Sys"]; imputedBPVals

# now we do a for loop where we call the 1st row name of the imputedBPVals reference dataset, convert it to a number, and use it as the row index for the bpLong set. In combination with the bpm_Sys column this is the location on the bpLong dataset of the first missing value. This is all done with the 'bpLong[as.numeric(row.names(imputedBPVals)[i]),"bpm_Sys"]' section. Into this location (previously an NA, we pass the imputed value for this missing value from the imputedBPVals dataframe ('imputedBPVals[i,"bpm_Sys"]'). We do this for all six imputed values in the bpm_Sys column. We then do the same for the bpm_Dia column. 

for (i in 1:nrow(imputedBPVals)) {
  bpLong[as.numeric(row.names(imputedBPVals)[i]),"bpm_Sys"] <- imputedBPVals[i,"bpm_Sys"]
  bpLong[as.numeric(row.names(imputedBPVals)[i]),"bpm_Dia"] <- imputedBPVals[i,"bpm_Dia"]
}

bpLong # there should now be no more missing values in this dataframe


########################### convert back to wide frame ########################################## 

# First we need to split the long data frame into two, reshape each of these into wide, then merge. 

# first split. We don't need the time variable any more
bpLongSysImp <- bpLong[, which(names(bpLong) %in% c("ID", "substr_ID_974", "Systolic", "bpm_Sys"))]
bpLongDiaImp <- bpLong[, which(names(bpLong) %in% c("ID", "substr_ID_974", "Diastolic", "bpm_Dia"))]

# now recast. Note we can use two variables as our id variables.
bpSysWide <- reshape(bpLongSysImp, idvar = c("ID", "substr_ID_974"), timevar = "Systolic", direction = "wide", sep = "")
bpDiaWide <- reshape(bpLongDiaImp, idvar = c("ID", "substr_ID_974"), timevar = "Diastolic", direction = "wide", sep = "")  

# check that the cols haven't been reordered somehow  
bpSysWide$substr_ID_974 == bpDiaWide$substr_ID_974

# rename cols

wideBPColNamesSys <- c("ID", "substr_ID_974", "Baseline_Systolic", "Pre_Systolic", "Post_Systolic")
wideBPColNamesDia <- c("ID", "substr_ID_974", "Baseline_Diastolic", "Pre_Diastolic", "Post_Diastolic")


colnames(bpSysWide) <- wideBPColNamesSys
colnames(bpDiaWide) <- wideBPColNamesDia


# now we can merge the two dataframes

impWide974 <- merge(bpSysWide, bpDiaWide, by = "ID")


# now subset and reorder
impWide974 <- impWide974[, c("Baseline_Systolic", "Baseline_Diastolic", "Pre_Systolic", "Pre_Diastolic", "Post_Systolic", "Post_Diastolic")]

# round impWide974 to two digits
impWide974 <- data.frame(sapply(colnames(impWide974), function (x) as.numeric(format(round(impWide974[,x], digits = 2), nsmall = 2))))

# subset the relevant columns from the original dataframe
checkBPColsMaster974 <- master974[, c("Baseline_Systolic", "Baseline_Diastolic", "Pre_Systolic", "Pre_Diastolic", "Post_Systolic", "Post_Diastolic")]

# now we check them
impWide974 == checkBPColsMaster974 # should only be NAs in the spots where there are missing values

# now we sub in the imputed columns
master974[, c("Baseline_Systolic", "Baseline_Diastolic", "Pre_Systolic", "Pre_Diastolic", "Post_Systolic", "Post_Diastolic")] <- impWide974

# one last check. Should return 'integer(0)'
which(is.na(master974[, c("Baseline_Systolic", "Baseline_Diastolic", "Pre_Systolic", "Pre_Diastolic", "Post_Systolic", "Post_Diastolic")]))


############################### Add RVIP to master using raw RVIP files from the folder ##########################


# This will create a character vector of all the RVIP files. All the file names this needs to have the full.names = T because this specification includes the file path in the name, which is essential
oldRVIPNames <- list.files("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/RVIP_Raw", full.names = T)

# the replace-extension command is from pathological package. But we don't really need to replace the extension because the read.delim function in the for-loop below reads the .iqdat files just fine
#newRVIPNames <- replace_extension(oldRVIPNames, "csv", include_dir = T)

# here we are creating empty variables to put the first entry of the for-loop into
RT <- NULL
dateRVIPRaw <- NULL
timeRVIPRaw <- NULL
sumHitRVIPRaw <- NULL
sumFARVIPRaw <- NULL
subjectRVIPRaw <- NULL

# this is the for-loop
for (n in 1:length(oldRVIPNames)) { #specifies a loop the same length as the number of elements in the oldRVIPNames vector of data frame names
  singleSet <- read.delim(oldRVIPNames[n]) # reads each data frame in turn to an object we have called singleSet
  RT[n] <- mean(subset(singleSet, values.Hit == 1)$latency)# this returns entries in the response latency column in each dataframe for each RVIP session for only the rows where the participant scored a hit
  dateRVIPRaw[n] <- as.character(singleSet$date[1])
  timeRVIPRaw[n] <- as.character(singleSet$time[1])
  sumHitRVIPRaw[n] <- max(singleSet$values.SumHit)
  sumFARVIPRaw[n] <- max(singleSet$values.SumFA)
  subjectRVIPRaw[n] <- as.character(singleSet$subject[1])
} 

# the for loop below does the same job just less efficiently
# 
# RT <- NULL # this gives the for-loop an object to copy the first value into. Neither of these will work without it 
# 
# for (singleSet in oldRVIPNames) {
#   singleS <- read.delim(singleSet)
#   RT1 <- mean(subset(singleS, values.Hit == 1)$latency)
#   RT <- append(RT, RT1)
# }
# RT


RVIP974frame <- data.frame(cbind(subjectRVIPRaw, timeRVIPRaw, dateRVIPRaw, sumHitRVIPRaw, sumFARVIPRaw, RT), stringsAsFactors = F)


# write.csv(RVIP974frame, "~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/RVIPRaw.csv", row.names = FALSE)


####### merging date and time columns

# this adds 0 to front of date vector so that it is in the correct format
RVIP974frame$dateRVIPRaw <- as.Date(ifelse(nchar(RVIP974frame$dateRVIPRaw)==5, # if no of char is 5
                                                 paste("0", RVIP974frame$dateRVIPRaw, sep=""), # add a 0
                                                 RVIP974frame$dateRVIPRaw), # otherwise keep it as-is
                                                 "%m%d%y") # convert it to the following format (part of as.Date function argument)

# this merges the columns into one
RVIP974frame <- within(RVIP974frame, { dateTime = format(as.POSIXct(paste(RVIP974frame$dateRVIPRaw, RVIP974frame$timeRVIPRaw)), "%Y-%m-%d %H:%M:%S") })

# removes now-superfluous columns
RVIP974frame <- RVIP974frame[, -which(names(RVIP974frame) %in% c("dateRVIPRaw", "timeRVIPRaw"))]

# from lubridate package turns new dateTime column into POSIX object
RVIP974frame$dateTime <- parse_date_time(RVIP974frame$dateTime, "%Y%m%d %H%M%S")



# rename columns with rename function from plyr

RVIP974frame <- rename(RVIP974frame, c("subjectRVIPRaw"="ID",
                                       "sumHitRVIPRaw"="sumhit",
                                       "sumFARVIPRaw"="sumfa",
                                       "RT"="rt",
                                       "dateTime"="datetime"))



# calculate accuracy score

RVIP974frame$sumfa <- as.numeric(RVIP974frame$sumfa) # convert from string to numeric
pFA <- RVIP974frame$sumfa/128.67

RVIP974frame$sumhit <- as.numeric(RVIP974frame$sumhit) # convert from string to numeric
pHit <- RVIP974frame$sumhit/38

RVIP974frame$sumacc <- (pHit - pFA)/ (1 - pFA)

# convert rt to numeric

RVIP974frame$rt <- as.numeric(RVIP974frame$rt)

# round all numeric variables to 2 decimal places. The function argument passed into sapply 'is.numeric' causes r to search the RVIP974frame dataframe for any columns that are numeric. the round function on the right rounds any columns which are numeric to 2 decimal places.

RVIP974frame[, sapply(RVIP974frame, is.numeric)] <- round(RVIP974frame[, sapply(RVIP974frame, is.numeric)],2)


#from here you can pass this frame onto the ddply function using ID number as reference variable

# ddply applies a function to a subset of a dataframe. ID is the variable we are subsetting by. Here we are creating a new column (i.e. 'daycode') with each entry as the rank order of that row within the subset that row belongs to (in this case ID).

RVIP974Long <- ddply(RVIP974frame,                     
                     "ID", 
                     function(subsetRows) {
                       subsetRows$daycode <- order(subsetRows$datetime) 
                       return(subsetRows)
                     }
)

#View(RVIP974Long)

# changes day codes to something sensible 
RVIP974Long$daycode <- mapvalues(RVIP974Long$daycode, from = c(1, 2, 3),
                                 to = c("B1", "T1", "T2"))

# reorder columns and simultaneously, via its omission, remove 'datetime' column (no longer useful since daycode serves this purpose)

RVIP974Long <- RVIP974Long[c("ID", "daycode", "sumhit", "sumfa", "sumacc", "rt")]

# reshape into wide format

RVIP974Wide <- reshape(RVIP974Long, idvar = "ID", timevar = "daycode", direction = "wide", sep = "")

# make a vector of all column names except ID
RVIP974ColNames <- colnames(RVIP974Wide[,-which(names(RVIP974Wide) %in% "ID")])

# search for non-lowercase characters. when found put them at front
RVIP974ColNamesSub <- gsub("(.*)([A-Z][0-9])$", "\\2\\1", RVIP974ColNames)

# rename col names 1st arg is dataframe, second is old names, last is new names.
setnames(RVIP974Wide, RVIP974ColNames, RVIP974ColNamesSub)

# write.csv(RVIP974Wide, "~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/RVIP974Wide.csv", row.names = FALSE)

################### merge RVIP files with master csv file ############################# 


mastList974 <- list(master974, RVIP974Wide)


masterCSV974 <- Reduce( function (...) merge(..., by = "ID", all = F), mastList974)


# write.csv(masterCSV974, "~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/master.csv")















########################### 2. Demographics questionnaire ###############################

all_content_Demog974 <- readLines("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/Demographics_and_Caffeine_Use_Questionnaire_974.csv") # this reads in the qualtrics file and converts it to an object


skip_second_Demog974 <- all_content_Demog974[-2] # removes second line with unneeded qualtrics headings

demog974 <- read.csv(textConnection(skip_second_Demog974), header = TRUE, stringsAsFactors = FALSE) # creates new object from the csv file, which has the second line removed

# this uses the rename function in the plyr package to rename the variables
demog974 <- rename(demog974, c("Q1"="genderM1F0",
                               "Q2"="age",
                               "Q3_1"="emp_FT",
                               "Q3_2"="emp_Casual",
                               "Q3_3"="emp_Vol",
                               "Q3_4"="emp_UnEmp",
                               "Q3_5"="emp_Student",
                               "Q3_6"="emp_Retired",
                               "Q3_7"="emp_Other",
                               "Q3_7_TEXT"="emp_OtherTxt",
                               "Q4"="edLevel",
                               "Q4_TEXT"="edLevel_TEXT",  	
                               "Q5"="maritalStat",
                               "Q6"="ethnicity",
                               "Q7"="langOtherEng",
                               "Q7_TEXT"="langOtherEng_TEXT",
                               "Q8"="language",
                               "Q9"="coffeeEveryday",
                               "Q11"="numCupsCoffee",
                               "Q15"="teaEveryday",
                               "Q17"="numCupsTea",
                               "Q20"="colaEveryday",
                               "Q40"="numCola",
                               "Q25"="typeCola",
                               "Q26"="energyDrinksEveryday",
                               "Q41"="typeEnergyDrinks",
                               "Q28"="numEnergyDrinks",
                               "Q31"="email"
))



# setnames(demog974, c("Q1", "Q2", "Q3_1"), c("gender", "Age", "Emp_FT")) # uses setnames function from data.table package to reassign names in a similar way as rename function above but with slightly different syntax

# convert maritalStat to a factor

demog974$maritalStat <- factor(demog974$maritalStat, level = c(1,2,3,4,5,6),
                               label = c("single",
                                         "inRelationship",
                                         "married",
                                         "divorced",
                                         "widowed",
                                         "other"), 
                               ordered = F)

# transform language factor

demog974$language <- replace(demog974$language, is.na(demog974$language), 0)

demog974$language <- factor(demog974$language, levels = c(0,1,2),
                            labels = c("engOnly",
                                       "engFirst",
                                       "engSecond"),
                            ordered = F)



# convert edLevel to a factor

demog974$edLevel <- factor(demog974$edLevel, level = c(1,2,3,4),
                           label = c("primary",
                                     "secondary",
                                     "tertiary",
                                     "other"),
                           ordered = F)

# Now we are going to make a composite column for all the employment types. First we create a data frame to work with

emp <- data.frame(demog974$emp_FT,
                  demog974$emp_Casual, 
                  demog974$emp_Vol,
                  demog974$emp_UnEmp,
                  demog974$emp_Student,
                  demog974$emp_Retired,
                  demog974$emp_Other
)

###### this creates the new column from the dataframe above and adds it to the main demog974 spreadsheet

demog974$emp <- do.call(pmax, col(emp)*replace(emp, is.na(emp), 0))

# demog974$emp on the left of the '<-' creates a new column in the emp dataframe. do.call applies the first argument in the parentheses to the specified arguments in the second division after the comma. So here pmax chooses a per-row maximum from the dataframe (matrix?) created by the second argument and passes it to the vector/column demog974$emp (without the pmax command the second part of the function yields three columns not one, which is what we want). 'col(emp)' makes a matrix correpsonding to the column numbers (so all row entries in that column are the same: 1 in col 1, 2 in col 2, 3 in etc). It then multiplies this by another matrix 'replace(emp, is.na(emp), 0)' which replaces the emp matrix with a new matrix of the same dimensions as emp where all NA entries become zero (and all non-zeros, which are 1s in this case remain 1). Now the corresponding elements in the two matrices are mulitplied together. Mutliplying anything by zero produces zero so the only nonzero entries left in the resulting matrix are the column numbers. It is these that are passed into demog974$emp. If there are two entries in each row then the maximum row entry is chosen to pass into the composite column (see row one of demog974$emp)

## convert the new data column to a factor

demog974$emp <- factor(demog974$emp,
                       levels = c(1,2,3,4,5,6,7),
                       labels = c("fulltime",
                                  "casual",
                                  "volunteer",
                                  "unemployed",
                                  "student",
                                  "retired",
                                  'other')
)


# Now replace NA with 0

numNames <- c("numCupsTea",
              "numCola", 
              "numEnergyDrinks")


demog974[, numNames] <- replace(demog974[,numNames], is.na(demog974[,numNames]), 0)

# Now create new total caffeine score column. These come from the coding I applied to 1034, which derives from barone and roberts and manufacturers info. See SPSS Recodes syntax for amounts. The coffee figure was averaged across the different types of coffee.

demog974$totCaffeine <- demog974$numCupsCoffee*85 +
                        demog974$numCupsTea*30 +
                        demog974$numCola*37 +
                        demog974$numEnergyDrinks*80

# now we are making a list of col names which we will use to shrink the dataframe to only the cols we want. Notice all the emp columns are ommitted, leaving only the new composite column
colsToKeep <- c("genderM1F0", 
                "age",
                "maritalStat",
                "ethnicity",
                "language",
                "edLevel",
                "coffeeEveryday",
                "numCupsCoffee",
                "teaEveryday",
                "numCupsTea",
                "colaEveryday",
                "numCola",
                "numEnergyDrinks",
                "email",
                "emp",
                "totCaffeine")



# the command below selects those columns in demog974 which are included in the vector colsToKeep

demog974 <- demog974[, which(names(demog974) %in% colsToKeep)] #if you wanted to remove these columns you'd simply put a - in front of which


# Now we add a column where each entry is a substring of the email column. We will use this to merge with the master csv above.

demog974$substr_ID_974 <- substr(demog974$email, 1, 5)


#unique(demog974$substr_ID) to check if it worked.


# Now we can remove the email column because it already exists in the master spreadsheet and the Substr_ID column is now the reference column


demog974 <- demog974[, -which(names(demog974) %in% "email")]

# demog974  # to check

# write.csv(demog974, "~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/demog974.csv", row.names=F)








#################### 3. Expectancy questionnaire

all_content_EQ <- readLines("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/CEQ974.csv") # this reads the content of the csv file as raw text without trying to parse it into columns. We have to keep it as text until we get rid of the second line, which contains the text of the qualtrics questions as text in each cell. R will see this second row as a string cell belonging under the column heading designated by the top row column headings. Because there is a string in each column R can't make the column numeric and read it properly. qualtrics generates the cell names for the top line automatically. It is these we will keep as they don't have spaces, which R does not like as column names. Once we get rid of this second row of column names the numeric columns just contain numbers and we can parse it properly into columns.

skip_second_EQ <- all_content_EQ[-2] # removes second line with unneeded qualtrics headings

EQ974 <- read.csv(textConnection(skip_second_EQ), header = TRUE, stringsAsFactors = FALSE) # creates new object from the csv file, which has the second line removed

# removes duplicate rows
EQ974 <- EQ974[!duplicated(EQ974$Q51), ]

colsToKeepEQ <-  c("EQformOfCaffeine",
                   "EQformOfCaffeine_TEXT",
                   "EQ1_PicksMeUp",
                   "EQ2_BetterConvers",
                   "EQ3_HelpsAvoidEating",
                   "EQ4_CaffMakesStress",
                   "EQ5_CaffImprovesAthl",
                   "EQ6_CaffLessSleepy",
                   "EQ7_CaffSuppressHunger",
                   "EQ8_NoCaffMakesMiser",
                   "EQ9_CaffImproveMood",
                   "EQ10_NoCaffAnxious",
                   "EQ11_CaffMakesJittery",
                   "EQ12_CaffMakesWorkoutsBetter",
                   "EQ13_NoCaffMakesWithdrawal",
                   "EQ14_DontLikeCaffFeel",
                   "EQ15_NoCaffFeelSick",
                   "EQ16_CaffIncreaseMotiv",
                   "EQ17_CaffMoreConf",
                   "EQ18_CaffThrowsSleep",
                   "EQ19_CaffMakesNervous",
                   "EQ20_CaffMakesAlert",
                   "EQ21_CaffSmallMakesAnxious",
                   "EQ22_CaffImproveConc",
                   "EQ23_CaffMakesFriendly",
                   "EQ24_NoCaffNeedCaffDaily",
                   "EQ25_CaffMakesSweat",
                   "EQ26_CaffAllowsMealSkipping",
                   "EQ27_NoCaffMakesDesire",
                   "EQ28_CaffMakesDifficult",
                   "EQ29_CaffMakesIrritable",
                   "EQ30_CaffHelpsMeWork",
                   "EQ31_CaffMakesHappy",
                   "EQ32_NoCaffNoFunction",
                   "EQ33_CaffMakesIrregularHeartbeat",
                   "EQ34_OftenCraveCaff",
                   "EQ35_NoCaffTroubleStartingDay",
                   "EQ36_CaffUpsetsStomach",
                   "EQ37_TroubleGivingUpCaff",
                   "EQ38_CaffLateDisruptsSleep",
                   "EQ39_CaffHelpsControlWeight",
                   "EQ40_NoCaffMakesHeadache",
                   "EQ41_CaffImprovesAttention",
                   "EQ42_CaffMakesSociable",
                   "EQ43_CaffMakesExerciseLonger",
                   "EQ44_CaffHelpsMeGetThroughDay",
                   "EQ45_CaffMakesMoreEnergy",
                   "EQ46_CaffDecreaseAppetite",
                   "EQ47_CaffLateMakesInsomnia",
                   "email"                           
) 


setnames(EQ974, c("Q2", "Q2_TEXT", paste("Q", 4:51, sep="")), colsToKeepEQ) # uses setnames function from data.table package to reassign names in a similar way as rename function above but with slightly different syntax

# transform EQ974, leaving in only the columns listed above

EQ974 <- EQ974[, colsToKeepEQ]

colsToRecode <-  c("EQ1_PicksMeUp",
                   "EQ2_BetterConvers",
                   "EQ3_HelpsAvoidEating",
                   "EQ4_CaffMakesStress",
                   "EQ5_CaffImprovesAthl",
                   "EQ6_CaffLessSleepy",
                   "EQ7_CaffSuppressHunger",
                   "EQ8_NoCaffMakesMiser",
                   "EQ9_CaffImproveMood",
                   "EQ10_NoCaffAnxious",
                   "EQ11_CaffMakesJittery",
                   "EQ12_CaffMakesWorkoutsBetter",
                   "EQ13_NoCaffMakesWithdrawal",
                   "EQ14_DontLikeCaffFeel",
                   "EQ15_NoCaffFeelSick",
                   "EQ16_CaffIncreaseMotiv",
                   "EQ17_CaffMoreConf",
                   "EQ18_CaffThrowsSleep",
                   "EQ19_CaffMakesNervous",
                   "EQ20_CaffMakesAlert",
                   "EQ21_CaffSmallMakesAnxious",
                   "EQ22_CaffImproveConc",
                   "EQ23_CaffMakesFriendly",
                   "EQ24_NoCaffNeedCaffDaily",
                   "EQ25_CaffMakesSweat",
                   "EQ26_CaffAllowsMealSkipping",
                   "EQ27_NoCaffMakesDesire",
                   "EQ28_CaffMakesDifficult",
                   "EQ29_CaffMakesIrritable",
                   "EQ30_CaffHelpsMeWork",
                   "EQ31_CaffMakesHappy",
                   "EQ32_NoCaffNoFunction",
                   "EQ33_CaffMakesIrregularHeartbeat",
                   "EQ34_OftenCraveCaff",
                   "EQ35_NoCaffTroubleStartingDay",
                   "EQ36_CaffUpsetsStomach",
                   "EQ37_TroubleGivingUpCaff",
                   "EQ38_CaffLateDisruptsSleep",
                   "EQ39_CaffHelpsControlWeight",
                   "EQ40_NoCaffMakesHeadache",
                   "EQ41_CaffImprovesAttention",
                   "EQ42_CaffMakesSociable",
                   "EQ43_CaffMakesExerciseLonger",
                   "EQ44_CaffHelpsMeGetThroughDay",
                   "EQ45_CaffMakesMoreEnergy",
                   "EQ46_CaffDecreaseAppetite",
                   "EQ47_CaffLateMakesInsomnia"                         
) 

# recode Expectancy questionnaire

for (col_name in colsToRecode) { #for each column name in the vector 'cols_to_reverse_code' 
  EQ974[,  col_name] <- mapvalues( #apply all the arguments in the curly brackets to 
    # each. So performs map values function for all entries in each of the columns one at a time.
    EQ974[, col_name], 
    from=c(1, 2, 3, 4, 5, 6),
    to=c(5, 4, 3, 2, 1, 0)
  )
}

# creates Substr_ID reference column
EQ974$substr_ID_974 <- substr(EQ974$email, 1, 5)

# Remove email column

EQ974 <- EQ974[, -which(names(EQ974) %in% "email")]

# unique(EQ974$substr_ID) # checking.

# write.csv(EQ974, "~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/EQ974.csv", row.names = F)







#################### 4.  Reading in the withdrawal symptom questionnaire #######################


### Need to remove the secondary header from the Qualtrics file by skipping the second line of the csv
all_content974 <- readLines("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/CWSQ_974.csv")  # this reads in the qualtrics file and converts it to an object

skip_second974 <- all_content974[-2] # removes second line with unneeded qualtrics headings

CWSQ974 <- read.csv(textConnection(skip_second974), header = TRUE, stringsAsFactors = FALSE) # creates new object from the csv file, which has the second line removed

### Make sure column from the qualtrics file V8 (questionnaire start date) is a datetime

CWSQ974$V8 = parse_date_time(CWSQ974$V8, "%Y%m%d %H%M%S") # this is a function from lubridate package which automatically turns any input vector into a POSIXct date-time object. Here we are using the V8 column of the qualtrics spreadsheet as a 

######### Renaming the columns we want to keep ################################################

# vector of names for each CWSQ item

CWSQN <- c("drowsy",
           "selfconfident",
           "yawning",
           "alert",
           "tiredfatigued",
           "content",
           "diffconc",
           "irritable",
           "heavyfeel",
           "depressedmood",
           "grouchy",
           "urgework",
           "flulike",
           "headache",
           "talkative",
           "sluggish",
           "upsetstomach",
           "clearheaded",
           "desiresoc",
           "energetic",
           "nauseavom",
           "musclepain",
           "discouraged",
           "queasy",
           "nauseaous",
           "vomiting",
           "headachey",
           "anxious",
           "nervous",
           "jittery",
           "cravingcoffee",
           "cravingcaffeine",
           "email")


# creates the vector 
CWSQ974Nam <- paste("Q", 1:33, CWSQN, sep="")

# change the relevant columns, which now have column names as you want them to appear in the composite file
setnames(CWSQ974, paste("Q", 2:34, sep=""), CWSQ974Nam)

setnames(CWSQ974, "V8", "testtime") # changes the name of the data column used to sort the rows for each participant into chronological order to something more intuitive than V8


######################## removing unwanted columns. This can only be done using column numbers. But with the data.table package we can do it using names instead, which is always better.

# create vector of column names you want to drop

colsToDrop <- c(paste("V", 1:7, sep = "" ), paste("V", 9:10, sep=""), "Q1", "Q35", paste("Location", c("Latitude", "Longitude", "Accuracy"), sep=""), "X" )

# Turn CWSQ974 into a data.table so that we can apply the ':=' operator from the data.table package (this is another way to do the same thing as the which(names)... command above and below )

CWSQ974 <- as.data.table(CWSQ974)

# Drop the columns using the ':=' operator

CWSQ974 <- CWSQ974[, (colsToDrop):=NULL]

CWSQ974 <- as.data.frame(CWSQ974)


###### Cleaning up the reference column

#Creating a reference column in the new object (which will be matched to a corresponding column in the master csv file). NOTE: Using first 4 letters of email will probably only work for current subjects, figure out something more robust!

CWSQ974$substr_ID_974 <- substr(CWSQ974$Q33email, 1, 5) # creates a column in the CWSQ dataframe created above. This column is comprised of a substring extracted from the email address, 1st argument is the object (in this case the column Q34 which is the email address entered by each participant). 1st number is starting number, second number is how many characters to extract.

################ Check for typos

# Try to fix spaces etc. in email ID field (using regular expressions)

CWSQ974$Q33email <- str_replace(CWSQ974$Q33email, "\\s", "")

#unique(CWSQ974$Q33email) # a check for typos

#unique(CWSQ974[,"Substr_ID"]) # this tells us how many unique id codes we might have (this should match subject)


##################### Turning long format into wide for each subject ##############################

# Split dataset into pieces, one per subject, and order the V8/StartDate values within each subject

CWSQ974 <- ddply(CWSQ974, # specifies data frame we are looking at
                 
                 "substr_ID_974", # specifies the variable which we will be subsetting by
                 
                 function(sub_rows) { # creates a function called sub_rows to apply to each subset within the id_code column of the dataframe CWSQ974. Here the 'sub-rows' argument stands in for the dataframe CWSQ974
                   sub_rows$survey_num <- order(sub_rows$testtime) # creates a new column called survey_num in which each row is an order number for the place of that row within each subset of the 'id_code' column. The order numbers in this case are assigned based on the criteria 'date-time in column V8 of dataframe CWSQ974' (Remember above on line 29 we specified this column as a date-time)
                   return(sub_rows) # returns the newly augmented CWSQ974
                 }
)


### Creates a new column assigning more meaningful names to the subject codes 

CWSQ974$SurveyCode <- mapvalues(
  CWSQ974$survey_num,
  from=c(1, 2, 3),
  to=c("B1", "T1", "T2"),
  warn_missing=TRUE
)

# Check that everything looks sensible

#head(CWSQ974[, c("substr_ID_974", "testtime", "survey_num", "SurveyCode")])

# get rid of superfluous numbered surveynumber (this role now filled by Survey_Code) and email columns
survNumEmail <- c("survey_num", "Q33email")

CWSQ974 <- CWSQ974[, -which(names(CWSQ974) %in% survNumEmail)]


# Go from long to wide, using the survey code as the suffix for each variable

CWSQ974_reshaped <- reshape(CWSQ974, idvar="substr_ID_974", timevar="SurveyCode", direction="wide", sep = "") 


# Giving columns proper names with Day/Test number as prefix

# creates vector of all relevant CWSQ colnames
CWSQ974Names <- colnames(CWSQ974_reshaped[, 2:100])

# uses gsub for regular expression to search for first caps (which is test-day name) then put these at front of colnames
CWSQ974Names_re <- gsub("(.*)([A-Z][0-9])$", "\\2\\1", CWSQ974Names)

# replaces old colnames with new ones with test-day as prefix
setnames(CWSQ974_reshaped, colnames(CWSQ974_reshaped[,2:100]), CWSQ974Names_re)

#returns new colnames
#colnames(CWSQ974_reshaped[, 2:100])

# write.csv(CWSQ974_reshaped, "~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/CWSQreshaped.csv", row.names = F)




####################### 5. Exit Questionnaires #############################################

# Removing second line from the exit questionnaire

all_content_Exit_974 <- readLines("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/Exit_Questionnaire_974.csv")  # this reads in the qualtrics file and converts it to an object

skip_second_974 <- all_content_Exit_974[-2] # removes second line with unneeded qualtrics headings

Exit974 <- read.csv(textConnection(skip_second_974), header = TRUE, stringsAsFactors = FALSE) # creates new object from the csv file, which has the second line removed



############# removing unwanted columns ###############################################

colsToKeepExit974 <- c("DurationCaff",
                       "Quit_Y1N0",
                       "NumQuit",
                       "effectCaffGen",
                       "effectCaffBloodP",
                       "effectCaff2ndRVIP",
                       "ComplianceTest_Y1N0",
                       "ComplianceTest_TEXT",
                       "Purpose",
                       "OtherComments",
                       "ProcedureDiffered_Y1N0",
                       "ProcedureDiffered_TEXT",
                       "CaffinCoffee_Y1N0",
                       "CaffCoffeeLikelihood",
                       "geneTestFake",
                       "geneTestFake_TEXT",
                       "geneTestFriendTold",
                       "geneTestFriendTold_TEXT",
                       "email"                    
)



# changes column names 

setnames(Exit974, paste("Q", 2:20, sep=""), colsToKeepExit974)


# removes superfluous columns by specifying and including only those we want to keep

Exit974 <- Exit974[, which(names(Exit974) %in% colsToKeepExit974)]


# Adds Substr_ID column

Exit974$substr_ID_974 <- substr(Exit974$email, 1, 5) 


# remove superfluous email column

Exit974 <- Exit974[, -which(names(Exit974) %in% "email")]

# write.csv(Exit974, "~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/ExitQuest974.csv", row.names = F)








################### Merge Files ###############################################################

# Reference category is substr_ID

MasterList974 <- list(masterCSV974, Exit974, EQ974, demog974, CWSQ974_reshaped)


compiledMaster974 <- Reduce( function (...) merge(..., by = "substr_ID_974", all = F), MasterList974)


#  write.csv(compiledMaster974, "~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/compiledMaster974.csv", row.names = F)















############### Recoding ##########################################################


# to preserve original
recodedMaster974 <- compiledMaster974



# Recoding values from the qualtrics questionnaires.

############################# 1. Exit Quesitonnaire is first ##################################

# exclude (recode the 2 to a 0)

recodedMaster974$Exclude_Y1N0 <- mapvalues(recodedMaster974$Exclude_Y1N0, from = c(2,1), to = c(0,1))

# quit yes or no (recode 2 to 0)

recodedMaster974$Quit_Y1N0 <- mapvalues(recodedMaster974$Quit_Y1N0, from = c(2,1), to = c(0,1))

# expected effect of caff generally (turn to factor)

recodedMaster974$effectCaffGen <- factor(recodedMaster974$effectCaffGen, 
                                         levels = c(1,2,3),
                                         labels = c("better", "worse", "same"),
                                         ordered = FALSE)

# expected effect of caffeine on blood pressure

recodedMaster974$effectCaffBloodP <- factor(recodedMaster974$effectCaffBloodP, 
                                         levels = c(1,2,3),
                                         labels = c("raise", "lower", "same"),
                                         ordered = FALSE)

# expected effect of caffeine on second RVIP test

recodedMaster974$effectCaff2ndRVIP <- factor(recodedMaster974$effectCaff2ndRVIP, 
                                            levels = c(1,2,3),
                                            labels = c("higher", "lower", "unchanged"),
                                            ordered = FALSE)


# compliance 

recodedMaster974$ComplianceTest_Y1N0 <- mapvalues(recodedMaster974$ComplianceTest_Y1N0, from = c(2,1), to = c(0,1))


# procedure differed

recodedMaster974$ProcedureDiffered_Y1N0 <- mapvalues(recodedMaster974$ProcedureDiffered_Y1N0, from = c(2,1), to = c(0,1))

# caffeine in coffee

recodedMaster974$CaffinCoffee_Y1N0 <- mapvalues(recodedMaster974$CaffinCoffee_Y1N0, from = c(2,1), to = c(0,1))

# did you believe the gentic test was fake

recodedMaster974$geneTestFake <- factor(recodedMaster974$geneTestFake, 
                                             levels = c(1,2),
                                             labels = c("fake", "genuine"),
                                             ordered = FALSE)

# prior knowledge of the procedure

recodedMaster974$geneTestFriendTold <- factor(recodedMaster974$geneTestFriendTold, 
                                        levels = c(1,2),
                                        labels = c("priorKnowledge", "didNotKnow"),
                                        ordered = FALSE)





######################## 2. Demographics Questionnaire #######################################

# gender 

recodedMaster974$genderM1F0 <- mapvalues(recodedMaster974$genderM1F0, from = c(2,1), to = c(0,1))

recodedMaster974$genderM1F0 <- factor(recodedMaster974$genderM1F0, levels = c(0,1), labels = c("F", "M"), ordered = FALSE)

# employment

# mapFunct <- function (varNam, fromVal, toVal, ordered = FALSE) {
#   varNamX <- mapvalues(varNam, from = fromVal, to = toVal)
#   varNam <- varNamX
# }
# 
# mapFunct(recodedMaster$edLevel, c(1,2,3,4), c(0,1,2,3))

# Ed level (no longer relevant because already done)
# recodedMaster974$edLevel <- factor(recodedMaster974$edLevel, levels = c(1,2,3,4), labels = c("Primary", "Secondary", "Tertiary", "Other"), ordered = FALSE)


# Marital Status (no longer relevant because columns deleted)

# recodedMaster974$maritalStat <- factor(recodedMaster974$maritalStat, levels = c(1,2,3,4,5,6), labels = c("Single", "inRelationship", "Married", "Divorced", "Widowed", "Other"), ordered = FALSE)
# 
# # language other than english spoken at home (no longer relevant because columns deleted)
# 
# recodedMaster974$langOtherEng <- mapvalues(recodedMaster974$langOtherEng, from = c(2,1), to = c(0,1))
# 
# recodedMaster974$langOtherEng <- factor(recodedMaster974$langOtherEng, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)


# coffee every day

# recodedMaster974$coffeeEveryday <- mapvalues(recodedMaster974$coffeeEveryday, from = c(1,2), to = c(1,0))
# 
# recodedMaster974$coffeeEveryday <- factor(recodedMaster974$coffeeEveryday, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)
# 
# # tea every day
# 
# recodedMaster974$teaEveryday <- mapvalues(recodedMaster974$teaEveryday, from = c(1,2), to = c(1,0))
# 
# recodedMaster974$teaEveryday <- factor(recodedMaster974$teaEveryday, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)
# 
# # cola every day
# 
# recodedMaster974$colaEveryday <- mapvalues(recodedMaster974$colaEveryday, from = c(1,2), to = c(1,0))
# 
# recodedMaster974$colaEveryday <- factor(recodedMaster974$colaEveryday, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)

# energy drinks every day (no longer relevant because deleted from demog974)
# 
# recodedMaster974$energyDrinksEveryday <- mapvalues(recodedMaster974$energyDrinksEveryday, from = c(1,2), to = c(1,0))

# recodedMaster974$energyDrinksEveryday <- factor(recodedMaster974$energyDrinksEveryday, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)





################ 3. CWSQ Questionnaire #################################


for (col_name in CWSQ974Names_re) { # using the re-ordered column names for the CWSQ questions in the vector 'CWSQ974Names_re' (created in DataPrep974.R)
  recodedMaster974[,  col_name] <- mapvalues( #apply all the arguments in the curly brackets to 
    # each. So performs map values function for all entries in each of the columns one at a time.
    recodedMaster974[, col_name], 
    from=c(1, 2, 3, 4, 5),
    to=c(0, 1, 2, 3, 4)
  )
}






######################### Reverse coding 'positive' items in the CWSQ ####################### 

# create vector of columns to reverse code

# creates a sort of mini data frame where every element in one column is matched with every element in the other
colsToRev974 <- expand.grid(testDays = c("B1", paste("T", 1:2, sep="")),  
                         itemsToReverse = c("Q2selfconfident",
                                            "Q4alert",
                                            "Q6content",
                                            "Q12urgework",
                                            "Q15talkative",
                                            "Q18clearheaded",
                                            "Q19desiresoc",
                                            "Q20energetic"))


# this then pastes those two columns together.
colsToReverse974 <- paste(colsToRev974$testDays, colsToRev974$itemsToReverse, sep="")

# create a new dataframe to work on so original is preserved and so we can check
recodedRevMaster974 <- recodedMaster974



# now we run through the columns we specified with a for loop, remapping values (reverse coding in this case)
for (columnN in colsToReverse974) {
  recodedRevMaster974[, columnN] <- mapvalues(recodedRevMaster974[, columnN],
                                           from = c(0, 1, 2, 3, 4),
                                           to = c(4, 3, 2, 1, 0)
  )
}



############################# Making New Factors ##################################


factoredMaster974 <- recodedRevMaster974


# Making CWSQ factors


#factor vectors
DrowsyFatigued <- c("Q1drowsy", "Q3yawning",  "Q5tiredfatigued", "Q16sluggish")

DecreasedAlertDiffConc <- c("Q2selfconfident", "Q4alert", "Q6content", "Q7diffconc", "Q18clearheaded")

Mood <- c("Q8irritable", "Q10depressedmood", "Q11grouchy", "Q23discouraged")

DecreasedSocMotiv <- c("Q12urgework", "Q15talkative", "Q19desiresoc", "Q20energetic")

Nausea <- c("Q17upsetstomach", "Q21nauseavom", "Q24queasy", "Q25nauseaous", "Q26vomiting")

FluLike <- c("Q9heavyfeel", "Q13flulike", "Q22musclepain")

Headache <- c("Q14headache", "Q27headachey")

Acute <- c("Q28anxious", "Q29nervous", "Q30jittery")

Craving <- c("Q31cravingcoffee", "Q32cravingcaffeine")


# create list of these elements. We don't use it below but might form basis for a better way of doing it.
factorList <- list(DrowsyFatigued,
                   DecreasedAlertDiffConc,
                   Mood,
                   DecreasedSocMotiv,
                   Nausea,
                   FluLike,
                   Headache,
                   Acute,
                   Craving)

# create factor scores for each day

###### B1

B1DrowsyFacNames <- paste("B1", DrowsyFatigued, sep="")
factoredMaster974$B1DrowsyFac <- rowSums(factoredMaster974[, B1DrowsyFacNames], na.rm=F)

B1DecreasedAlertDiffConcFacNames <- paste("B1", DecreasedAlertDiffConc, sep="")
factoredMaster974$B1DecAlertFac <- rowSums(factoredMaster974[, B1DecreasedAlertDiffConcFacNames], na.rm=F)

B1MoodFacNames <- paste("B1", Mood, sep="")
factoredMaster974$B1MoodFac <- rowSums(factoredMaster974[, B1MoodFacNames], na.rm=F)

B1DecreasedSocMotivFacNames <- paste("B1", DecreasedSocMotiv, sep="")
factoredMaster974$B1DecreasedSocMotivFac <- rowSums(factoredMaster974[, B1DecreasedSocMotivFacNames], na.rm=F)

B1NauseaFacNames <- paste("B1", Nausea, sep="")
factoredMaster974$B1NauseaFac <- rowSums(factoredMaster974[, B1NauseaFacNames], na.rm=F)

B1FluLikeFacNames <- paste("B1", FluLike, sep="")
factoredMaster974$B1FluLikeFac <- rowSums(factoredMaster974[, B1FluLikeFacNames], na.rm=F)

B1HeadacheFacNames <- paste("B1", Headache, sep="")
factoredMaster974$B1HeadacheFac <- rowSums(factoredMaster974[, B1HeadacheFacNames], na.rm=F)

B1AcuteFacNames <- paste("B1", Acute, sep="")
factoredMaster974$B1AcuteFac <- rowSums(factoredMaster974[, B1AcuteFacNames], na.rm=F)

B1CravingFacNames <- paste("B1", Craving, sep="")
factoredMaster974$B1CravingFac <- rowSums(factoredMaster974[, B1CravingFacNames], na.rm=F)



### T1

T1DrowsyFacNames <- paste("T1", DrowsyFatigued, sep="")
factoredMaster974$T1DrowsyFac <- rowSums(factoredMaster974[, T1DrowsyFacNames], na.rm=F)

T1DecreasedAlertDiffConcFacNames <- paste("T1", DecreasedAlertDiffConc, sep="")
factoredMaster974$T1DecAlertFac <- rowSums(factoredMaster974[, T1DecreasedAlertDiffConcFacNames], na.rm=F)

T1MoodFacNames <- paste("T1", Mood, sep="")
factoredMaster974$T1MoodFac <- rowSums(factoredMaster974[, T1MoodFacNames], na.rm=F)

T1DecreasedSocMotivFacNames <- paste("T1", DecreasedSocMotiv, sep="")
factoredMaster974$T1DecreasedSocMotivFac <- rowSums(factoredMaster974[, T1DecreasedSocMotivFacNames], na.rm=F)

T1NauseaFacNames <- paste("T1", Nausea, sep="")
factoredMaster974$T1NauseaFac <- rowSums(factoredMaster974[, T1NauseaFacNames], na.rm=F)

T1FluLikeFacNames <- paste("T1", FluLike, sep="")
factoredMaster974$T1FluLikeFac <- rowSums(factoredMaster974[, T1FluLikeFacNames], na.rm=F)

T1HeadacheFacNames <- paste("T1", Headache, sep="")
factoredMaster974$T1HeadacheFac <- rowSums(factoredMaster974[, T1HeadacheFacNames], na.rm=F)

T1AcuteFacNames <- paste("T1", Acute, sep="")
factoredMaster974$T1AcuteFac <- rowSums(factoredMaster974[, T1AcuteFacNames], na.rm=F)

T1CravingFacNames <- paste("T1", Craving, sep="")
factoredMaster974$T1CravingFac <- rowSums(factoredMaster974[, T1CravingFacNames], na.rm=F)

##### T2

T2DrowsyFacNames <- paste("T2", DrowsyFatigued, sep="")
factoredMaster974$T2DrowsyFac <- rowSums(factoredMaster974[, T2DrowsyFacNames], na.rm=F)

T2DecreasedAlertDiffConcFacNames <- paste("T2", DecreasedAlertDiffConc, sep="")
factoredMaster974$T2DecAlertFac <- rowSums(factoredMaster974[, T2DecreasedAlertDiffConcFacNames], na.rm=F)

T2MoodFacNames <- paste("T2", Mood, sep="")
factoredMaster974$T2MoodFac <- rowSums(factoredMaster974[, T2MoodFacNames], na.rm=F)

T2DecreasedSocMotivFacNames <- paste("T2", DecreasedSocMotiv, sep="")
factoredMaster974$T2DecreasedSocMotivFac <- rowSums(factoredMaster974[, T2DecreasedSocMotivFacNames], na.rm=F)

T2NauseaFacNames <- paste("T2", Nausea, sep="")
factoredMaster974$T2NauseaFac <- rowSums(factoredMaster974[, T2NauseaFacNames], na.rm=F)

T2FluLikeFacNames <- paste("T2", FluLike, sep="")
factoredMaster974$T2FluLikeFac <- rowSums(factoredMaster974[, T2FluLikeFacNames], na.rm=F)

T2HeadacheFacNames <- paste("T2", Headache, sep="")
factoredMaster974$T2HeadacheFac <- rowSums(factoredMaster974[, T2HeadacheFacNames], na.rm=F)

T2AcuteFacNames <- paste("T2", Acute, sep="")
factoredMaster974$T2AcuteFac <- rowSums(factoredMaster974[, T2AcuteFacNames], na.rm=F)

T2CravingFacNames <- paste("T2", Craving, sep="")
factoredMaster974$T2CravingFac <- rowSums(factoredMaster974[, T2CravingFacNames], na.rm=F)





##### Total CWSQ Scores across each day


# removing the email element from the list of CWSQ items set up in 974DataPrep
CWSQ974Nom <- CWSQ974Nam[-33]


# create new day total CWSQ score variables

# B1 Total


B1Names <- paste("B1", CWSQ974Nom, sep="")
factoredMaster974$B1Total <- rowSums(factoredMaster974[, B1Names], na.rm = F) 


# T1 Total

T1Names <- paste("T1", CWSQ974Nom, sep="")
factoredMaster974$T1Total <- rowSums(factoredMaster974[, T1Names], na.rm = F) 

# T2 Total

T2Names <- paste("T2", CWSQ974Nom, sep="")
factoredMaster974$T2Total <- rowSums(factoredMaster974[, T2Names], na.rm = F) 


# calculate difference score for withdrawal
factoredMaster974$testDiff <- factoredMaster974$T1Total - factoredMaster974$T2Total 




# create factors for the EQ

factoredMaster974$EQWithdrawal <- factoredMaster974$EQ8_NoCaffMakesMiser +
  factoredMaster974$EQ10_NoCaffAnxious +
  factoredMaster974$EQ13_NoCaffMakesWithdrawal +
  factoredMaster974$EQ15_NoCaffFeelSick +
  factoredMaster974$EQ24_NoCaffNeedCaffDaily
  factoredMaster974$EQ27_NoCaffMakesDesire +
  factoredMaster974$EQ32_NoCaffNoFunction +
  factoredMaster974$EQ35_NoCaffTroubleStartingDay +
  factoredMaster974$EQ37_TroubleGivingUpCaff +
  factoredMaster974$EQ40_NoCaffMakesHeadache +
  factoredMaster974$EQ34_OftenCraveCaff +
  factoredMaster974$EQ44_CaffHelpsMeGetThroughDay

factoredMaster974$EQEnergyWork <- factoredMaster974$EQ20_CaffMakesAlert +
  factoredMaster974$EQ1_PicksMeUp +
  factoredMaster974$EQ45_CaffMakesMoreEnergy +
  factoredMaster974$EQ6_CaffLessSleepy +
  factoredMaster974$EQ30_CaffHelpsMeWork +
  factoredMaster974$EQ16_CaffIncreaseMotiv +
  factoredMaster974$EQ22_CaffImproveConc +
  factoredMaster974$EQ41_CaffImprovesAttention

factoredMaster974$EQSocMood <- factoredMaster974$EQ23_CaffMakesFriendly +
  factoredMaster974$EQ2_BetterConvers +
  factoredMaster974$EQ42_CaffMakesSociable +
  factoredMaster974$EQ17_CaffMoreConf +
  factoredMaster974$EQ31_CaffMakesHappy +
factoredMaster974$EQ9_CaffImproveMood




########## Rearrange ####################

# rearrange columns
factoredMaster974 <-  factoredMaster974[, c(2, 5, 8, 1, 7, 3, 4, 10, 9, 6, 11, 12:length(factoredMaster974))]

length(factoredMaster974)


# reorder dataframe according to id no
factoredMaster974 <- factoredMaster974[order(factoredMaster974$ID),]

# change name of DurationCaff and NumQuit Variables

# factoredMaster974 <- setnames(factoredMaster974, c("DurationCaff.x", "NumQuit.x"), c("DurationCaff", "NumQuit"))


# write to file
write.csv(factoredMaster974, "~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/factoredMaster974.csv", row.names = F)



