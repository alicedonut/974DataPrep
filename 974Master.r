Here we are creating a new column with each entry as the rank order of that row within the subset that row belongs to (in this case ID).
  RVIP974 <- ddply(RVIP974, 
                   "ID", 
                   function(subsetRows) {
                     subsetRows$newCol <- order(subsetRows$dateTime) 
                     return(subsetRows)
                   }
  )
  
  
  # now we give the new column codes better names in a new column
  
  RVIP974$testCode <- mapvalues(RVIP974$newCol,
                                from = c(1,2,3),
                                to = c("B1", "T1", "T2"),
                                warn_missing = T)
  
  # remove now-superfluous rank column
  RVIP974 <- RVIP974[, -which(names(RVIP974) %in% c("newCol", "dateTime"))]
  
  #reorder columns
  
  RVIP974 <- RVIP974[c("ID", "testCode", "sumhit", "sumfa", "sumacc")] 
  
  
  # now we reshape the data into wide format
  
  RVIP974_reshaped <- reshape(RVIP974, idvar = "ID", timevar = "testCode", direction = "wide", sep="")
  
  
  # make a vector of all column names except ID
  RVIP974ColNames <- colnames(RVIP974_reshaped[,-which(names(RVIP974) %in% "ID")])
  
  
  # search for non-non-caps. when found put them at front
  RVIP974ColNamesSub <- gsub("(.*)([A-Z][0-9])$", "\\2\\1", RVIP974ColNames)
  
  # rename col names 1st arg is dataframe, second is old names, last is new names.
  setnames(RVIP974_reshaped, RVIP974ColNames, RVIP974ColNamesSub)
  
  
} else {

  # if on the other hand the file does not exist as an iqdat and is already a csv we skip the conversion process and jump straight in to the processing of the csv file
  RVIP974 <- read.csv("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/RVIP_summary.csv")

# converts the crappy inquisit date format to ISO 8601 
RVIP974$script.startdate <- as.Date(ifelse(nchar(RVIP974$script.startdate)==5, # IF number of characters equals 5
                                           paste("0", RVIP974$script.startdate ,sep=""), # add a 0 to the front
                                           RVIP974$script.startdate), # otherwise read it as is.
                                    "%m%d%y") # read it as this format

#RVIP974$newDateAlt <- as.Date(sprintf("%06d", RVIP974$script.startdate), "%m%d%y") # this does the same thing as above

# ditto for this command using the mdy function from lubridate
# RVIP974$newDateAlt2 <- as.Date(mdy(RVIP974$script.startdate))

# creates new column merging the date column and the time column
RVIP974 <- within(RVIP974, { dateTime = format(as.POSIXct(paste(RVIP974$script.startdate, RVIP974$script.starttime)), "%Y-%m-%d %H:%M:%S") })

# from lubridate package turns new dateTime column into POSIX object
RVIP974$dateTime <- parse_date_time(RVIP974$dateTime, "%Y%m%d %H%M%S")


# removes the now-superfluous date and time columns
RVIP974 <- RVIP974[, -which(names(RVIP974) %in% c("script.startdate", "script.starttime", "values.SumTargets") )]


# renames columns to more tractable names using the rename function from plyr
RVIP974 <- rename(RVIP974, c("script.subjectid" = "ID",
                             "values.SumHit" = "sumhit",
                             "values.SumFA" = "sumfa"))


# accuracy score: p(Hit) - p(FA)/ 1 - p(FA)

pFA <- RVIP974$sumfa/128.67

pHit <- RVIP974$sumhit/38

RVIP974$sumacc <- (pHit - pFA)/ (1 - pFA)




####################### Turning long into wide #########################

# ddply applies a function to a subset of a dataframe. ID is the variable we are subsetting by. Here we are creating a new column with each entry as the rank order of that row within the subset that row belongs to (in this case ID).
RVIP974 <- ddply(RVIP974, 
                 "ID", 
                 function(subsetRows) {
                   subsetRows$newCol <- order(subsetRows$dateTime) 
                   return(subsetRows)
                 }
)


# now we give the new column codes better names in a new column

RVIP974$testCode <- mapvalues(RVIP974$newCol,
                              from = c(1,2,3),
                              to = c("B1", "T1", "T2"),
                              warn_missing = T)

# remove now-superfluous rank column
RVIP974 <- RVIP974[, -which(names(RVIP974) %in% c("newCol", "dateTime"))]

#reorder columns

RVIP974 <- RVIP974[c("ID", "testCode", "sumhit", "sumfa", "sumacc")] 


# now we reshape the data into wide format

RVIP974_reshaped <- reshape(RVIP974, idvar = "ID", timevar = "testCode", direction = "wide", sep="")


# make a vector of all column names except ID
RVIP974ColNames <- colnames(RVIP974_reshaped[,-which(names(RVIP974) %in% "ID")])


# search for non-non-caps. when found put them at front
RVIP974ColNamesSub <- gsub("(.*)([A-Z][0-9])$", "\\2\\1", RVIP974ColNames)

# rename col names 1st arg is dataframe, second is old names, last is new names.
setnames(RVIP974_reshaped, RVIP974ColNames, RVIP974ColNamesSub)

} # end of if else statement




################### merge RVIP files with master csv file ############################# 


mastList974 <- list(master974, RVIP974_reshaped)


masterCSV974 <- Reduce( function (...) merge(..., by = "ID", all = F), mastList974)



####@@@@@@@@@@@@@@@@@ 2. Demographics questionnaire

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
                               "Q8"="engFirst",
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


colsToKeep <- c("genderM1F0", 
                "age",
                "emp_FT",
                "emp_Casual",
                "emp_Vol",
                "emp_UnEmp",
                "emp_Student",
                "emp_Retired",
                "emp_Other",
                "emp_OtherTxt",
                "edLevel",
                "edLevel_TEXT",
                "maritalStat",
                "ethnicity",
                "langOtherEng",
                "langOtherEng_TEXT",
                "engFirst",
                "coffeeEveryday",
                "numCupsCoffee",
                "teaEveryday",
                "numCupsTea",
                "colaEveryday",
                "numCola",
                "typeCola",
                "energyDrinksEveryday",
                "typeEnergyDrinks",
                "numEnergyDrinks",
                "email"
)


# the command below selects those columns in demog974 which are included in the vector colsToKeep

demog974 <- demog974[, which(names(demog974) %in% colsToKeep)] #if you wanted to remove these columns you'd simply put a - in front of which

# Now we add a column where each entry is a substring of the email column. We will use this to merge with the master csv above.

demog974$substr_ID_974 <- substr(demog974$email, 1, 5)


#unique(demog974$substr_ID) to check if it worked.


# Now we can remove the email column because it already exists in the master spreadsheet and the Substr_ID column is now the reference column


demog974 <- demog974[, -which(names(demog974) %in% "email")]

# demog974  # to check





#################### 3.  Reading in the withdrawal symptom questionnaire #######################


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






####################### 4. Exit Questionnaires #############################################

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










################### Merge Files ###############################################################

# Reference category is substr_ID

MasterList974 <- list(masterCSV974, Exit974, demog974, CWSQ974_reshaped)



compiledMaster974 <- Reduce( function (...) merge(..., by = "substr_ID_974", all = F), MasterList974)



# write.csv(compiledMaster974, "~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/compiledMaster974.csv")

















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

recodedMaster974$edLevel <- factor(recodedMaster974$edLevel, levels = c(1,2,3,4), labels = c("Primary", "Secondary", "Tertiary", "Other"), ordered = FALSE)


# Marital Status

recodedMaster974$maritalStat <- factor(recodedMaster974$maritalStat, levels = c(1,2,3,4,5,6), labels = c("Single", "inRelationship", "Married", "Divorced", "Widowed", "Other"), ordered = FALSE)

# language other than english spoken at home

recodedMaster974$langOtherEng <- mapvalues(recodedMaster974$langOtherEng, from = c(2,1), to = c(0,1))

recodedMaster974$langOtherEng <- factor(recodedMaster974$langOtherEng, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)


# coffee every day

recodedMaster974$coffeeEveryday <- mapvalues(recodedMaster974$coffeeEveryday, from = c(1,2), to = c(1,0))

recodedMaster974$coffeeEveryday <- factor(recodedMaster974$coffeeEveryday, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)

# tea every day

recodedMaster974$teaEveryday <- mapvalues(recodedMaster974$teaEveryday, from = c(1,2), to = c(1,0))

recodedMaster974$teaEveryday <- factor(recodedMaster974$teaEveryday, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)

# cola every day

recodedMaster974$colaEveryday <- mapvalues(recodedMaster974$colaEveryday, from = c(1,2), to = c(1,0))

recodedMaster974$colaEveryday <- factor(recodedMaster974$colaEveryday, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)

# energy drinks every day

recodedMaster974$energyDrinksEveryday <- mapvalues(recodedMaster974$energyDrinksEveryday, from = c(1,2), to = c(1,0))

recodedMaster974$energyDrinksEveryday <- factor(recodedMaster974$energyDrinksEveryday, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)





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



########## Rearrange ####################



# rearrange columns
factoredMaster974 <-  factoredMaster974[, c(2, 5, 8, 1, 7, 3, 4, 10, 9, 6, 11:206)]


# reorder dataframe according to id no
factoredMaster974 <- factoredMaster974[order(factoredMaster974$ID),]


# write to file
write.csv(factoredMaster974, "~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/factoredMaster974.csv", row.names = F)





















