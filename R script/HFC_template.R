##################################### Meta ##########################################

# High Frequency Code template
# 29th September 2017
# Contributors : Krishanu Chakraborty, Vikram Jambulapati, James Dunham
# Originally written in Stata by Saurabh (J-PAL SA) and IPA (https://github.com/PovertyAction/high-frequency-checks)
# Version of code : 2.0.0
# R version : R version 3.4.2 (2017-09-28) -- "Short Summer"
# Last edited by : Krishanu Chakraborty

#################################### Introduction ###################################

# Why are High Frequncy Checks (HFC) important ?

# A high-frequency check is a check of some element of the data collection process, 
# completed on a regular basis as new data comes in. At J-PAL/IPA, high-frequency checks are
# typically implemented in Stata, after the data flow is complete. High-frequency checks can
# provide information about any of the following elements of data collection:
# 
# 1. The quality of the data
# 2. Enumerator performance
# 3. The CAI survey program (are there programming errors?)
# 4. The data flow (are there systemic flaws?)
# 
# Given how much information they can provide about the quality of the data collection, high
# -frequency checks are one of the major benefits of CAI. It's hard to overstate how 
# important these checks are.

# High-frequency checks are different from CAI logic checks, which are programmed into the 
# CAI survey program and not in Stata. Typically, high-frequency checks are checks that 
# can't be implemented in a CAI program. For instance, while CAI logic checks are restrictions
# on a field or the relationship between fields within a survey, high-frequency checks often
# check trends across surveys.

# Most high-frequency checks should be completed as often as possible, ideally daily. However
# ,some high-frequency checks can happen less frequently, perhaps weekly or monthly. It's
# often easiest to put checks that occur at different frequencies in separate scripts: Your
# project could have a daily_checks.R, a weekly_checks.R, and a monthly_checks.R

# To the extent possible, you should write the code for your high-frequency checks before data
# collection begins. The first few days of data collection are already chaotic, and you likely
# won't have time for serious programming. To make sure you implement these essential checks
# even in the first days of data collection, you'll need to program them beforehand.

# Note that not all checks listed below are appropriate for all projects.  Further, the list
# below is not exhaustive. Before data collection begins, you should brainstorm other checks
# to complete. Additionally, as data collection is ongoing, you'll pick up on unexpected
# trends in the data and think of new high-frequency checks. To make sure you discover these
# trends, commit to spending a portion of each day or each week just inspecting your data and
# dreaming up new high-frequency checks. Tabulate frequencies, graph distributions, calculate
# means, and just eyeball variables with view() in R) and make sure their values look right.

# Quick queries:
# 
# Question 1: How do you view your dataset in R?
# Question 2: What if, you wanted to plot variables on a graph from the data frame that you
# have? Can you think of a way to implement the same?

# Below there are some examples of daily logic checks that should be run

# Daily logic checks
# 
# 1. Check that all interviews were completed
# 2. Check that there are no duplicate observations
# 3. Check that all surveys have consent
# 4. Check that certain critical variables have no missing values
# 5. Check that follow-up record ids match original
# 6. Check skip patterns and constraints
# 7. Check that no variable has all missing values
# 8. Check hard/soft constraints
# 9. Check specify other vars for items that can be included
# 10. Check that date values fall within survey range
# 11. Check that there are no outliers for unconstrained vars
# 
# Enumerator checks (dashboard)
# 
# 1. Check the percentage of "don't know" and "refusal" values for each variable by enumerator
# 2. Check the percentage giving each answer for key filter questions by enumerator
# 3. Check the percentage of survey refusals by enumerator
# 4. Check the number of surveys per day by enumerator
# 5. Check average interview duration by enumerator
# 6. Check the duration of consent by enumerator
# 7. Check the duration of other (anthropometrics, games, etc)
# 
# Research checks (dashboard)
# 
# 1. Survey progress towards recruitment goals.
# 2. Summary of key research variables.
# 3. Two-way summaries of survey variables by demographic/geographic characteristics.
# 4. Refusal/not found rates by treatment status.
# 5. Maps/GIS
# 
# Please note that these are all recommended checks and all of them are not included in this
# high frequency check template
# 
# In this high-frequncy check template, we would focus on the follwing important modules:
# 
# 1. Unique ID Checks
# 
#   a. Checking for missing unique IDs
#   b. Duplicates in unique ID
#   c. Unique ID matches with master tracking list & field tracking lists
# 
# 2. Date and Time checks
# 
#   a. Surveys that don't end on the same day as they started
#   b. Surveys where end date/time is before start date/time
#   c. Surveys that show start time earlier than first day of data collection
#   d. Surveys that have start time after system date (current)
#   
# 3. Distributions
# 
#   a. Missing Values
#     i.   Variables with all observations missing
#     ii.  Missing value percentages for remaining variables (which don't have all values
#     missing)
#   b. Number of distinct values
#   c. Distribution of specific coded values (don't know, refused, other etc.)
#   d. Outliers
# 
# 4. Survey Durations
# 
#   a. Calculating duration
# 
# 5. Enumerator Checks
# 
#   a. Enumerator level average survey duration
#   b. Enumerator level distribution checks
#     i.  Missing Values
#     ii. Number of distinct values
#     iii. Distribution of specific coded values (don't know, refused, other etc.)
#   
# 6. Productivity
# 
#   a. Overall Productivity
#     i.   Summary of daily average productivity
#     ii.  Overall Productivity histogram 
#   b. Enumerator level productivity
#     i.  Average productivity per day by surveyor
#     ii. Surveyors with very low or high productivity
# 
# Let us go through these steps one by one.
# 
################################## Setting up the environment #######################

# install packages if necessary 
# install.packages(c("foreign", "haven","dplyr", "VIM", "outliers", "ggplot2", "scales", "grid", "RColorBrewer", "psych", "lubridate", "ggthemes"), dependencies=TRUE, INSTALL_opts = c('--no-lock'))
# load the required packages

library(foreign)
library(haven)
library(dplyr)
library(VIM)
library(outliers)
library(ggplot2)
library(scales) 
library(grid)
library(RColorBrewer)
library(psych)
library(lubridate)

# setting user path

user_path <- function() {
  # Return a hardcoded path that depends on the current user, or the current 
  # working directory for an unrecognized user. If the path isn't readable,
  # stop.
  
  user <- Sys.info()["user"]
  
  if (user == "james") 
  {
    path = "~/r-resources"
    file.exists
  } else if (user == "Krishanu_Chakrobarty") 
  {
    path = "D:/R/HFC/HFC-2"
  } else {
    warning("No path found for current user (", user, ")")
    path = getwd()
  }
  
  stopifnot(file.exists(path))
  return(path)
}

setwd(user_path())  # setting the working directory as required

# getting the data

dummy_data <- read.dta("dummy_main.dta")

# Question 3: Can you think of any other way to load .dta files?

#------------------------------------ 1. Unique ID checks -------------------------------#

# The Unique ID cannot be missing. This should be taken care of during CAI / DDC programming.
# CAI = Computer aided interviews
# DDC = Digital data collection

########################### Checking for missing Unique IDs ######################### 

# Let us consider the variables which are unique and you want to run the missing checks for. 
# Suppose we consider "surveyor_id" and "name"

variable_names <- c("surveyor_id", "name")      # you can put as many variables as you want
sapply(dummy_data[,variable_names], function(x) sum(is.na(x)))

# The output lists down the missing values for the specified variables: surveyor_id and name. # There are 1 missing value in surveyor_id and no value is missing in name.

# Question 4: Can you think of another way to implement the above operation?

# To see all values that are missing for each of the variable in the data frame:

sapply(dummy_data, function(x) sum(is.na(x)))

# Question 5: Can you think of another way to emulate the sapply? [Hint : Loops]

# Suppose, we want to see the number of values which are missing as a percentage

signif(colMeans(is.na(dummy_data)) * 100, digits = 2)

# Question 6: Can you find out an alternate way to emulate the same function? Using dplyr?

# Let us now list relevant information (e.g., surveyor ID and start time) for
# missing surveyor IDs

dummy_data[is.na(dummy_data$surveyor_id), c("surveyor_id", "starttime")]

# 483 is the row number which has has themissing surveyor ID.
# When you are dealing with factors, when the NA is wrapped in angled brackets (<NA>) 
# indicates # that it is in fact NA. When it is NA without brackets, then it is not NA, but 
# rather a proper factor whose label is "NA".


# Bonus Tip : To see the structure of missing values in the data frame
# graphically we can use the aggr function from the VIM package. As you can see, the output is a graph that shows proportion of missing values for each of the variables in the dataset.

aggr(dummy_data)

############################ Duplicates in Unique IDs ###############################

# Finding the number of duplicates in surveyor_id (with the dplyr package)

sum(duplicated(dummy_data$surveyor_id))

# The output is 458, or the number of rows with a surveyor ID that has already
# been seen. 

# To see the number of surveyor IDs that appear more than once:

dummy_data <- dummy_data %>%
  arrange(surveyor_id) %>%                ## sorted by surveyor id
  group_by(surveyor_id) %>%               ## grouped by surveyor id 
  mutate(dup = row_number())

# finding the number of duplicates. 

sum(dummy_data$dup > 0)

# Please note that the Stata template shows this as 481 instead of 483. The same output can 
# also be achieved easily but is not mandatory for high frequency checks.

# Question 7: Can you think of a code snippet that aligns the output with Stata?

# Now let us see the surveys with duplicates. Please remember that here the variable we are 
# checking with is surveyor id. Depending on project, the variable to check duplicate surveys
# should be appropriately selected

dummy_data_temp = subset(dummy_data, dummy_data$dup > 0 & !is.na(dummy_data$surveyor_id))

print("Surveys with duplicates")
subset(dummy_data_temp, select = c("surveyor_id", "starttime", "dup"))

# The "dup" column shows the nth duplicate associated with that surveyor ID.

# You can free up memory by using rm(). This is currently not implmented because the dummy_data_temp
# is used as a proxy for master tracking sheet

# rm(dummy_data_temp)                       

########## Unique ID matches with master tracking list & field tracking lists #######

# creating a vector with all variable names except unique ID

variable_list_master <- setdiff(names(dummy_data), "surveyor_id")

# the same but for the tracking sheet

variable_list_using <- setdiff(names(dummy_data_temp), "surveyor_id")

# Checking for variables with same name in master & using. It's a
# good habit to check for such common variable names to avoid losing 
# necessary variables from using data 

duplicate_names <- intersect(variable_list_master, variable_list_using)
if (length(duplicate_names) > 0) {
  print(paste("The following variable(s) have the same name in master and using:",
              paste(duplicate_names, collapse = ", ")))
}

# Alternate longer way to implement the same:

# for (i in 1:length(variable_list_master)) {
#   for (j in 1:length(variable_list_using)) {
#     if (variable_list_master[i] == variable_list_using[j]) {
#       print(paste("Variable", variable_list_master[i] ,"has same name in both master and using"))
#     }
#   }
# }

# Question 8: Suppose we do not want to use/ see such a detailed analysis. What other commands can also be used?

# Check the merge results thoroughly and carry out further checks as needed

# merge(dummy_data, dummy_data_temp)          # Run this when you have actual data to check
# Please be careful with all.x and all.y.

#----------------------------- Date and Time Checks ---------------------------------------#

# In R, there are two basic classes of date/times. Class "POSIXct" represents the (signed) number of seconds since the beginning of 1970 (in the UTC time zone) as a numeric vector. "POSIXct" is more convenient for including in data frames, and "POSIXlt" is closer to human-readable forms. A virtual class "POSIXt" exists from which both of the classes inherit: it is used to allow operations such as subtraction to mix the two classes.
# 
# Logical comparisons and some arithmetic operations are available for both classes. 
# One can add or subtract a number of seconds from a date-time object, but not add two dat
# -time objects. Subtraction of two date-time objects is equivalent to using difftime. Be
# aware that "POSIXlt" objects will be interpreted as being in the current time zone for 
# these operations unless a time zone has been specified.

############### Surveys that don't end on the same day as they started ##############

# Let us check for the surveys which do not end on the same day as they started. In most cases these types of surveys are a cause of concern.

subset(subset(dummy_data, as_date(dummy_data$starttime) != as_date(dummy_data$endtime), select= c("surveyor_id", "starttime","endtime", "name")))

############## Surveys where end date/time is before start date/time ################

# Similarly, surveys with end date/time is before start data/time is also a genuine case of concern

subset(subset(dummy_data, as.Date(dummy_data$starttime) > as.Date(dummy_data$endtime)), select= c("surveyor_id", "starttime","endtime"))

subset(subset(dummy_data, strftime(dummy_data$starttime, format="%H:%M:%S") > strftime(dummy_data$endtime, format="%H:%M:%S") & as.Date(dummy_data$starttime) > as.Date(dummy_data$endtime)), select= c("surveyor_id", "starttime","endtime"))

# Don't worry of you do not get any coincidences

####### Surveys that show starttime earlier than first day of data collection #######

subset(subset(dummy_data, as.Date(dummy_data$starttime, "%m/%d/%y") < as.Date("4/21/16", "%m/%d/%y")), select= c("surveyor_id", "starttime","endtime", "name")) # 21st April 2016 is just an example. Replace it with start date of your data collection

########### Surveys that have starttime after system date (current) #################

subset(subset(dummy_data, as.Date(dummy_data$starttime) > Sys.Date()), select= c("surveyor_id", "starttime","endtime"))

# Question 9: When dealing with live survey data, the default columns starttime and endtime can be very deceptive sometimes. Can you think about what problem may arise and how do you go about solving the same?

#--------------------------- Distribution ------------------------------------------#

# Checking distributions across all variables / major variables let us know critical problems with one/ more variable. Also, at a very early stage, the distributions help us detect problem with the survey design / data collection. This is one of the most important daily checks.

############################# Missing Values ########################################

# Variables with all observations missing 

sapply(dummy_data, function(x) !any(!is.na(x)))

# Missing value percentages for remaining variables (which don't have all values missing)

# Let us first see the missing values in non-numeric variables

print("Displaying percent missing in non-numeric variables") 
signif(colMeans(is.na(dummy_data[sapply(dummy_data, is.character)])) * 100, digits = 2)
signif(colMeans(dummy_data[sapply(dummy_data, is.character)] == "") * 100, digits = 2) # this is for a non - NA blank field (the empty string "")

# Now, applying the same logic for numeric variables

print("Displaying percent missing in numeric variables") 
signif(colMeans(is.na(dummy_data[!sapply(dummy_data, is.character)])) * 100, digits = 2)

# Question 10 : Can you think of any other way to implement the same? [hint : Loops again, 
# if sapply seems counter- intuitive]

########################### Number of distinct values ###############################

# Pay attention to variables with very few distinct values. 
# Lack of variation in variables is an important flag to be raised and discussed with the PIs. 
n_distinct_no_na <- function(x) n_distinct(x[!is.na(x)])
sapply(dummy_data, n_distinct_no_na)

####### Distribution of specific coded values (don't know, refused, other etc.) #####

# "-999" is used as and example. Run this for all the codes in your survey

# For numeric variables

signif(colMeans(dummy_data[!sapply(dummy_data, is.character)] == -999, na.rm =
                  TRUE) * 100, digits = 2)

# For non-numeric variables

signif(colMeans(dummy_data[sapply(dummy_data, is.character)] == "-999", na.rm =
                  TRUE) * 100, digits = 2)

# Note: if you want to run the following code snippet: 
# colMeans(dummy_data[sapply(dummy_data, class) != "character"] == "-999", na.rm = T)
# you might face this error : Error in as.POSIXlt.character(x, tz, ...) : character string 
# is not in a standard unambiguous format

# Question 11 : How do you deal with this error?

################################### Outliers ########################################

# Outliers in key variables gives us a sense of the values which are potentially misrecorded/
# problemmatic, indicating the need for further probing and cross-validation

# Here argument "z" is used while using the scores function. Appropriate custom functions 
# can also be defined.

# Question 12: What doe the "z" argument do? What is actually calculated?

scores_na <- function(x) scores(x[!is.na(x) & x > -1], type = "z")
scores_outliers <- sapply(dummy_data[!sapply(dummy_data, is.character)], scores_na)

# Question 13: What would have been the code if we wanted only numeric type variable

# Now let us find the scores for those columns where there is atleast one score calculated

scores_outliers <- scores_outliers[sapply(scores_outliers, length) > 0]

# Let us see the scores that is an outlier

for (i in 1:length(scores_outliers)) { 
  for (j in 1:length(scores_outliers[[i]])) { 
    # only to be used for debugging
    # cat("i=", i, "j=", j, "\n", sep = " ")                   
    if (!is.na(scores_outliers[[i]][j])) {
      # the value of 2 sds is used here as an example. You will have to come up
      # with a suitable threshold for your data
      if (abs(scores_outliers[[i]][j]) > 2) {
        cat("Variable = ", names(scores_outliers)[i], "Z-score = ",
            scores_outliers[[i]][j], "\n")
      }
    }
  }
}  

#----------------------------- Survey Duration -------------------------------------#

# It is better to create separate variables as described in answer to Question 9 and then subsequently calculate the survey duration to avoid any erroneous results. 

########################## Calculating Duration #####################################

# Let us get the duration of each survey

dummy_data$duration <- round(as.numeric(dummy_data$endtime - dummy_data$starttime))
duration_outliers <- scores(dummy_data$duration[!is.na(dummy_data$duration)], type = "z")

for (i in 1:length(duration_outliers)) {
  if(abs(duration_outliers[][i]) > 2) {
    cat("Z Scores =", duration_outliers[][i], "Duration =",
        dummy_data$duration[i], "\n")
  }
}

#------------------------ Enumerator checks ----------------------------------------#

# As a practice, we should look at enumerator level checks. Also, we may extend this for enumerator pairs or enumerator teams.

################### Enumerator level average survey duration ########################

overall_avg_duration <- mean(dummy_data$duration[!is.na(dummy_data$duration)])

dummy_data %>%
  group_by(surveyor_id) %>%
  summarise(duration_mean = mean(duration),
            overall_avg_duration,
            perc_diff_avg = ((duration_mean - overall_avg_duration) / overall_avg_duration) * 100)

################### Enumerator level distribution checks #############################

dummy_data %>%
  group_by(surveyor_id) %>%                                     # group by surveyor id
  summarise_each(funs(sum(is.na(.))))                           # for missing values

dummy_data %>%               
  group_by(surveyor_id) %>%                                     # group by surveyor id
  summarise_each(funs(n_distinct_no_na(.)))                     # for distinct values

dummy_data %>%
  group_by(surveyor_id) %>%                                     # group by surveyor id
  summarize_each(funs(sum(dummy_data[!sapply(dummy_data, is.character)] == -999))) 
# -999 is used as and example. Run this for all the codes in your survey

dummy_data %>%
  group_by(surveyor_id) %>%                                     # group by surveyor id 
  summarize_each(funs(sum(dummy_data[sapply(dummy_data, is.character)] == "-999")))
# "-999" is used as and example. Run this for all the codes in your survey

# Question 14 : We have been using -999 as an example code throughout this template? Can 
# you give some cases where responses needs to be specially coded like this?

#-------------------------------- Productivity -------------------------------------#

################### Summary of daily average productivity ########################### 

dummy_data <- dummy_data %>%
  arrange(surveydate) %>%                                        # arrange by survey date
  group_by(surveydate) %>%                                       # group by survey date
  mutate(surveydate_N=n())

########################### Overall Productivity Histogram ##########################

productivity_plot <- ggplot(dummy_data, aes(dummy_data$surveydate)) +
  geom_histogram(binwidth=1)

library(ggthemes)

productivity_plot  + scale_color_fivethirtyeight("cyl") 
                   + theme_fivethirtyeight() 
                
# If you want to see a detailed example with ggplot, please see last section

####################### Enumerator level productivity ###############################

dummy_data %>%
  group_by(surveyor_id) %>% 
  summarise(days_worked = length(unique(surveydate)), total_surveys_done = n(), daily_average = total_surveys_done/days_worked)

############################# Answers and Bonus exercise ############################

# Answer to Q1:

# View
View(dummy_data)
# list the variables
names(dummy_data)
# list the structure of mydata
str(dummy_data)
# dimensions of an object
dim(dummy_data)
# class
class(dummy_data)
# print 
dummy_data
# print first 10 rows of mydata
head(dummy_data, n=10)
# print last 5 rows of mydata
tail(dummy_data, n=5)


# Answer to Q2:
# 
require(ggplot2)

# Let us create an arbitrary data frame 
require(zoo)
set.seed(1)
## example data
dat <- data.frame(X = cumsum(rnorm(100)), Y = cumsum(rnorm(100)),
                  Z = cumsum(rnorm(100)))
## convert to multivariate zoo object
datz <- zoo(dat)
## plot it
plot(datz)

# Answer to Q3:

dummy_data_example <- read_dta("dummy_main.dta")

# Answer to Q4: 

dummy_data_summary = summary(dummy_data$surveyor_id)
#dummy_data_summary
paste("surveyor_id has", dummy_data_summary[7] ,"missing value(s)", sep = " ")

# Answer to Q5:

for (Var in names(dummy_data)) {
  missing <- as.numeric(sum(is.na(dummy_data[,Var])))
  if (missing > 0) {
    print(Var,missing)
  }
}

# Answer to Q6 :

missing <- sapply(dummy_data, function(x) sum(is.na(x)))
# print missing to see the content

# Answer to Q7:

dummy_data <- dummy_data %>%
  arrange(surveyor_id) %>%                     ## sorted by surveyor id
  group_by(surveyor_id) %>%                    ## group by surveyor id
  mutate(N=n())

sum(dummy_data$N > 1)

# Answer to Q8:

all.equal(variable_list_master, variable_list_using)
identical(variable_list_master, variable_list_using)

# Answer to Q9: 

# SurveyCTO generally updates the time related variables when you change the survey at 
# a later time and date. So, it is better to create two dummy variable for start time and end # time and do all the checks for date and time.

# Answer to Q10:

for(Var in names(dummy_data))
{
  if(sapply(dummy_data[, Var], class) == "character")
    print(colMeans(is.na(dummy_data[, Var])))
}


for(Var in names(dummy_data))
{
  if(sapply(dummy_data[, Var], class) != "character")
    print(colMeans(is.na(dummy_data[, Var])))
}

# Answer to Q11:

# use as.character.POSIXt() on POSIXt variable

# Answer to Q12:

# This function calculates normal, t, chi-squared, IQR and MAD scores of given data. "z" calculates normal scores (differences between each value and the mean divided by sd), "t" calculates t-Student scores (transformed by (z*sqrt(n-2))/sqrt(z-1-t^2) formula, "chisq" gives chi-squared scores (squares of differences between values and mean divided by variance. For the "iqr" type, all values lower than first and greater than third quartile is considered, and difference between them and nearest quartile divided by IQR are calculated. For the values between these quartiles, scores are always equal to zero. "mad" gives differences between each value and median, divided by median absolute deviation.

# Answer to Q13:

# Run the following line of code if you need only the numeric type variable
# scores_outliers <- scores_outliers[sapply(scores_outliers, class) == "numeric"]
 
# Answer to Q14:
 
# There might be various cases where you would need coding in response to a specific answer. For example, refusing to answer a question may be coded with a special negative number. Similarly, you might also want to specially code "don't know" as a response.

# Five Thirty Eight

# Fun part. let us make the graph more interesting with Five Thirty Eight theme. Thanks to @minimaxir:-)

fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

ggplot(dummy_data, aes(dummy_data$surveydate)) +
  geom_histogram(binwidth=1, fill="#c0392b", alpha=0.75) +
  fte_theme() +
  labs(title="Number of surveys", x="Date", y="Number of surveys") +
  geom_hline(yintercept=0, size=0.4, color="black")

# Try with other theme plots! Maybe NYT and/or World Bank

# End
