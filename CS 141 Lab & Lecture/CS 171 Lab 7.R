# CS 171 - Lab 7
# YOUR NAME HERE
# PUT TODAY'S DATE HERE

#An important part of data science is cleaning and preparing data before you analyze it.
#In order to properly prepare datasets for analysis, it is necessary to examine your data
#and determine what needs to be done to it. In this exercise, we’ll be examining and cleaning a dataset, as
#well as exploring it.
#We will be using a slightly tweaked version of the Historical Plane Crashes dataset from Kaggle.

#Install packages
#install.packages("tidyr")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("naniar")
#install.packages("stringr")
#install.packages("chron")

#Loading packages so that we can use them
library(tidyr)
library(readr)
library(dplyr)
library(naniar)
library(stringr)
library(chron)

#Loading data into the workspace
data <- read.csv("planecrashdata.csv")
df <- data.frame(data)

#------------------------------------------------------------------------#
#PART 1:  Getting Familiar with the Data (4 points)
#------------------------------------------------------------------------#
#Use the summary and structure functions to answer the following questions:
#1. How many attributes are in this dataset? How many observations?
    # YOUR ANSWER HERE
#2. What dates had 4 plane crashes on the same day? (Hint:  There are 5)
    # YOUR ANSWER HERE
    # YOUR ANSWER HERE
    # YOUR ANSWER HERE
    # YOUR ANSWER HERE
    # YOUR ANSWER HERE
#3. What are the two non-numeric characters in the flight_no column?
    # YOUR ANSWER HERE
    # YOUR ANSWER HERE
#Keep these characters in mind for Part 2.

#------------------------------------------------------------------------#
#PART 2:  Cleaning Data (7 points)
#------------------------------------------------------------------------#
#A good first step for cleaning data is removing NA values. To see if the data has any NA values,
#we'll use the is.na function:
is.na(df)

#It appears that there are no NA values in the dataset. However, if you recall from Part 1, there are two
#characters that are used as NA values. Luckily, there is a package that is useful for dealing with
#NA values and missing data. We will use a function from the naniar package to replace the placeholder
#characters with an NA value.

#Replacing the "?" in the time column with NA
df <- df %>%
  replace_with_na(replace = list(time = "?"))

#Look at the summary again
summary(df)

#Success! The "?" has been replaced with an NA value.
#Write the code below to clean the other columns. You can use the "summary" function to check
#your work and make sure you replaced "?".

#Replacing the "?" in flight_no
# YOUR CODE HERE

#Replacing the "?" in route
# YOUR CODE HERE

#Replacing the "?" in registration
# YOUR CODE HERE

#Replacing the "?" in summary
# YOUR CODE HERE

#Look at the data again with the summary function.
summary(df)

#It looks like the flight_no column has one more placeholder for an NA value. Let's replace it now.
# YOUR CODE HERE

#Now let's check for NA values again.
# YOUR CODE HERE

#As you can see, there are now several NA values in our dataset. Sometimes it is valuable to keep NA values,
#but for this particular exercise, we don't want them in our dataset.
#We'll use na.omit and save it as a different object so that we know it's clean.
planecrash <- na.omit(df)

#Now that there are no NA values in the dataset, we can move on.

#------------------------------------------------------------------------#
#Part 3: Manipulating Data (2 points)
#------------------------------------------------------------------------#
#Let's look at a summary of the data that we have after cleaning.
summary(planecrash)

#Notice how the "aboard" and "fatalities" attributes are not just numbers? They are being interpreted
#as character strings because of the additional details in the parentheses. Luckily, there is a way to
#remove them.

#First, let's clean up the aboard attribute. We'll use a function to extract the number we want by using
#a regular expression. A regular expression is a series of characters used to define a search pattern.
#The one we will use is "[0-9]+". The first part "[0-9]" means that we are looking for a number.
#The "+" means that we are looking for one or more numbers. Since there is a space between the total
#number aboard and the breakdown between passengers and crew, this regular expression will just grab the
#total number of people aboard.

#We will then turn this value from a character to a numeric value using the "as.numeric" function.
#Go ahead and run the following line of code. Use the View function to check if it worked.
planecrash$aboard <- str_extract(planecrash$aboard, "[0-9]+")

#See the difference? Now let's do the same to the fatalities attribute. We can use the same regular
#expression as we did for the aboard attribute.
# YOUR CODE HERE

#The "time" attribute also needs the help of a regular expression to get rid of the "c" in front of some of
#the values. This one is a bit trickier so you don't have to write the regular expression. This regular
#expression states that there should be exactly 2 numbers (denoted by the "{2}") followed by a colon (:) and
#then exactly 2 numbers again.
planecrash$time <- str_extract(planecrash$time, "[0-9]{2}:[0-9]{2}")

#------------------------------------------------------------------------#
#Part 4: Formatting Data (12 points total)
#------------------------------------------------------------------------#
#Let's look at the structure of the data.
str(planecrash)

#Normally, there is an abbreviation of whatever data type each attribute it.
#Notice how all of our attributes just say "Factor w/" and then a large number of levels.
#Let's format this so that the data is in its proper format and type and factor where needed.
#Before we start formatting, let's examine our attributes to see what we need to change.
#Answer the following questions and make sure that you list all 12 attributes.

#1. Which attributes should be character attributes?
    # YOUR ANSWER HERE
#2. Which attributes should be numeric?
    # YOUR ANSWER HERE
#3. Which attributes should be dates or times?
    # YOUR ANSWER HERE

#------------------------------------------------------------------------#
#Character Attributes (3 points)
#------------------------------------------------------------------------#
#We'll use the as.character function to change the appropriate attributes.

#first attribute
# YOUR CODE HERE

#second attribute
# YOUR CODE HERE

#third attribute
# YOUR CODE HERE

#fourth attribute
# YOUR CODE HERE

#fifth attribute
# YOUR CODE HERE

#sixth attribute
# YOUR CODE HERE

#seventh attribute
# YOUR CODE HERE

#-----------#
#Factoring (2 points)
#-----------#
#The only two attributes that really need to be factored are ac_type and operator. Let's factor them.

#Factor ac_type
# YOUR CODE HERE

#Factor operator
# YOUR CODE HERE

#------------------------------------------------------------------------#
#Numeric Attributes (3 points)
#------------------------------------------------------------------------#
#Let's start by formatting all the numeric attributes. This is a pretty simple fix when we use the
#as.numeric function.

#first attribute
# YOUR CODE HERE

#second attribute
# YOUR CODE HERE

#third attribute
# YOUR CODE HERE

#------------------------------------------------------------------------#
#Date and Time Attributes (1 point)
#------------------------------------------------------------------------#
#To format the date, we'll use the as.Date function. First, let's take a look at the values in the date
#attribute using the head function.
head(planecrash)

#So, it looks like the data has an unabbreviated month, followed by the day, and then the 4-digit year.
#Take a look at the slides and figure out what our format code will look like. Fill in the blank below.
planecrash$date <- as.Date(planecrash$date, "_______")

#We'll use chron to format the time. Let's take a look at the data again using the head function.
head(planecrash)

#What format is the time in? Fill in the blank below.
planecrash$time <- chron(times = planecrash$time, format = "______")

#Now that we have finished cleaning and formatting our data, let's save it as a CSV file.
write_csv(planecrash, "planecrashdata-clean.csv")

#Now that our data is saved, we can use the clean data to run analysis on it in the future.
#Please turn in this R file and the clean CSV file to Laulima.

#------------------------------------------------------------------------#
#Bonus
#------------------------------------------------------------------------#
#1. What is the format code for the date attribute after we ran it through as.Date? (1 point)
    # YOUR ANSWER HERE