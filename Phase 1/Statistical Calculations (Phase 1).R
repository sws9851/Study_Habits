# Define the function to find the statistical mode
get_mode <- function(x) {
  # Get the frequency of each unique value
  uniq_vals <- unique(x)
  counts <- tabulate(match(x, uniq_vals))
  
  # Check if all values have the same frequency
  if (length(unique(counts)) == 1) {
    return("No mode")  # Return "No mode" if all values have the same frequency
  }
  
  # Return the most frequent value if there is a mode
  uniq_vals[which.max(counts)]
}
#---------- 1- intro ------------------------
#The data was gathered from a comprehensive survey designed to explore the connection between students' study habits and their Grade Point Averages (GPA). This study aimed to analyze how different approaches to studying may influence academic performance, providing insights into effective strategies for achieving higher grades.
#---------- 2- data structure ------------------------

str(Study_Habits)
dim(Study_Habits)
colnames(Study_Habits,5)
#---------- 3- data types ------------------------
# ID ----> Quantitative,Nominal
# Start time ----> Quantitative,Interval
# Completion time ----> Quantitative,Interval
# Email ----> Qualitative,Nominal
# Name ----> Qualitative,Nominal
# Gender ----> Qualitative,Nominal
# Age ----> Quantitative,Ratio
# Parents’ Education Level ----> Qualitative,Ordinal
# Parents’ Education Level2 ----> Qualitative,Ordinal
# Family Structure ----> Qualitative,Nominal
# Lifestyle Factors ----> Qualitative,Nominal
# Digital Access ----> Qualitative,Ordinal
# Study Environment ----> Qualitative,Nominal
# Studying Methods ----> Qualitative,Nominal
# Taking Notes ----> Qualitative,Ordinal
# Reviewing Notes ----> Qualitative,Ordinal
# Organized material ----> Qualitative,Ordinal
# Homework's time ----> Quantitative,Ratio
# Planning for study Sessions ----> Qualitative,Ordinal
# Exam Preparation ----> Qualitative,Ordinal
# Seek academic help ----> Qualitative,Ordinal
# Confidence in Academic success ----> Qualitative,Ordinal
# Study Habits Impact ----> Qualitative,Ordinal
# GPA ----> Quantitative,Ratio
# Studying hours ----> Quantitative,Ratio
#---------- 4- variables' measurements ------------------------
#------- 1- ID -------
#------- 2- Start time -------
#------- 3- Completion time -------
#------- 4- Email -------
#------- 5- Name -------
#Those variables are out of consideration, as they will not provide any information.
#------- 6- Gender -------
#As this variable is nominal, we can only calculate the mode for this variable
get_mode(Study_Habits$Gender)
#It can be concluded that the number of males who participated in this survey is more than the females who participated
#to make a bar chart for this variable we have to change it to numeric data first
gender_counts <- table(Study_Habits$Gender)
barplot(gender_counts,
        las = 2)
#the graph shows that the portion of males is higher than females
#------- 7- Age -------
#As this variable is Ratio, we can calculate all of the central and variability measures
#centeral measure:
mean(Study_Habits$Age)
#it returns NA because r deals with the age values as characters, not numbers. So, we have to make it numerical values first then calculate the mean again
# Convert Age to numeric
Age <- 
  as.numeric(Study_Habits$Age)
#calculating the new mean
mean(Age)
#it appears that the average of the ages of all participants was 18.17
get_mode(Age)
#The most frequent value is 18 years old.
median(Age)
#This means that 50% of the participants are above 18 and the other 50% are below it.
#Variability measures
range(Age)
#it shows that the elder participant is 20 years old, and the youngest is 15, making the range = 20-15=5
IQR(Age)
#The difference between the 75th percentile of the ages and the 25th percentile = 1
var(Age)
#It appears that the variability between the data is 1.0106 years^2
sd(Age)
#It shows that 68% of the data lies between 19.005 and 16.995
hist(Age,
     main = "Histogram of Age")
#it shows that the graph is skewed to the left
#to check for outliers we can do a box blot
boxplot(Age,
        main = "Box Plot of Age")
#the box plot shows that we have outliers in 16 and 15. so we have to remove them
# Remove outliers based on IQR (Interquartile Range)
Study_Habits$Age <- as.numeric(Study_Habits$Age)
Q1 <- quantile(Age, 0.25)
Q3 <- quantile(Age, 0.75)
IQR_value <- Q3 - Q1
# Define the outlier threshold
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
# Filter out the outliers
filtered_study_habits <- Study_Habits[Study_Habits$Age >= lower_bound & 
                                        Study_Habits$Age <= upper_bound, ]
#finding the updated box plot
boxplot(filtered_study_habits$Age, main = "Box Plot of Age (Outliers Removed)")
#finding the updated measures
#updated central measure:
mean(filtered_study_habits$Age)
#it appears that the average of the ages of all participants was 18.38 instead of 18.17
get_mode(filtered_study_habits$Age)
#no change in mode
median(filtered_study_habits$Age)
#no change in the median as it isn't affected by outliers
#Variability measures
range(filtered_study_habits$Age)
#it shows that the elder participant is 20 years old, and the youngest is 17 instead of the outlier (15), making the range = 20-17=3 instead of 5
IQR(filtered_study_habits$Age)
#no change in IQR as it isn't affected by outliers
var(filtered_study_habits$Age)
#the new variability between the data is 0.5. years^2 which is very different from the old one
sd(filtered_study_habits$Age)
#It shows that 68% of the data lies between 19.08 and 17.65 not 19.005 and 16.995
#------- 8- Parent's education level -------
#As this variable is ordinal, we can only calculate Mode, Median,Range,IQR for this variable
#centeral measure:
get_mode(Study_Habits$`Parents Education Level`)
#The most frequent value is a "University degree or higher". Shows that the majority of participants have fathers who have a university degree or higher than it
#if we try to get the median directly we will get an error saying"need numeric data". This happened because r deals with the values as characters not ordered options. So, we have to make it clear that this is an ordinal data
# Convert 'Parents’ Education Level' to an ordered factor
Study_Habits$`Parents Education Level` <- factor(
  Study_Habits$`Parents Education Level`,
  levels = c("No formal education", 
             "Primary school", 
             "Secondary school", 
             "University degree or higher"),
  ordered = TRUE
)
# Convert the ordered factor to a numeric representation
numeric_parents_education <- as.numeric(Study_Habits$`Parents Education Level`)
#finding the new median
median(numeric_parents_education, na.rm = TRUE)
# it returns 4 referring to "University degree or higher", this happened because our data is skewed to the left, meaning more than half of the participants chose the highest category.
#Variability measures
range(Study_Habits$`Parents Education Level`)
#it shows that the highest education level of father's participants is "University degree or higher" , and the lowest ones are "Primary school"
IQR(Study_Habits$`Parents Education Level`)
#it returns 0 meaning there is no difference between Q3 and Q1. This happened because the majority of the participants chose "University degree or higher", making both the Q3 and Q1 "University degree or higher"
father_education_counts <- table(Study_Habits$`Parents Education Level`)
barplot(father_education_counts,
        las = 2)
#it shows that the highest portion chose "University degree or higher" and the lowest portion chose "No formal education"
#------- 9- Parent's education level2 -------
#As this variable is ordinal, we can only calculate Mode, Median,Range,IQR for this variable
#centeral measure:
get_mode(Study_Habits$`Parents Education Level2`)
#The most frequent value is a "University degree or higher". Shows that the majority of participants have mothers who have a university degree or higher than it
#if we try to get the median directly we will get an error saying"need numeric data". This happened because r deals with the values as characters not ordered options. So, we have to make it clear that this is an ordinal data
# Convert 'Parents’ Education Level' to an ordered factor
Study_Habits$`Parents Education Level2` <- factor(
  Study_Habits$`Parents Education Level2`,
  levels = c("No formal education", 
             "Primary school", 
             "Secondary school", 
             "University degree or higher"),
  ordered = TRUE
)
# Convert the ordered factor to a numeric representation
numeric_parents2_education <- as.numeric(Study_Habits$`Parents Education Level2`)
#finding the new median
median(numeric_parents2_education, na.rm = TRUE)
# it returns 4 referring to "University degree or higher", this happened because our data is skewed to the left, meaning more than half of the participants chose the highest category.
#Variability measures
range(Study_Habits$`Parents Education Level2`)
#it shows that the highest education level of mother's participants is "University degree or higher" , and the lowest ones are "No formal education"
IQR(Study_Habits$`Parents Education Level2`)
#it returns 0 meaning there is no difference between Q3 and Q1. This happened because the majority of the data chose "University degree or higher", making both the Q3 and Q1 "University degree or higher"
mother_education_counts <- table(Study_Habits$`Parents Education Level2`)
barplot(mother_education_counts,
        las = 2)
#it shows that the highest portion chose "University degree or higher" and the lowest portion chose "No formal education" and "primary school"
#------- 10- Family Structure -------
#As this variable is nominal, we can only calculate the mode for this variable
get_mode(Study_Habits$`Family Structure`)
#We can conclude that the majority of participants in this survey have a Two-parent household 
family_counts <- table(Study_Habits$`Family Structure`)
barplot(family_counts,
        las = 2)
#the graph shows that the portion of Two-parent households is the highest
#------- 11- Lifestyle Factors -------
#As this variable is nominal, we can only calculate the mode for this variable
get_mode(Study_Habits$`  Lifestyle Factors (Urban vs. Rural Living)`)
#We can conclude that the majority of participants in this survey live in an urban area
life_counts <- table(Study_Habits$`  Lifestyle Factors (Urban vs. Rural Living)`)
barplot(life_counts,
        las = 2)
#the graph shows that the portion of Urban area is the highest portion
#------- 12- Digital Access -------
#As this variable is ordinal, we can only calculate Mode, Median,Range,IQR for this variable
#centeral measure:
get_mode(Study_Habits$`Digital Access`)
#The most frequent value is "Good access". Showing that the majority of participants have good access to the digital
median(Study_Habits$`Digital Access`)
#This means that 50% of the participants chose "Good access" or higher than it and the other 50% chose it or lower.
#Variability measures
range(Study_Habits$`Digital Access`)
#it shows that the highest one chose "Excellent access", and the lowest was "Moderate access"
#as this variable is not numerical, so we can not calculate the IQR
digital_counts <- table(Study_Habits$`Digital Access`)
barplot(digital_counts,
        las = 2)
#it shows that the highest portion chose "Good access" and the lowest portion chose "limited access"
#------- 13- Study Environment -------
#As this variable is nominal, we can only calculate the mode for this variable
get_mode(Study_Habits$`Study Environment`)
#We can conclude that the majority of participants in this survey prefer studying at home
study_counts <- table(Study_Habits$`Study Environment`)
barplot(study_counts,
        las = 2)
#the graph shows that the portion of people who study at home is the highest
#------- 14- Studying Methods -------
#As this variable is nominal, we can only calculate the mode for this variable
get_mode(Study_Habits$`Studying Methods`)
#We can conclude that the majority of participants in this survey prefer watching educational videos to study
method_counts <- table(Study_Habits$`Studying Methods`)
barplot(method_counts)
#this was a multiple choice question where the user can choose more than one option, so r considered every person chose a unique option. to solve this we have to teach r that people chose the same ones
methods_list <- unlist(strsplit(as.character(Study_Habits$`Studying Methods`), split = ";"))
methods_list <- trimws(methods_list)
method_counts <- table(methods_list)

# now, reCreating a bar plot
barplot(method_counts,
        las = 2)
#the graph shows that the portion of people who watch educational videos to study is the highest
#------- 15- Taking Notes -------
#As this variable is ordinal, we can only calculate Mode, Median,Range,IQR for this variable
#centeral measure:
get_mode(Study_Habits$`Taking Notes`)
#The most frequent value is "Sometimes". Showing that the majority of participants are taking notes sometimes
median(Study_Habits$`Taking Notes`)
#This means that 50% of the participants chose "Sometimes" or higher than it and the other 50% chose it or lower.
#Variability measures
range(Study_Habits$`Taking Notes`)
#it shows that the highest one chose "Sometimes", and the lowest was "Never"
#as this variable is not numerical, so we can not calculate the IQR
notes_counts <- table(Study_Habits$`Taking Notes`)
barplot(notes_counts,
        las = 2)
#It shows that all of the participants whether take notes sometimes or never
#------- 16- Reviewing Notes -------
#As this variable is ordinal, we can only calculate Mode, Median,Range,IQR for this variable
#centeral measure:
get_mode(Study_Habits$`Reviewing Notes`)
#The most frequent value is "Sometimes". Shows that the majority of participants sometimes review their notes
median(Study_Habits$`Reviewing Notes`)
#This means that 50% of the participants review their notes more frequently than rearly and the other 50% review them less frequent
#Variability measures
range(Study_Habits$`Reviewing Notes`)
#it shows that the most frequently reviewing the notes one chose "Always", and the least frequent chose "Sometimes"
#as this variable is not numerical, so we can not calculate the IQR
review_counts <- table(Study_Habits$`Reviewing Notes`)
barplot(review_counts,
        las = 2)
#shows that the highest portion of participants sometimes review their notes
#------- 17- Organized material -------
#As this variable is ordinal, we can only calculate Mode, Median,Range,IQR for this variable
#centeral measure:
get_mode(Study_Habits$`Organized material`)
#The most frequent value is "Somewhat organized". Showing that the majority of participants were somewhat organized
median(Study_Habits$`Organized material`)
#This means that 50% of the participants are more organized than "Somewhat organized" other 50% are less organized
#Variability measures
range(Study_Habits$`Organized material`)
#it shows that the most organized one chose "Very organized", and the least one chose "Neutral"
#as this variable is not numerical, so we can not calculate the IQR
material_counts <- table(Study_Habits$`Organized material`)
barplot(material_counts,
        las = 2)
#shows that the highest portion of participants are somewhat organized
#------- 18- Homework's time -------
#As this variable is Ratio, we can calculate all of the central and variability measures
#centeral measure:
mean(Study_Habits$`Homework's time`)
#it returns NA because r is dealing with the homework's time values as characters, not numbers. So, we have to make it numerical values first then calculate the mean again
# Convert Homework's time to numeric
Study_Habits$`Homework's time` <- 
  as.numeric(Study_Habits$`Homework's time`)
#calculating the new mean
mean(Study_Habits$`Homework's time`)
#it appears that the average of the Hours per week spent doing homework by participants is 10.714
get_mode(Study_Habits$`Homework's time`)
#The most frequent value is 3, showing that the most repeated number of hours spent doing homework per week is 3 hours
median(Study_Habits$`Homework's time`)
#This means that 50% of the participants spent more than 7 hours doing their homework per week and the other 50% less than 7.
#Variability measures
range(Study_Habits$`Homework's time`)
#it shows that the lowest one spend 0 hours doing their homework, and the highest one spends 67
IQR(Study_Habits$`Homework's time`)
#The difference between the 75th percentile of the homework hours and the 25th percentile = 8
var(Study_Habits$`Homework's time`)
#It appears that the variability between the data is 121.7 hours^2
sd(Study_Habits$`Homework's time`)
#It shows that 68% of the data lies between 21.73 and -1.67
#------- 19- Planning for study Sessions -------
#As this variable is ordinal, we can only calculate Mode, Median,Range,IQR for this variable
#centeral measure:
get_mode(Study_Habits$`Planning for study Sessions`)
#The most frequent value is "Often". Showing that the majority of participants often plan for their study sessions
median(Study_Habits$`Planning for study Sessions`)
#This means that 50% of the participants plan more than "Often" about their study sessions, and the other 50% plan less
#Variability measures
range(Study_Habits$`Planning for study Sessions`)
#it shows that the most planning one chose "Always", and the least one chose "Sometimes"
#as this variable is not numerical, so we can not calculate the IQR
planning_counts <- table(Study_Habits$`Planning for study Sessions`)
barplot(planning_counts,
        las = 2)
#shows that the highest portion of participants often plan for their study session
#------- 20- Exam Preparation -------
#As this variable is ordinal, we can only calculate Mode, Median,Range,IQR for this variable
#centeral measure:
get_mode(Study_Habits$`Exam Preparation`)
#The most frequent value is "Often". Showing that the majority of participants often feel prepared before the exam
median(Study_Habits$`Exam Preparation`)
#This means that 50% of the participants feel prepared more than "Often", and the other 50% feel less
#Variability measures
range(Study_Habits$`Exam Preparation`)
#it shows that the most confident one chose "Always", and the least one chose "Sometimes"
#as this variable is not numerical, so we can not calculate the IQR
exam_counts <- table(Study_Habits$`Exam Preparation`)
barplot(exam_counts,
        las = 2)
#shows that the highest portion of participants often feels prepared before the exam
#------- 21- Seek academic help -------
#As this variable is ordinal, we can only calculate Mode, Median,Range,IQR for this variable
#centeral measure:
get_mode(Study_Habits$`Seek academic help`)
#The most frequent value is "Sometimes". Shows that the majority of participants sometimes seek academic help
median(Study_Habits$`Seek academic help`)
#This means that 50% of the participants seek academic help less frequently than "Rarely", and the other 50% seek more frequent
#Variability measures
range(Study_Habits$`Seek academic help`)
#it shows that the most frequent one seeking academic help chose "Always", and the least one chose "Sometimes"
#as this variable is not numerical, so we can not calculate the IQR
help_counts <- table(Study_Habits$`Seek academic help`)
barplot(help_counts,
        las = 2)
#shows that the highest portion of participants often seek academic help
#------- 22- Confidence in Academic success -------
#As this variable is ordinal, we can only calculate Mode, Median,Range,IQR for this variable
#centeral measure:
get_mode(Study_Habits$`Confidence in Academic success`)
#The most frequent value is "Moderately confident". Shows that the majority of participants feel moderately confident about their academic success
median(Study_Habits$`Confidence in Academic success`)
#This means that 50% of the participants feel more confident than "Moderately confident", and the other 50% are less confident
#Variability measures
range(Study_Habits$`Confidence in Academic success`)
#it shows that the most confident one chose "Extremely confident", and the least one chose "Very confident"
#as this variable is not numerical, so we can not calculate the IQR
success_counts <- table(Study_Habits$`Confidence in Academic success`)
barplot(success_counts,las = 2)
#shows that the highest portion of participants feel moderately confident about their academic success
#------- 23- Study Habits Impact -------
#As this variable is ordinal, we can only calculate Mode, Median,Range,IQR for this variable
#centeral measure:
get_mode(Study_Habits$`Study Habits Impact`)
#The most frequent value is "Somewhat positively". Shows that the majority of participants feel that their study habits affect them somewhat positively
median(Study_Habits$`Study Habits Impact`)
#This means that 50% of the participants' study habits affect them more positively than "Somewhat positively", and the other 50% are less organized
#Variability measures
range(Study_Habits$`Study Habits Impact`)
#it shows that the most positively affected ones chose "Very positively", and the most negatively affected ones chose "Negatively"
#as this variable is not numerical, so we can not calculate the IQR
habits_counts <- table(Study_Habits$`Study Habits Impact`)
barplot(habits_counts,
        las = 2)
#shows that the highest portion of participants feel that their study habits affect them somewhat positively
#------- 24- GPA -------
#As this variable is Ratio, we can calculate all of the central and variability measures
#centeral measure:
mean(Study_Habits$GPA)
#it appears that the average GPA of all participants was 4.56 which is impossible to happen, meaning we have outliers
get_mode(Study_Habits$GPA)
#The most frequent value is 3.5
median(Study_Habits$GPA)
#This means that 50% of the participants have a GPA above 3.7 and the other 50% have below it.
#Variability measures
range(Study_Habits$GPA)
#it shows that the highest participant got 94 out of 4 (outlier) , and the lowest is 0 out of 4 (Outlier), making the range = 94-0=94
IQR(Study_Habits$GPA)
#The difference between the 75th percentile of the GPAs and the 25th percentile = 0.3
var(Study_Habits$GPA)
#It appears that the variability between the data is 107.4993
sd(Study_Habits$GPA)
#It shows that the standard deviation of the data is 10.36819, leading to a very big variability between the data
GPA = as.numeric(Study_Habits$GPA)
hist(GPA,
     main = "Box Plot of GPA")
#it shows that the graph is skewed to the right because of the outliers who entered their GPA on a scale out of 100 not 4
#to check for outliers we can do a box blot
boxplot(GPA,
        main = "Box Plot of GPA")
#the box plot shows that we have many outliers. so we have to remove them
# Remove outliers based on IQR (Interquartile Range) and execute any values more than 4
GQ1 <- quantile(GPA, 0.25)
GQ3 <- quantile(GPA, 0.75)
GIQR_value <- GQ3 - GQ1
# Define the outlier threshold
Glower_bound <- GQ1 - 1.5 * GIQR_value
Gupper_bound <- GQ3 + 1.5 * GIQR_value
# Filter the data
Study_Habits$GPA <- as.numeric(as.character(Study_Habits$GPA))
filtered_GPA <- Study_Habits[Study_Habits$GPA >= Glower_bound & 
                               Study_Habits$GPA <= Gupper_bound & 
                               Study_Habits$GPA <= 4, ]

# Box plot of the filtered GPA
boxplot(filtered_GPA$GPA, main = "Boxplot of Filtered GPA")
#finding the updated measures
#updated central measure:
mean(filtered_GPA$GPA)
#it appears that the average of the GPA of treated data was 3.666 instead of 4.56
get_mode(filtered_GPA$GPA)
#no change in mode
median(filtered_GPA$GPA)
#no change in the median as it isn't affected by outliers
#Variability measures
range(filtered_GPA$GPA)
#it shows that the participant with the highest GPA got 4.0, and the one with the lowest GPA got 3.1 instead of 94 and 0
IQR(filtered_GPA$GPA)
#no change in IQR as it isn't affected by outliers
var(filtered_GPA$GPA)
#the new variability between the data is 0.05 which is very different from the old one
sd(filtered_GPA$GPA)
#It shows that the standard deviation of the treated data is 0.22instead of 10.37
#------- 25- Studying hours -------
#As this variable is Ratio, we can calculate all of the central and variability measures
#central measure:
mean(Study_Habits$`Studying hours`)
#it appears that the average of the studying hours per week of all participants was 21.9
get_mode(Study_Habits$`Studying hours`)
#The most frequent number of hours is 30. 
median(Study_Habits$`Studying hours`)
#This means that 50% of the participants study more than 21 hours peer week and the other 50% study less than it.
#Variability measures
range(Study_Habits$`Studying hours`)
#it shows that the one with the highest studying hours per week study 60 hours, and the least study 2, making the range = 60-2=58
IQR(Study_Habits$`Studying hours`)
#The difference between the 75th percentile of the ages and the 25th percentile = 20
var(Study_Habits$`Studying hours`)
#It appears that the variability between the data is 172.45 hours^2
sd(Study_Habits$`Studying hours`)
#It shows that the standard deviation of the data is 13.13
Study_Habits$`Studying hours` <- as.numeric(as.character(Study_Habits$`Studying hours`))
hist(Study_Habits$`Studying hours`,
     main = "Histogram of Studying hours")
#it shows that the graph is skewed to the right
#to check for outliers we can do a box blot
boxplot(Study_Habits$`Studying hours`,
        main = "Box Plot of studying hours")
#the box plot shows that we don't have any outliers