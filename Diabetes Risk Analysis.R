
# DIABETES PROJECT
# My Name: Emaan Tahir Abbasi
# Date:28/02/2026

# STEP 1: READ THE DATA
mydata <- read.csv('C:/Users/emaan/Downloads/diabetes.csv')  # "Reads the diabetes CSV file from my computer and stores it in 'mydata'. 

# PART A: EXPLORING THE DATA (EDA)

# How many patients?
total <- nrow(mydata)                  # Counts total number of rows (patients) and stores in 'total'."
print(paste("Total patients:", total)) #Prints the total number of patients (768).



# What's in the data?
print("Column names:") # Prints a heading for column names.


print(names(mydata)) #Shows all 9 column names in the dataset.

# First few patients
print("First 5 patients:") #Prints a heading for first few rows.#
print(head(mydata, 5)) #Shows the first 5 rows to see what data looks like.

# Summary of all numbers
print("Summary of data:") #Prints a heading for summary statistics.
print(summary(mydata)) #Gives min, max, average, and quartiles for each column."

# Count diabetes
diab_count <- table(mydata$Outcome) #"Counts how many patients have diabetes (1) and how many don't (0)."
print("Diabetes count (0=No, 1=Yes):") #Prints a heading for diabetes count."
print(diab_count) #Shows the counts: 500 non-diabetic, 268 diabetic."*

no_diab <- diab_count[1] #Shows the counts: 500 non-diabetic, 268 diabetic."*
yes_diab <- diab_count[2] #Stores the second value (diabetic count = 268) in 'yes_diab'."*
no_pct <- round(no_diab/total*100, 1) #Calculates percentage of non-diabetic patients (500/768 × 100 = 65.1%)."
yes_pct <- round(yes_diab/total*100, 1) #Calculates percentage of diabetic patients (268/768 × 100 = 34.9%)."*


print(paste("No Diabetes:", no_diab, "(", no_pct, "%)")) #Prints non-diabetic count and percentage."
print(paste("Has Diabetes:", yes_diab, "(", yes_pct, "%)")) #Prints diabetic count and percentage."



# Check zeros in important columns
print("Checking zeros in medical data:") #"Prints a heading for zero value check."
print(paste("Glucose zeros:", sum(mydata$Glucose == 0))) #Counts how many patients have glucose = 0 (possible missing data)."*
print(paste("BMI zeros:", sum(mydata$BMI == 0))) #"Counts how many patients have BMI = 0 (possible missing data)."*
print(paste("Blood Pressure zeros:", sum(mydata$BloodPressure == 0))) #Counts how many patients have blood pressure = 0 (possible missing data)."*


# handling zero/missing values option 1
#This would remove all rows with zeros, but we would lose some patients."
clean_data <- mydata
clean_data <- clean_data[clean_data$Glucose != 0, ] #Removes 5 rows
clean_data <- clean_data[clean_data$BMI != 0, ]  #removes 11
clean_data <- clean_data[clean_data$BloodPressure != 0, ]  #removes 35
print(paste("Rows after removing zeros:", nrow(clean_data))) 

# OPTION 2: Replace zeros with mean preserving the data
mydata$Glucose[mydata$Glucose == 0] <- mean(mydata$Glucose[mydata$Glucose > 0], na.rm=TRUE)
mydata$BMI[mydata$BMI == 0] <- mean(mydata$BMI[mydata$BMI > 0], na.rm=TRUE)

# Averages for each group
no_diab_data <- mydata[mydata$Outcome == 0, ] #"Creates a new dataset with ONLY non-diabetic patients (Outcome = 0)."*
yes_diab_data <- mydata[mydata$Outcome == 1, ] #"Creates a new dataset with ONLY diabetic patients (Outcome = 1)."*

print("AVERAGES - No Diabetes:")   #Prints heading for non-diabetic averages."
print(paste("  Glucose:", round(mean(no_diab_data$Glucose), 1))) #Calculates and prints average glucose for non-diabetic patients (110
print(paste("  BMI:", round(mean(no_diab_data$BMI), 1))) #Calculates and prints average BMI for non-diabetic patients (30)."
print(paste("  Age:", round(mean(no_diab_data$Age), 1)))  #Calculates and prints average age for non-diabetic patients (31)."*

print("AVERAGES - Has Diabetes:")  ##Prints heading for diabetic averages."
print(paste("  Glucose:", round(mean(yes_diab_data$Glucose), 1))) #Calculates and prints average glucose for diabetic patients (141)."
print(paste("  BMI:", round(mean(yes_diab_data$BMI), 1))) #Calculates and prints average BMI for diabetic patients (35)."
print(paste("  Age:", round(mean(yes_diab_data$Age), 1)))  #Calculates and prints average age for diabetic patients (37)."

# PART B: 5 VISUALIZATIONS

# Set up to show 2 rows and 3 columns of charts
par(mfrow = c(2, 3)) #"Sets up the plot window to show 2 rows and 3 columns of charts (6 charts total)."

# CHART 1: HISTOGRAM - Age

hist(mydata$Age,                    #Creates a histogram of the Age column."
     main = "1. Age of Patients",   #"Adds title '1. Age of Patients' to the histogram."
     xlab = "Age (years)",          #"Labels the x-axis as 'Age (years)'."
     ylab = "Number of Patients",   #"Labels the y-axis as 'Number of Patients'."
     col = "lightblue",              #Fills the histogram bars with light blue color."
     border = "black")              #Adds black borders around each bar."


# CHART 2: BAR CHART - Diabetes Count

counts <- c(no_diab, yes_diab)  #"Creates a vector with the two counts (500 and 268)."
names(counts) <- c("No Diabetes", "Diabetes")  # "Names the two bars as 'No Diabetes' and 'Diabetes'."


barplot(counts,                                       #"Creates a bar chart using the counts."
        main = "2. Diabetes Count",                     #Adds title '2. Diabetes Count' to the bar chart."
        ylab = "Number of Patients",                   #Labels the y-axis as 'Number of Patients'."
        col = c("lightpink", "lavenderblush"),          #"Colors the first bar light pink and second bar lavender
        ylim = c(0, 600))                                #Sets y-axis range from 0 to 600 to fit both bars."*
text(x = c(0.7, 1.9), y = counts + 20, labels = counts)   #Adds the numbers (500 and 268) on top of the bars."

# CHART 3: BOX PLOT - Glucose by Diabetes

boxplot(Glucose ~ Outcome, data = mydata,  #"Creates box plots comparing Glucose grouped by Outcome."
        names = c("No Diabetes", "Diabetes"),   #Labels the two box plots as 'No Diabetes' and 'Diabetes'.
        main = "3. Glucose Levels",             #"Adds title '3. Glucose Levels' to the box plot."
        ylab = "Glucose",                       #"Labels the y-axis as 'Glucose'
        col = c("lightgreen", "pink"))            #"Colors first box light green, second box pink."
Q_glu <- quantile(mydata$Glucose, c(0.25, 0.75), na.rm=TRUE)
IQR_glu <- Q_glu[2] - Q_glu[1]
upper_glu <- Q_glu[2] + 1.5 * IQR_glu
lower_glu <- Q_glu[1] - 1.5 * IQR_glu

# Remove BMI outliers
Q_bmi <- quantile(mydata$BMI, c(0.25, 0.75), na.rm=TRUE)
IQR_bmi <- Q_bmi[2] - Q_bmi[1]
upper_bmi <- Q_bmi[2] + 1.5 * IQR_bmi
lower_bmi <- Q_bmi[1] - 1.5 * IQR_bmi




# CHART 4: BOX PLOT - BMI by Diabetes

boxplot(BMI ~ Outcome, data = mydata,             #Creates box plots comparing BMI grouped by Outcome."
        names = c("No Diabetes", "Diabetes"),     #"Labels the two box plots as 'No Diabetes' and 'Diabetes'."
        main = "4. BMI Levels",                   #Adds title '4. BMI Levels' to the box plot."
        ylab = "BMI",                             #  "Labels the y-axis as 'BMI'."
        
        col = c("steelblue", "coral"))       # Colors first box seagreen, second box coral

# Remove BMI outliers
Q_bmi <- quantile(mydata$BMI, c(0.25, 0.75), na.rm=TRUE)
IQR_bmi <- Q_bmi[2] - Q_bmi[1]
upper_bmi <- Q_bmi[2] + 1.5 * IQR_bmi
lower_bmi <- Q_bmi[1] - 1.5 * IQR_bmi
#clean our data with outliers
clean_data <- mydata[
  mydata$Glucose >= lower_glu & mydata$Glucose <= upper_glu &
    mydata$BMI >= lower_bmi & mydata$BMI <= upper_bmi, ]

print(paste("Removed", nrow(mydata) - nrow(clean_data), "outliers"))
# CHART 5: SCATTER PLOT - Age vs Glucose

plot(mydata$Age, mydata$Glucose,                                     #Creates scatter plot with Age on x-axis and Glucose on y-axis."
     main = "5. Age vs Glucose",                                     #"Adds title '5. Age vs Glucose' to the scatter plot."
     xlab = "Age",                                                    #"Labels the x-axis as 'Age'.
     ylab = "Glucose",                                                #  "Labels the y-axis as 'glucose'
     col = ifelse(mydata$Outcome == 1, "lightgoldenrod", "thistle"),   #Colors dots: yellow for diabetic, purple for non-diabetic."
     pch = 16)                                                        #Makes the dots solid circles instead of hollow."
print("End Of Project")



