#Riskspan New Hires SKill Assessment
#Siddharth Dixit

install.packages("tidyverse")
library(readxl)


readxl_example()
riskspan <- readxl_example("RiskSpanSkillsAssessment.xlsx")
# To make this work, I copied the RiskSpanSkillsAssessment.xlsx file into C:\Users\Siddharth\Documents\R\win-library\3.3\readxl\extdata location

data <- read_excel(riskspan, sheet = "Data")
data <- unique(data)
# Just to make sure there are no duplicate rows. There are still 2000 rows so no duplicates found.

# Hence, we have all the data from the 'Data' tab into the dataframe 'Data' in R.

library(dummies)

# REPORT 1

instdes <- dummy(data$LENDER_INST_TYPE_DESCRIPTION)

data <- cbind(data, instdes)

# Loan Counts:

lc1 <- sum(data[,30]) 
lc1

lc2 <- sum(data[,31])
lc2

lc3 <- sum(data[,32])
lc3

lc4 <- sum(data[,33])
lc4

lc5 <- sum(data[,34])
lc5

lcsum <- lc1 + lc2 + lc3 + lc4 + lc5
lcsum

# Average Current Balance

data$cb1 <- data[,15]*data[,30]
acb1 <- sum(data$cb1)/lc1
acb1

data$cb2 <- data[,15]*data[,31]
acb2 <- sum(data$cb2)/lc2
acb2

data$cb3 <- data[,15]*data[,32]
acb3 <- sum(data$cb3)/lc3
acb3

data$cb4 <- data[,15]*data[,33]
acb4 <- sum(data$cb4)/lc4
acb4

data$cb5 <- data[,15]*data[,34]
acb5 <- sum(data$cb5)/lc5
acb5

tacb <- (acb1*lc1 + acb2*lc2 + acb3*lc3 + acb4*lc4 + acb5*lc5)/(lc1 + lc2 + lc3 + lc4 + lc5)
tacb
# Total average validation

sum(data[,15])/2000

# Max Current Balance

max1 <- max(data$cb1)
max1

max2 <- max(data$cb2)
max2

max3 <- max(data$cb3)
max3

max4 <- max(data$cb4)
max4

max5 <- max(data$cb5)
max5

tmax <- max(data[,15])
tmax
# This calculates the total maximum out of all rows

# Min Current Balance

min1 <- sort(data[, 35]) 
min1 <- min1[!!min1][1] # This removes the null values from sorted rows
min1

min2 <- sort(data[, 36]) 
min2 <- min2[!!min2][1]
min2

min3 <- sort(data[, 37]) 
min3 <- min3[!!min3][1]
min3

min4 <- sort(data[, 38]) 
min4 <- min4[!!min4][1]
min4

min5 <- sort(data[, 39]) 
min5 <- min5[!!min5][1]
min5

tmin <- min(min1,min2,min3,min4,min5)
tmin
# This calculates the total minimum out of all rows

# REPORT 2

# This part will be similar to the previous part, we just define new dummy variables 

data$b <- NA

for (i in 1:nrow(data)){
  if (data[i,17] <= 85)
    data[i,40] <- 1
  else if (data[i,17] > 85 & data[i,17] <= 90)
    data[i,40] <- 2
  else if (data[i,17] > 90 & data[i,17] <= 95)
    data[i,40] <- 3
  else
    data[i,40] <- 4
}
# Now we have categorized all rows by LTV into 4 categories provided in report 2

# The rest of the solution is similar to the Report 1 parts, we will dummify the new column 'b' and proceed in a similar manner

bdummy <- dummy(data$b)

data <- cbind(data, bdummy)

# This created 4 columns in the dataframe, b1, b2, b3 and b4. b1 corresponds to b = 1 for that row, b2 has a value 1 if b = 2 for that row and so on.

# Loan Counts:

lc1 <- sum(data[,41]) 
lc1

lc2 <- sum(data[,42])
lc2

lc3 <- sum(data[,43])
lc3

lc4 <- sum(data[,44])
lc4

lcsum <- lc1 + lc2 + lc3 + lc4
lcsum

# Average Current Balance

data$ltv1 <- data[,15]*data[,41]
altv1 <- sum(data$ltv1)/lc1
altv1

data$ltv2 <- data[,15]*data[,42]
altv2 <- sum(data$ltv2)/lc2
altv2

data$ltv3 <- data[,15]*data[,43]
altv3 <- sum(data$ltv3)/lc3
altv3

data$ltv4 <- data[,15]*data[,44]
altv4 <- sum(data$ltv4)/lc4
altv4

taltv <- (altv1*lc1 + altv2*lc2 + altv3*lc3 + altv4*lc4)/(lc1 + lc2 + lc3 + lc4)
taltv

# Total average validation

sum(data[,15])/2000

# Max Current Balance

max1 <- max(data$ltv1)
max1

max2 <- max(data$ltv2)
max2

max3 <- max(data$ltv3)
max3

max4 <- max(data$ltv4)
max4

tmax <- max(data[,15])
tmax
# This caltvulates the total maximum out of all rows

# Min Current Balance

min1 <- sort(data[, 45]) 
min1 <- min1[!!min1][1] # This removes the null values from sorted rows
min1

min2 <- sort(data[, 46]) 
min2 <- min2[!!min2][1]
min2

min3 <- sort(data[, 47]) 
min3 <- min3[!!min3][1]
min3

min4 <- sort(data[, 48]) 
min4 <- min4[!!min4][1]
min4

tmin <- min(min1,min2,min3,min4)
tmin
# This calculates the total minimum out of all rows

# REPORT 3

library(zoo)
data$loanage <- NA

for (i in 1:nrow(data)){
  data[i,49] <- (as.yearmon(strptime("1.06.2013", format = "%d.%m.%Y"))- as.yearmon(strptime(data[i,14], format = "%Y-%m-%d")))*12
}

# Hence we have calculated the months into a new column by the name of "loanage"

# We create a new column by the name of "LAC" which is a Loan Age Category abbreviation

data$LAC <- NA

for (i in 1:nrow(data)){
  if (is.na(data[i,49]))
    data[i,50] <- 1
  else if (data[i,49] >= 0 & data[i,49] < 9)
    data[i,50] <- 2
  else if (data[i,49] >= 10 & data[i,49] < 19)
    data[i,50] <- 3
  else if (data[i,49] >= 20 & data[i,49] < 29)
    data[i,50] <- 4
  else if (data[i,49] >=30 & data[i,49] < 39)
    data[i,50] <- 5
  else
    data[i,50] <- 6
}

# Again, we proceed with the dummy variables creation for all the 6 categories. (1 is Unknown, 2 is 0-9 Months and so on, in the same order as Template sheet)

loanagedum <- dummy(data$LAC)

data <- cbind(data, loanagedum)

# Loan Counts:

lc1 <- sum(data[,51]) 
lc1

lc2 <- sum(data[,52])
lc2

lc3 <- sum(data[,53])
lc3

lc4 <- sum(data[,54])
lc4

lc5 <- sum(data[,55])
lc5

lc6 <- sum(data[,56])
lc6

lcsum <- lc1 + lc2 + lc3 + lc4 + lc5 + lc6
lcsum

# Average Current Balance

data$ld1 <- data[,15]*data[,51]
ald1 <- sum(data$ld1)/lc1
ald1

data$ld2 <- data[,15]*data[,52]
ald2 <- sum(data$ld2)/lc2
ald2

data$ld3 <- data[,15]*data[,53]
ald3 <- sum(data$ld3)/lc3
ald3

data$ld4 <- data[,15]*data[,54]
ald4 <- sum(data$ld4)/lc4
ald4

data$ld5 <- data[,15]*data[,55]
ald5 <- sum(data$ld5)/lc5
ald5

data$ld6 <- data[,15]*data[,56]
ald6 <- sum(data$ld6)/lc6
ald6

tald <- (ald1*lc1 + ald2*lc2 + ald3*lc3 + ald4*lc4 + ald5*lc5 + ald6*lc6)/(lc1 + lc2 + lc3 + lc4 + lc5 + lc6)
tald
# Total average validation

sum(data[,15])/2000

# Max Current Balance

max1 <- max(data$ld1)
max1

max2 <- max(data$ld2)
max2

max3 <- max(data$ld3)
max3

max4 <- max(data$ld4)
max4

max5 <- max(data$ld5)
max5

max6 <- max(data$ld6)
max6

tmax <- max(data[,15])
tmax
# This calculates the total maximum out of all rows

# Min Current Balance

min1 <- sort(data[, 57]) 
min1 <- min1[!!min1][1] # This removes the null values from sorted rows
min1

min2 <- sort(data[, 58]) 
min2 <- min2[!!min2][1]
min2

min3 <- sort(data[, 59]) 
min3 <- min3[!!min3][1]
min3

min4 <- sort(data[, 60]) 
min4 <- min4[!!min4][1]
min4

min5 <- sort(data[, 61]) 
min5 <- min5[!!min5][1]
min5

min6 <- sort(data[, 62]) 
min6 <- min6[!!min6][1]
min6

tmin <- min(min1,min2,min3,min4,min5)
tmin
# This calculates the total minimum out of all rows

# REPORT 4

data$FICO <- NA

for (i in 1:nrow(data)){
  if (data[i,16] < 600)
    data[i,63] <- 1
  else if (data[i,16] >= 600 & data[i,16] < 700)
    data[i,63] <- 2
  else if (data[i,16] >= 700 & data[i,16] < 800)
    data[i,63] <- 3
  else
    data[i,63] <- 4
}


# Below I created a matrix (crosstab) to store all the sums for REPORT 4

crosstab <- matrix(0, nrow = 4, ncol = 4)
crosstab

for (k in 1:nrow(data)){
  i <- data[k,63]
  j <- data[k,40]
  crosstab[i,j] <- crosstab[i,j] + data[k,15]
}
    
crosstab

# REPORT 5

options(scipen=5) # Used this because R was using exponential scale on y axis due to large numbers
colnames(crosstab) <- c("<=85%",">85% - <=90%",">90% - <=95%",">95%")
colours <- c("red", "blue", "yellow", "green")
barplot(crosstab, ylim = range( 200000 , 150000000 ), main="Riskspan FICO/LTV", ylab = "Sum of Current UPB ($)", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)
legend("topleft", c("FICO <600","FICO 600-699","FICO 700-799","FICO >=800"), cex=1.3, bty="n", fill=colours)

