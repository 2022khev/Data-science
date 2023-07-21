#loading library packages
library(ggplot2)
library(dplyr)

#importing the csv/excel dataset
library(readxl)
df<- read.csv("C:\\Users\\kelvin\\Downloads\\cema_internship_task_2023.csv")
#First five rows
head(df)

#View dataset nicely
View(df)

#Length & width of the dataset
dim(df)
 
#length of the dataset
length(df)
nrow(df)

#rows of the dataset
ncol(df)

#number of columns
colnames(df)

#datatype of data structure
class(df)

#Viewing uniques counties columns
table(unique(df$county))


#summary of the dataset
summary(df)

#identifying the number of the Null values
table(is.na(df))
sum(is.na(df))

#View Period/Date columns
df$period
df['period']

#Selecting First two columns  
df %>% select(2:3)

#View  columns (Period & County)

df %>% select(c('period','county'))

#Cleaning the dataset(Mutation)
#Replace null values with Zeros
df[is.na(df)] = 0

#Confirming there is no null values
sum(is.na(df))


#Checking wether  the period/Date is as.Date type
class(df$period)

#Checking the names of the columns
colnames(df)

#loading library dplyr
library(dplyr)
                                 #ANALYSIS
#Finding the county with the high number of dewormed and with Acute Malnutrion using GROUP BY FUNCTION

df_grp_region = df %>% group_by(county)  %>%
  summarise(total_dewormed = sum(Total.Dewormed),
            total_acute_malnutrion = sum(Acute.Malnutrition),
            .groups = 'drop')
df_grp_region


            #Visualizing the above GROUP BY FUNCTION
ggplot(df_grp_region,aes(x=county, y= total_dewormed)) + 
  geom_bar(stat = "identity") +
  coord_flip()

 #Finding the Period with the High Malnutrion & Number of Dewormed

df_grp_region = df %>% group_by(period) %>%
  summarise(total_dewormed = sum(Total.Dewormed),
            total_acute_malnutrion = sum(Acute.Malnutrition),
            .groups = 'drop')

df_grp_region

               #Visualizing the above analysis
ggplot(df_grp_region,aes(x=period, y= total_dewormed)) + 
  geom_bar(stat = "identity") +   coord_flip()
year

               #Data Transformation
     #Extracting year from period column and replacing with the full year digit

library(tidyr)
new = separate(df, col=period, into=c('months', 'years'), sep='-')
colnames(new)

class(new$year)
new$years <- replace(new$years, new$years=="21", 2021)
new$years <- replace(new$years, new$years=="22", 2022)
new$years <- replace(new$years, new$years=="23", 2023)
new$years

#Checking wether it is a character
class(new$years)

#Comparing the year where many were dewormed

df_year_dewormed = new %>% group_by(years) %>%
  summarise(total_dewormed = sum(Total.Dewormed),
            total_acute_malnutrion = sum(Acute.Malnutrition),
            .groups = 'drop')

df_year_dewormed

              #Visualizing with the pie Chart
#install.packages("plotrix")
library(plotrix)

# Get the library.
library(plotrix)
 
g <- df_year_dewormed$total_dewormed
labels <- df_year_dewormed$years
piepercent<- paste( round(100 * geeks/sum(df_year_dewormed$total_dewormed), 1),"%",sep="")
piepercent

 
                                 #Plot the chart.
pie3D(g, labels = piepercent,main = "Comparing Population of Dewormed From 2021 To 2023", col = rainbow(length(g)))
legend("topright", c("2021", "2022", "2023"),cex = 0.5, fill = rainbow(length(g)))


#Which county had many cases of the diarrhoes
new[new$diarrhoea.cases == max(new$diarrhoea.cases),]["county"]

#Which county had very minimum cases of the diarrhoes
new[new$diarrhoea.cases == min(new$diarrhoea.cases),]["county"]
 
#which and How many countries had more than average cases of diarrhoes

table(new[new$diarrhoea.cases >= mean(new$diarrhoea.cases),]["county"])

#Which month has high cases of diarrhoes in the year 2023

new[new$years=='2023' & new$diarrhoea.cases == max(new$diarrhoea.cases),]["months"]
new$diarrhoea.cases


colnames(new)
               #Compare the Underweight groups
underweight<- new %>% select(c("years","Underweight.0..6.months","Underweight.6.23.months","Underweight.24.59.Months"))
head(underweight)
                #Unpivot the data
#install.packages("reshape")
                #wide to long format
library(reshape)
piv.underwe<- melt(underweight,id = c("years"))
View(piv.underwe)


library(dplyr)
                           #Group By 
df_underweight_years =piv.underwe %>% group_by(variable)  %>%
  summarise(Number = sum(value))
df_underweight_years                           


colnames(piv.underwe) <- c("years", "Underweight_Status","Population")
colnames(piv.underwe)

#install.packages("tidyverse")
library(tidyverse)

max_underweight <- piv.underwe %>% summarise("Max of Underweight"=mean(Population))
max_underweight

mean_underweight <- piv.underwe %>% summarise("Mean of Underweight"=maxPopulation))
mean_underweight   

#Total number of underweight over three years period
table(piv.underwe$Underweight_Status)
                     #Underweight.24.59.Months
sum_2021 <- sum(piv.underwe[new$years=='2021' & piv.underwe$Underweight_Status =="Underweight.24.59.Months",]["Population"])
sum_2021
sum_2022 <- sum(piv.underwe[new$years=='2022' & piv.underwe$Underweight_Status =="Underweight.24.59.Months",]["Population"])
sum_2022
sum_2023 <- sum(piv.underwe[new$years=='2023' & piv.underwe$Underweight_Status =="Underweight.24.59.Months",]["Population"])
sum_2023
                    #Underweight.6.23.months"
sum1_2021 <- sum(piv.underwe[new$years=='2021' & piv.underwe$Underweight_Status ==" Underweight.6.23.months",]["Population"])
sum1_2021
sum1_2022 <- sum(piv.underwe[new$years=='2022' & piv.underwe$Underweight_Status ==" Underweight.6.23.months",]["Population"])
sum1_2022
sum1_2023 <- sum(piv.underwe[new$years=='2023' & piv.underwe$Underweight_Status ==" Underweight.6.23.months",]["Population"])
sum1_2023
                  #Underweight.0..6.months
sum2_2021 <- sum(piv.underwe[new$years=='2021' & piv.underwe$Underweight_Status =="  Underweight.0..6.months",]["Population"])
sum2_2021
sum2_2022 <- sum(piv.underwe[new$years=='2022' & piv.underwe$Underweight_Status =="  Underweight.0..6.months",]["Population"])
sum2_2022
sum2_2023 <- sum(piv.underwe[new$years=='2023' & piv.underwe$Underweight_Status =="  Underweight.0..6.months",]["Population"])
sum2_2023 

colors = c("green", "orange", "brown")
under_weight <- c("Underweight.24.59.Months", "Underweight.6.23.months", "Underweight.0..6.months")
years <- c("2021", "2022", "2023")
  
# Create the matrix of the values.
Population<- matrix(c(sum_2021,sum_2022,sum_2023,sum1_2021,sum1_2022,sum1_2023,sum2_2021,sum2_2022,sum2_2023),nrow = 3, ncol = 3, byrow = TRUE)
Population

rownames(Population) <- c("2021", "2022","2023") 
colnames(Population) <- c("Underweight.24.59.Months", "Underweight.6.23.months","Underweight.0..6.months")
Population
                        #Compare The Yearly Underweight Groups
barplot(Population, main = "Yearly Comparison of Yearly Underweight", names.arg = years, 
                          xlab = "years", ylab = "under_weight", 
                          col = colors, beside = TRUE)
  
# Add the legend to the chart
legend("topleft",under_weight , cex = 0.7, fill = colors)

barplot(Population)
barplot(Population, main = "Yearly Comparison of Yearly Underweight", names.arg = under_weight, 
                          xlab = "years", ylab = "under_weight", 
                          col = colors, beside = TRUE)
l





      