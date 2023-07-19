#importing the dataset
library(readxl)
df<- read.csv("C:\\Users\\kelvin\\Downloads\\cema_internship_task_2023.csv")
head(df)
#View dataset
View(df)

#Length & width of the dataset
dim(df)
 
#length of the dataset
length(df)
nrow(df)

#rows of the dataset
ncol(df)

#summary of the dataset
summary(df)

#Null values
table(is.na(df))
sum(is.na(df))

#View Period columns
df$period
df['period']

#Selection 
df %>% select(2:3)
df %>% select(c(2:length(df)))

#View Period & Country
df %>% select(c('period','county'))

#Replace null values with Zeros
df[is.na(df)] = 0
sum(is.na(df))

View(df)






