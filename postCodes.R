#load the CSV file with the data
#note that is in the data folder/, in the workspace.
postCodes <- read.csv("data/NIPostcodes.csv", header=FALSE)

#a) Show the total number of rows, the structure of the data frame, and first 10
#   rows of the data frame containing all of the NIPostcode data.
nrow(postCodes)
sprintf("Number of rows %s in NI post codes data frame", nrow(postCodes))
str(postCodes, vec.len = 1, digits.d = 0)
#show 10 first row of NI Post Codes dataframe
head(postCodes,10)


#b) Add a suitable title for each attribute of the data.
colnames(postCodes)
colnames(postCodes)[1] <- "Organisation name"
colnames(postCodes)[2] <- "Sub-building Name"
colnames(postCodes)[3] <- "Building Name"
colnames(postCodes)[4] <- "Number"
colnames(postCodes)[5] <- "Primary Thorfare"
colnames(postCodes)[6] <- "Alt Thorfare"
colnames(postCodes)[7] <- "Secondary Thorfare"
colnames(postCodes)[8] <- "Locality"
colnames(postCodes)[9] <- "Townland"
colnames(postCodes)[10] <- "Town"
colnames(postCodes)[11] <- "County"
colnames(postCodes)[12] <- "Postcode"
colnames(postCodes)[13] <- "x-coordinates"
colnames(postCodes)[14] <- "y-coordinates"
colnames(postCodes)[15] <- "Primary Key"
colnames(postCodes)


#c) Replace and recode all missing entries with a suitable identifier. 
#   Decide whether it is best to remove none, some or all of the missing data. Discuss the
#   missing data using suitable graphical output to explain your answer and justify
#   your decision in detail.

#change the columns with the feature type to character, 
#for after replace the empty values by NA
for (i in colnames(postCodes)){
  if ( is.factor(postCodes[[i]]) ){
    postCodes[[i]] <- as.character(postCodes[[i]])
  }
  print(class(postCodes[[i]]))
  #print(postCodes[[i]])
}

#check the structure of the dataframe result of the before operation
str(postCodes)
class(postCodes)

#replaces all the values in a character column type to NA
postCodes[postCodes == ""] <- NA
#postCodes$Locality[complete.cases(postCodes)] <- NA

#change back the columns with the character type to factor, 
#so can be done summary of these columns and get extra info
for (i in colnames(postCodes)){
  if ( is.character(postCodes[[i]]) ){
    postCodes[[i]] <- as.factor(postCodes[[i]])
  }
  print(class(postCodes[[i]]))
}
summary(postCodes)

if ( "mice" %in% rownames(installed.packages()) == FALSE){
  install.packages("mice")
}
library(mice)
md.pattern(postCodes[1:12],plot = TRUE, rotate.names = FALSE)


#e) Move the primary key identifier to the start of the dataset.
#reorder columns position with "primary key" at first position.
postCodes[,c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]

#f) Create a new dataset called Limavady_data. 
#   Store within it only information where locality, 
#   townland and town contain the name Limavady. 
#   Count and display the number of rows. 
#   Store this information in a csv file called Limavady. 

#check the column type
str(postCodes, vec.len = 1, digits.d = 0)

#subset Limavady data
limavady <- postCodes[ which(postCodes$Locality == "LIMAVADY" | postCodes$Townland == "LIMAVADY" | postCodes$Town == "LIMAVADY"),]

#show the numbers of rows in the new subset
sprintf("Number of rows %s in limavady data frame", nrow(limavady))

#store the dataframe in a file
write.csv(limavady,'Limavady.csv')

#g) Save the modified NIPostcode dataset in a csv file called CleanNIPostcodeData.
write.csv(postCodes,'CleanNIPostcodeData.csv')

