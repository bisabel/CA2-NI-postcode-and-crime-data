#(a) Using R, amalgamate all of the crime data from each csv file into one dataset.
#    Save this dataset into a csv file called AllNICrimeData. 
#    Count and show the number of rows in the AllNICrimeData dataset. 
#    Do not hard code the dataset path into your R code.

#load the name files in a list
files <- list.files(path ="data", 
           pattern="*northern-ireland-street.csv", 
           all.files=FALSE,
           full.names=TRUE, 
           recursive = TRUE)

#numbers of file to load
length(files)
crimeData = data.frame()
for ( file_name in files){
  print(file_name)
  #aux_dataframe <- read.csv(file_name)
  crimeData <- rbind(crimeData, read.csv(file_name))
  print(nrow(crimeData))
}

#store the result in a file
write.csv(crimeData,"AllNICrimeData.csv")
nrow(crimeData)
class(crimeData)
sprintf("Number of rows %s read in crime files", nrow(crimeData))

#(b) Modify the structure of the newly created AllNICrimeData csv file 
#    and remove the following attributes: 
#    CrimeID, Reported by, Falls within, LSOA code, LSOA name,
#    last outcome and context. Save this new structure and show the structure of the
#    modified file

#remove columns
crimeData$Crime.ID <- NULL
crimeData$Reported.by <- NULL
crimeData$Falls.within <- NULL
crimeData$LSOA.code <- NULL
crimeData$LSOA.name <- NULL
crimeData$Last.outcome.category <- NULL
crimeData$Context <- NULL

#store the data frame
write.csv(crimeData,"AllNICrimeData.csv")
str(crimeData, vec.len = 1, digits.d = 0)
sprintf("Number of cols %s after modify structure of crime file", ncol(crimeData))
sprintf("Number of rows %s after modify structure of crime file", nrow(crimeData))

#(c) In the AllNICrimeData csv file, shorten each crime type as follows:
crimeData$Crime.type <- as.character(crimeData$Crime.type)
crimeData$Crime.type[crimeData$Crime.type == "Anti-social behaviour"] <- "ASBO"
crimeData$Crime.type[crimeData$Crime.type == "Bicycle theft"] <- "BITH" 
crimeData$Crime.type[crimeData$Crime.type == "Burglary"] <- "BURG"
crimeData$Crime.type[crimeData$Crime.type == "Criminal damage and arson"] <- "CDAR"
crimeData$Crime.type[crimeData$Crime.type == "Drugs"] <- "DRUG"
crimeData$Crime.type[crimeData$Crime.type == "Other theft"] <- "OTTH"
crimeData$Crime.type[crimeData$Crime.type == "Public order"] <- "PUBO"
crimeData$Crime.type[crimeData$Crime.type == "Robbery"] <- "ROBY"
crimeData$Crime.type[crimeData$Crime.type == "Shoplifting"] <- "SHOP"
crimeData$Crime.type[crimeData$Crime.type == "Theft from the person"] <- "THPR"
crimeData$Crime.type[crimeData$Crime.type == "Vehicle crime"] <- "VECR"
crimeData$Crime.type[crimeData$Crime.type == "Violence and sexual offences"] <- "VISO"
crimeData$Crime.type[crimeData$Crime.type == "Other crime"] <- "OTCR"
#The value "Possession of weapons" dont specified in the statement, but I reasign
crimeData$Crime.type[crimeData$Crime.type == "Possession of weapons"] <- "POS"

crimeData$Crime.type <- as.factor(crimeData$Crime.type)
summary((crimeData$Crime.type))


#(d) Using the plot() function

#install required libraries if not done before
if ( "ggplot2" %in% rownames(installed.packages()) == FALSE){
  install.packages("ggplot2")
}
if ( "viridis" %in% rownames(installed.packages()) == FALSE){
  install.packages("viridis")
}
library(ggplot2)
library(viridis)

#get ready the data for the plot function
thief_type_frecuency <- ggplot(crimeData, aes(Crime.type)) +
                        geom_bar(fill = "#0073C2FF") +
                        scale_color_viridis()

thief_type_frecuency <- thief_type_frecuency +
                        ggtitle("Plot count crime by type") +
                        xlab("Crime type") + ylab("Count")

print(thief_type_frecuency)



#e) Modify the AllNICrimeData dataset so that the Location attribute 
#   contains only a street name.
summary(crimeData$Location)
head(crimeData$Location, 10)
crimeData$Location <- gsub("On or near ", "", crimeData$Location)
crimeData$Location[crimeData$Location == ""] <- NA
head(crimeData$Location, 10)



#f) Choose 5000 random samples of crime data from the AllNICrimeData dataset
#   where the location attribute contains location information. 
set.seed(100)
random_crime_sample <-  crimeData[sample(nrow(crimeData[complete.cases(crimeData$Location),]), 5000, replace = TRUE),]

#check the info of the randodom_crime_sample data frame
nrow(random_crime_sample)
str(random_crime_sample)

#prefunction
CleanNIPostcodeData <- read.csv("CleanNIPostcodeData.csv", header=TRUE)
uniquesLocations <- unique(CleanNIPostcodeData[,c(6,11)])

#function find_a_town
# params: address: string with the location to search
find_a_town <- function(address) {
  if ( is.na(address)){
    return(NA)
  } else {
    found <- uniques[which(uniquesLocations$Primary.Thorfare == toupper(address)),]
    if ( nrow(found) == 0) {
      return(NA)
    } else if ( nrow(found) == 1) {
      return(as.character(found$Town[]))
    } else {
      return("Other Town")
    }
  }
}

#some example data for testing the function
find_a_town("Jamaica road")
find_a_town("beneton road")     
find_a_town("belfast road") 
find_a_town("Aghserton Drive")
find_a_town("Galgorm Street")
find_a_town("Ballydown Meadows")

#creating the new column "Town" for the random data frame
#using the function find_a_town
random_crime_sample$Town <- do.call(rbind, lapply(random_crime_sample[,4], find_a_town))


#(g) Create a function called add_town_data that examines the information from
#    each crime record in random_crime_sample and matches each record with
#    relevant data in the VillageList.csv file. Add the population attribute to the
#    random_crime_sample data frame.

#load the file with the population data in a data frame
villages <- read.csv("data/VillageList.csv", header=TRUE)
villages$CITY.TOWN.VILLAGE <- toupper(villages$CITY.TOWN.VILLAGE)
#show some info about the villages data
nrow(villages)
unique(villages$CITY.TOWN.VILLAGE)
str(villages)

#function add_town_data
# params: town: string with the town to search the population
add_town_data <- function(town) {
  if ( is.na(town)){
    return(NA)
  } else {
    found <- villages[which(villages$CITY.TOWN.VILLAGE == toupper(town)),]
    print(town)
    if ( nrow(found) == 0) {
      return(NA)
    } else if ( nrow(found) == 1) {
      return(as.integer(found$POPULATION[]))
    } else {
      return(NA)
    }
  }
}

#some example data for testing the function
add_town_data(NA)
add_town_data("BELFAST")
add_town_data("Claudy")
add_town_data("London")
add_town_data("Coagh")

#creating the new column "Town" for the random data frame
#using the function find_a_town
random_crime_sample$Population <- do.call(rbind, lapply(random_crime_sample[,6], add_town_data))
colnames(random_crime_sample)[5] <- "Crime type"
colnames(random_crime_sample)[6] <- "City-Town-Village"
str(random_crime_sample)
write.csv(random_crime_sample,"random_crime_sample.csv")

#(i) Now we want to display crime for both cities in Northern Ireland which are Belfast
#    and Derry. From the random_crime_sample data frame, sort the chart data by crime
#    type for each city and then show both charts side-by-side by setting relevant
#    graphical parameters using the par() command

belfast <- random_crime_sample[random_crime_sample$`City-Town-Village` == "BELFAST",]
derry <- random_crime_sample[random_crime_sample$`City-Town-Village` == "LONDONDERRY",]

if ( "ggplot2" %in% rownames(installed.packages()) == FALSE){
  install.packages("ggplot2")
}
if ( "gridExtra" %in% rownames(installed.packages()) == FALSE){
  install.packages("gridExtra")
}
library(ggplot2)
library(gridExtra)

ggplot_belfast <- ggplot(belfast, aes(`Crime type`)) +
  geom_bar(fill = "#0eca4aFF") +
  ggtitle("Crime in Belfast by cryme type") 

ggplot_derry <- ggplot(derry, aes(`Crime type`)) +
  geom_bar(fill = "#0eca4aFF") +
  ggtitle("Crime in Derry by cryme type") 

grid.arrange(ggplot_belfast, ggplot_derry,ncol = 2, nrow = 1)


