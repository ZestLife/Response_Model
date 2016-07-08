# # Loads some additional data (Race/City/...) ------------------------------

 source(paste(Path, "/R/Load_Other_Data.R", sep = ""))



 
data <- read.csv(paste(Path, "/Data/", "SNS response data.csv", sep = ""), na.strings=c(""))





###### Remove unnecessary columns

data <- select(data, -First.Name, -First.Name.2, -Message.Sent)


############### add area and suburb info based on postal code.

postal_code     <- data.frame(POSTALCODE = data$Postal.Code)
postal_code$ID  <- seq(from = 1, to = length(data$Postal.Code))
postal_data     <- merge.data.frame(postal_code, City_Post_Data, by.x = "POSTALCODE", by.y = "POSTALCODE", all.x = TRUE)
postal_data     <- arrange(postal_data, ID)

# # Cleans the Data
# 

# Getting location info from the postal code.

postal_data <- select(postal_data, -ID)

data <- select(data, -Postal.Address.1, -Postal.Address.2, -Postal.Address.3, -Postal.Address.4)

data <- cbind(data, postal_data)



##### Create a check data frame

### checks if all the postal codes are the same (that is the merge was fine) and then deletes the old postal code column
check             <- data.frame(data$ID)
check$POSTALCODE  <- 0
check$POSTALCODE[data$Postal.Code != data$POSTALCODE] <- 1
data <-  select(data, - Postal.Code)




####### Get age and gender from ID

ID      <- data$ID.Number
yy      <- substr(ID, start = 1, stop = 2)
mm      <- substr(ID, start = 3, stop = 4)
dd      <- substr(ID, start = 5, stop = 6)
gender  <- ifelse(substr(ID, start = 7, stop = 10) >= 5000, "M", "F")


data$GENDER <- gender
data$YY     <- paste("19",yy, sep = "")
data$MM     <- mm


data <- select(data, -ID.Number)

rm(ID, yy, mm, dd, gender)

####### Get race info

Surname <- data$Last.Name

Surname         <- data.frame(SURNAME = toupper(data$Last.Name))
Surname$ID      <- seq(from = 1, to = length(data$Last.Name))
race_data       <- merge.data.frame(Surname, Race_Data, by.x = "SURNAME", by.y = "SURNAME", all.x = TRUE)
race_data       <- arrange(race_data, ID)
race_data       <- select(race_data, -ID)

data <- cbind(data, race_data)
data <- select(data, -Last.Name)




############ phone number info.


# is the work number the same as the home number?

data$Work.Tel <- ifelse(is.na(data$Work.Tel), data$Home.Tel, data$Work.Tel)
work_home <- ifelse(data$Work.Tel == data$Home.Tel, 1, 0) 
data$SAMETEL <- work_home 

rm(work_home) 



######## number of dots at the end of email address

dots <- gsub(".*@", "", data$E.Mail.Address, perl = TRUE)
dots <- nchar(dots) - nchar(gsub("\\.","", dots))

data$DOTS <- dots

rm(dots)

##########  Clean response


data$YES  <- ifelse(data$Response.Group == "Yes", 1, 0)
data      <- select(data, -Response.Group, -Responses)


rm(race_data, postal_data, postal_code, City_Data, Race_Data)


######### Replace blanks with NA

