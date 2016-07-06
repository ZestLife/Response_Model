# # Loads some additional data (Race/City/...) ------------------------------

 source(paste(Path, "/R/Load_Other_Data.R", sep = ""))



 
data <- read.csv(paste(Path, "/Data/", "SNS response data.csv", sep = ""))


postal_code <- cbind(POSTALCODE = data$Postal.Code)
postal_code$ID <- seq(from = 1, to = length(data$Postal.Code))
postal_data <- merge.data.frame(postal_code, City_Post_Data, by.x = "data.Postal.Code", by.y = "POSTALCODE", all.x = TRUE, sort = FALSE)
postal_data <- arrange(postal_data, ID)

# # Cleans the Data
# 

# Getting location info from the postal code.

postal_data <- select(postal_data, -ID)


data <- select(data, -Postal.Address.1, -Postal.Address.2, -Postal.Address.3, -Postal.Address.4)

data <- cbind(data, postal_data)







