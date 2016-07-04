# # Loads some additional data (Race/City/...) ------------------------------
 source(paste(Path, "/R/Load_Other_Data.R", sep = ""))



 
data <- read.csv(paste(Path, "/Data/", "SNS response data.csv", sep = ""))


# # Cleans the Data
# 

data$ID.Number <- gsub(" ","", toupper(data$ID.Number))

postadr <- data.frame(data$Postal.Address.1, data$Postal.Address.2, data$Postal.Address.3, data$Postal.Address.4, data$Postal.Code)

n <- max(sapply(postadr, length))
l <- lapply(postadr, function(X) c(X, rep(NA, n - length(X))))

postadr <- data.frame(t(do.call(cbind, l)))
colnames(postadr) <- paste("CLIENTPOSTALADDRESS", seq(1:ncol(postadr)), sep = "")

codepos <- ncol(postadr) - rowSums(is.na(postadr))
codepos[codepos == 0] <- 1
t <- cbind(1:nrow(postadr), codepos)
 
 
 codepos <- ncol(postadr) - rowSums(is.na(postadr))
 codepos[codepos == 0] <- 1
 t <- cbind(1:nrow(postadr), codepos)

 Pos_Code <- as.data.frame(as.numeric(postadr[t]))
 colnames(Pos_Code) <- "CLIENTPOSTALADDRESSPOSTALCODE"

 postadr2 <- data.frame(lapply(postadr, as.character), stringsAsFactors = FALSE)
 postadr2[is.na(postadr2)] <- 111
 postadr2 <- data.frame(lapply(postadr2, as.numeric), stringsAsFactors = FALSE)
 postadr2 <- ifelse(is.na(postadr2), TRUE, FALSE)
 postadr3 <- as.matrix(postadr)
 postadr  <- as.data.frame(ifelse(postadr2, postadr3, NA))

 data <- subset(data, select = -POSTALADDRESS)

 data <- cbind(data, postadr, Pos_Code)


