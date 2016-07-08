### Fitting a GLM for the response model for assetlife
### Jadon
### 2016-07-08

# define train and test set
train_percent <-1
train <- data[1:round(train_percent*50967),]
test <- data[round(train_percent*50967):50967,]

# get possible factors from column in data
names(data)


# choose factors to include
variables <-  c("GENDER",
                "AGECATAGORY",
                "RACE",
                "SAMETEL",
                "DOTS",
                "YES")

train <- train[variables]
test  <- test[variables]

# fit glm() with factor YES as the output variable

model <- glm(YES ~ ., family = binomial(link = 'logit'),data=train)


summary(model)


anova(model, test="Chisq")
