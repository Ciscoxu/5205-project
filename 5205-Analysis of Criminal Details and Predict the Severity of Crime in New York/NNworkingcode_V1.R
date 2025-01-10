data$CMPLNT_FR_DT <- as.Date(data$CMPLNT_FR_DT, "%m/%d/%Y")
data$CMPLNT_FR_TM <- as.POSIXct(data$CMPLNT_FR_TM, format="%H:%M:%S")
data$Month <- as.integer(format(data$CMPLNT_FR_DT, "%m"))
data$Hour <- as.integer(format(data$CMPLNT_FR_TM, "%H"))

categorical_columns <- c("OFNS_DESC", "PD_DESC", "CRM_ATPT_CPTD_CD", "BORO_NM", "PREM_TYP_DESC", "PATROL_BORO", "VIC_SEX")
data[categorical_columns] <- lapply(data[categorical_columns], as.factor)

data$LAW_CAT_CD <- as.factor(data$LAW_CAT_CD)

inputs <- c("Month", "Hour", categorical_columns)
data[inputs] <- lapply(data[inputs], function(x) as.numeric(as.factor(x)))

set.seed(123)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

model <- multinom(LAW_CAT_CD ~ ., data = train_data, maxit=200)


predictions <- predict(model, test_data)
accuracy <- sum(predictions == test_data$LAW_CAT_CD) / nrow(test_data)
print(paste("Accuracy:", accuracy))
