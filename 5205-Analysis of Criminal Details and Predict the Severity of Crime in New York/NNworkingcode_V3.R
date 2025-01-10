data$CMPLNT_FR_DT <- as.Date(data$CMPLNT_FR_DT, format="%m/%d/%Y")
data$Year <- as.integer(format(data$CMPLNT_FR_DT, "%Y"))
data = data %>% filter(Year>2016)
data$CMPLNT_FR_DT <- as.Date(data$CMPLNT_FR_DT, format="%m/%d/%Y")
data$CMPLNT_FR_TM <- as.POSIXct(data$CMPLNT_FR_TM, format="%H:%M:%S")

data$Month <- as.integer(format(data$CMPLNT_FR_DT, "%m"))
data$Hour <- as.integer(format(data$CMPLNT_FR_TM, "%H"))

data <- mutate(data,
               Jan = ifelse(Month == 1, 1, 0),
               Feb = ifelse(Month == 2, 1, 0),
               Mar = ifelse(Month == 3, 1, 0),
               Apr = ifelse(Month == 4, 1, 0),
               May = ifelse(Month == 5, 1, 0),
               Jun = ifelse(Month == 6, 1, 0),
               Jul = ifelse(Month == 7, 1, 0),
               Aug = ifelse(Month == 8, 1, 0),
               Sep = ifelse(Month == 9, 1, 0),
               Oct = ifelse(Month == 10, 1, 0),
               Nov = ifelse(Month == 11, 1, 0),
               Dec = ifelse(Month == 12, 1, 0))

data <- mutate(data,
               Morning = ifelse(Hour >= 6 & Hour < 12, 1, 0),
               Afternoon = ifelse(Hour >= 12 & Hour < 18, 1, 0),
               Evening = ifelse(Hour >= 18 & Hour < 24, 1, 0),
               Night = ifelse(Hour >= 0 & Hour < 6, 1, 0))

categorical_vars <- c("OFNS_DESC", "PD_DESC", "BORO_NM")
data[categorical_vars] <- lapply(data[categorical_vars], function(x) as.numeric(as.factor(x)))

data$LAW_CAT_CD <- as.factor(data$LAW_CAT_CD)

inputs <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
            "Morning", "Afternoon", "Evening", "Night", "OFNS_DESC", "PD_DESC", "BORO_NM")

set.seed(123)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

model <- multinom(LAW_CAT_CD ~ ., data = train_data[, c(inputs, "LAW_CAT_CD")], maxit=200)

predictions <- predict(model, test_data[, inputs])

accuracy <- sum(predictions == test_data$LAW_CAT_CD, na.rm = TRUE) / nrow(test_data)
print(paste("Accuracy:", accuracy))
