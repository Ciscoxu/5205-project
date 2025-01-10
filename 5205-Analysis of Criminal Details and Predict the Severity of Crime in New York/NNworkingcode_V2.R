library(dplyr)

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
