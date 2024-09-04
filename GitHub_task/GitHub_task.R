# Read data from tsv file
data_tsv <- read_delim("data/raw/data_tsv.tsv", delim = "\t", n_max = Inf)

# Read data from tsv file and write data to csv file
data_tsv2 <- read_tsv("data/raw/data_tsv.tsv", skip = 0, n_max = Inf)
write_excel_csv(data_tsv2, "data/raw/data_csv.csv")

# Read data from csv file with , separator and write data to csv file with ; separator
data_csv1 <- read_csv("data/raw/data_csv.csv")
write_csv2(data_csv1, "data/raw/data_csv2.csv")

# Check if data doesn't change after format change
isMatch <- all.equal(data_csv1, data_tsv2)
print(isMatch)

# Read data from csv file with ; separator
data_csv2 <- read_csv2("data/raw/data_csv2.csv")

# Read data from excel file
data_excel <- read_excel("data/raw/data_excel.xlsx", sheet = "data_csv2")

# Write data in rds file
write_rds(data_tsv2, "data/raw/data_rds.rds")

# Read data from rds file
data_rds <- read_rds("data/raw/data_rds.rds")

# Mean calculation
mean1 <- mean(c(1, -1, 5, -12, -12, 3, 8, -10, 0), trim = 0, na.rm = FALSE)
mean2 <- mean(c(-13, 19, -24, NA, 30, 64, -53, NA, 50, 31, -58, -34, -3, -34, 77), trim = 0, na.rm = TRUE)

# Median calculation
median1 <- median(c(1, 9, NA, 88, 2, NA, 42, NA, 4, 68, NA), na.rm = TRUE)
median2 <- median(c(-91, -33, 13, 34, 34, 75, -80, -35, -90, -72, 70, 67, -100, -94, -18), na.rm = FALSE)

# Min max calculation
arr1 <- c(48.11, 45.3, 58.42, 51.64, 62.07, 57.26, 49.69, 93.29, 81.18, 44.78, 55.1, 76.74, 58.08)
min <- min(arr1, na.rm = FALSE)
max <- max(arr1, na.rm = FALSE)

# Quantile calculation
quant1 <- quantile(c(80.94, 44.46, 46.33, 65.1, 66.42, 104.43, 53.15, 48.41, 12.88, 51.1, 43.03, 40.3, 33.71, 55.1, 22.17), probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type =7)
quant2 <- quantile(c(26.17, 97.73, 24.81, 53.62, 87.72, 45.19, 45.7, 69.63, 36.76, 7.17), probs = seq(0, 1, 0.5))

# Dispersion and SD calculation
arr2 <- c(76.22, 65, 19.69, 29.84, 37.18, 70.93, 64.78, 61.66, 49.03, 51.56)
var <- var(arr2, na.rm = FALSE)
sd <- sd(arr2, na.rm = FALSE)

# IQR calculation
arr3 <- c(63.92, 35.85, 26.9, 48.92, 43.1, 66.94, 47.06, 56.54, 29.1, 58.88)
iqr <- IQR(arr3, na.rm = FALSE, type = 7)

# Array length calculation
len <- length(arr1)
len1 <- sum(!is.na(arr3))

# Standard error of the mean calculation
SEM <- function(x){
  result = sd(x) / sqrt(sum(!is.na(x)))
  return(result)
}
x <- c(76.22, 65, 19.69, 29.84, 37.18, 70.93, 64.78, 61.66, 49.03, 51.56)
SEM(x)

# Convert categorical variables to factors
data_tsv$Группа <- as.factor(data_tsv$Группа)
data_tsv$Пол <- as.factor(data_tsv$Пол)
# Print summary of base statistics
sum <- summary(data_tsv)
print(sum)

# Print summary of base statistics for numeric data (second method). Psych packet
numeric_data <- read_rds("data/raw/numeric_data.rds")
describe(numeric_data, na.rm = TRUE, skew = FALSE, ranges = TRUE)

# Print summary of base statistics for categorical data (second method)
categorical_data <- read_rds("data/raw/factor_data.rds")

table(categorical_data$Группа, categorical_data$`Группа крови`)

# Print relative frequencies for categorical data
prop.table(table(categorical_data$Группа, categorical_data$`Группа крови`))

