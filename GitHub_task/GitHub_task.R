# Read data from tsv file
data_tsv <- read_delim("data/raw/data_tsv.tsv", delim = NULL, n_max = Inf)

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