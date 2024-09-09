# Get data
# To read strings as factors must to use stringsAsFactors = TRUE 

data <- read.csv("data/raw/data_csv.csv", stringsAsFactors = TRUE)

# Learning pipe
data %>%
  select(., `Группа`, `Рост`, contains("E1")) %>%
  filter(., `Группа` == "Группа 1") %>%
  slice(., 1:10) %>%
  mutate(., `Рост` = `Рост` / 10)

# Variables access using tibble
data$Группа
data$`Группа крови`

# Create variables using tibble
tibble(var_first = 1:10, var_second = ifelse(var_first < 5, var_first + 100, var_first))

tibble(`var 1` = 1:10, `var 2` = `var 1` * 100)

# Data view in table format
data %>% View()

# Add column
data <- data %>%
  add_column(newHeight = data$`Рост` / 10, .after = Inf)

# Add row
data <- data %>%
  add_row(`Группа` = "New_group", `Возраст` = 50, .before = 1)

# Add numeration
data %>%
  mutate(ID = row_number())

# Binding columns
data_1 <- tibble(ID_num = 1:10, Group = rep(c("placebo", "active"), 5))
data_2 <- tibble(random = rbeta(10, 3, 5), norm = rnorm(10))
data_3 <- tibble(Age = 100:91, Drugs = rep(c("omeprazole", "aspirin"), 5))
data_1 %>% bind_cols(data_2) %>% bind_cols(data_3)

# Binding rows
data_1 <- tibble(ID_num = 1, group = "active", test_res = "positive")
data_2 <- tibble(ID_num = 2, group = "placebo", test_res = "positive")
data_3 <- tibble(ID_num = 3, group = "placebo", test_res = "negative")
data_4 <- tibble(ID_num = 4, group = "active", test_res = "positive")

data_1 %>% bind_rows(data_2) %>% bind_rows(data_3) %>% bind_rows(data_4)

# Join table
data_1 <- tibble(var_1 = 1:8) %>% mutate(id = row_number())

data_2 <- tibble(var_2 = rnorm(10)) %>% mutate(`Subject ID` = row_number())

data_1 %>%
  left_join(data_2, by = c("id" = "Subject ID"))

data_1 %>%
  right_join(data_2, by = c("id" = "Subject ID"))

data_1 %>%
  inner_join(data_2, by = c("id" = "Subject ID"))

data_1 %>%
  full_join(data_2, by = c("id" = "Subject ID"))

# Group data

data %>% group_by("Группа")

# Divide data frame by values
data %>% split("Группа")

# Group by row
data %>% 
  rowwise() %>%
  mutate(`Average_hemoglobin` = mean(c(`Гемоглобин_E1`, `Гемоглобин_E2`))) %>%
  ungroup() %>%
  select(`Гемоглобин_E1`, `Гемоглобин_E2`, `Average_hemoglobin`)
  






