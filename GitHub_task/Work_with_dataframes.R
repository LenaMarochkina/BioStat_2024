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
  
# Select columns
data %>%
  select(`Группа.крови`, `Рост`)

data %>% 
  select(!`Рост` & !`Пол`)

data %>%
  select(where(is.numeric))

data %>%
  select(where(is.factor))

data %>% 
  select(`Группа`, (function(x) is.factor(x)) | where(function(x) is.numeric(x)))

# Select data by visit 
data %>%
  select(contains("_E1"))

data %>%
  select(matches("_E\\d{1}"))

data %>%
  select(starts_with("Б"))

# Select strictly
var <- c("Базофилы_E1", "Базофилы_E2", "Гемоглобин_E1")

data %>% 
  select(all_of(var))

# Select gentle

data %>%
  select(any_of(var))

# Conditional select - AND, OR, NEG
data %>%
  select(where(is.numeric)) %>%
  select(where(function(x) sd(x, na.rm = TRUE) > 2 & mean(x, na.rm = TRUE) < 10))

data %>%
  select(where(is.numeric)) %>%
  select(where(function(x) sd(x, na.rm = TRUE) > 2 | mean(x, na.rm = TRUE) < 10 | median(x, na.rm = TRUE) > 5))

data %>%
  select(!where(is.numeric))

# Select other columns 
data %>% 
  select(`Пол`, `Эритроциты_E1`, everything())

# Select and change names
data %>%
  select("Erythrocytes_V1" = `Эритроциты_E1`, "Erythrocytes_V2" = `Эритроциты_E2`)

# Select rows
data %>%
  slice(1:10)

data %>%
  slice_head(n = 10)

data %>%
  slice_tail(prop = 0.2)

data %>%
  slice_sample(prop = 0.1)

data %>%
  slice_min(`Возраст`)

# Data filtration
data %>%
  filter(`Пол` == "Женский" & `Группа.крови` %in% c("A (II)", "B (III)") & `Группа` != "Группа 1" )

data %>%
  filter(between(`Возраст`, 31, 34))

data %>%
  filter(near(`Эозинофилы_E1`, 3.38, tol = 0.1))

data %>%
  filter(if_all(.cols = contains("Базофилы"), .fns = function(x) x > 1.5))

data %>%
  filter(if_any(.cols = contains("Базофилы"), .fns = function(x) x > 1.5))
  
data %>%
  group_by(`Группа`) %>%
  filter(`Возраст` > 30)

# Data mutation
data %>%
  mutate(`Women_4th_blood_type` = ifelse(`Пол` == "Женский" & `Группа.крови` == "AB (IV)", "Y", "N")) %>%
  select(`Women_4th_blood_type`, everything()) %>%
  arrange(desc(`Women_4th_blood_type`))

# Arithmetic operations
tibble(var_1 = 1:10, var_2 = var_1 + 1.123) %>%
  mutate (var_sum = var_1 + var_2,
          var_minus = var_1 - var_2,
          var_multiple = var_1 * var_2,
          var_divide = var_1 / var_2,
          var_1_log = log(var_1),
          var_1_log1p = log1p(var_1),
          var_1_exp = exp(var_1_log),
          var_1_exm1 = expm1 (var_1_log1p),
          var_2_round = round (var_2, 2),
          var_2_ceil = ceiling (var_2),
          var_2_floor = floor (var_2)) %>%
  glimpse()

# Use a case-when option
data %>%
  mutate(`Age_group` = case_when(`Возраст` < 20 ~ "<20",
                                 between(`Возраст`, 20, 30) ~ "20-30",
                                 `Возраст` > 30 ~ ">30") %>% as.factor()) %>%
  select(`Возраст`, `Age_group`) %>%
  arrange(`Возраст`)

# Replace NA
data %>%
  mutate(`Группа.крови` =`Группа.крови` %>% as.character() %>% replace_na("No data") %>% as.factor())

data %>%
  mutate(`Группа.крови` =`Группа.крови` %>% na_if("AB (IV)"))

# Delete variable through mutate()
data %>% 
  mutate(`Группа.крови` = NULL)

# Across() function
data %>%
  mutate(across(where(is.numeric), function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)))

# Iteration through rows
data %>%
  rowwise() %>%
  mutate(`Basophils_Mean` = mean(c_across(contains("Базофилы")))) %>%
  ungroup() %>%
  select(`Basophils_Mean`, contains("Базофилы"))

data %>%
  group_by(`Группа`) %>%
  mutate(across(contains("Базофилы"), function(x) x - mean(x, na.rm = TRUE))) %>%
  ungroup() %>%
  select(`Группа`, contains("Базофилы"))

# Rename variables
data %>%
  rename(`Group` = `Группа`) %>%
  select(`Group`)

data %>%
  rename_with(function(x) x %>% stri_replace_all_regex(c("_E1", "_E2"), c("_V1", "_V2"), vectorize_all =  FALSE)) %>%
  glimpse()

new_name <- "Group"
data %>%
  rename(!! new_name := `Группа`) %>%
  glimpse()

# Sorting
data %>%
  arrange(`Пол`, desc(`Возраст`))

data %>%
  group_by(`Группа`) %>%
  arrange(`Возраст`, .by_group = TRUE)

# Pivoting data frames 
data %>%
  select(`Группа`, contains("_E1")) %>%
  pivot_longer(!`Группа`)

data %>%
  select(`Группа`, contains("_E1")) %>%
  mutate(ID = row_number()) %>%
  pivot_longer(!c(`Группа`, ID)) %>%
  pivot_wider(id_cols = ID)

# Unique values
data %>%
  distinct(`Группа`, .keep_all = TRUE)

# Separate variables
tibble(var_1 = rep(paste0("first part", "__", "second_part"), 10)) %>% 
  separate(var_1, into = c("var_1", "var_2"), sep = "__")

# Unite variables
tibble(var_1 = rep(paste0("first part", "__", "second_part"), 10)) %>% 
  separate(var_1, into = c("var_1", "var_2"), sep = "__") %>%
  unite("new_var", var_1, var_2, sep = " AND ")


