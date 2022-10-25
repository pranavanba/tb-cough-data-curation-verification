library(install.load)
install_load("synapser", "dplyr", "tidyr", "readxl", "magrittr", "stringr")

synLogin()

# Curated data ------------------------------------------------------------
participant.id.mapping.id <- "syn41600144" # csv file

clinical.curated.id <- "syn41604915" # csv file
solicited.meta.curated.id <- "syn41604939" # csv file
long.meta.curated.id <- "syn41604935" # csv file
solicited.raw.curated.id <- "syn40358494" # Folder of wav files
long1.raw.curated.id <- "syn40390832" # Folder of wav files
long2.raw.curated.id <- "syn40390840" # Folder of wav files


# Original data -----------------------------------------------------------
mad.raw.original.id <- "syn34634870" # xlsx file
tanz.raw.original.id <- "syn35839676" # xlsx file
r2d2.test.raw.original.id <- "syn32792189" # csv file
r2d2.train.raw.original.id <- "syn32792191" # csv file

mad.solicited.meta.original.id <- "syn34634889" # xlsx file
tanz.solicited.meta.original.id <- "syn35839678" # xlsx file
r2d2.solicited.meta.original.id <- "syn32792196" # csv file

mad.long.meta.original.id <- "syn34646061" # xlsx file
tanz.long.meta.original.id <- "syn35839677" # xlsx file
r2d2.long.meta.original.id <- "syn41831765" # csv file


# Get data ----------------------------------------------------------------
id_map <- read.csv(synGet(participant.id.mapping.id)$path)

clinical_curated_data <- read.csv(synGet(clinical.curated.id)$path)
solicited_metadata <- read.csv(synGet(solicited.meta.curated.id)$path)
# solicited_data <- read.csv(synGet(solicited.raw.curated.id)$path)
long_metadata <- read.csv(synGet(long.meta.curated.id)$path)
# long1_data <- read.csv(synGet(long1.raw.curated.id)$path)
# long2_data <- read.csv(synGet(long2.raw.curated.id)$path)

mad_data <- read_excel(synGet(mad.raw.original.id)$path)
mad_solicited_metadata <- read_excel(synGet(mad.solicited.meta.original.id)$path)
mad_long_metadata <- read_excel(synGet(mad.long.meta.original.id)$path)

tanz_data <- read_excel(synGet(tanz.raw.original.id)$path)
tanz_solicited_metadata <- read_excel(synGet(tanz.solicited.meta.original.id)$path)
tanz_long_metadata <- read_excel(synGet(tanz.long.meta.original.id)$path)

r2d2_test_data <- read.csv(synGet(r2d2.test.raw.original.id)$path)
r2d2_train_data <- read.csv(synGet(r2d2.train.raw.original.id)$path)
r2d2_solicited_metadata <- read.csv(synGet(r2d2.solicited.meta.original.id)$path)
r2d2_long_metadata <- read.csv(synGet(r2d2.long.meta.original.id)$path)


# Verification ------------------------------------------------------------
# r2d2_data <- bind_rows(r2d2_train_data, r2d2_test_data)

# Split raw data by train and test classification
mad_train_data <- 
  mad_data %>% 
  filter(Type=="Train") %>% 
  select(Type, HyfeID) %>% 
  rename(StudyID = HyfeID)

mad_train_data$Type %<>% tolower()
mad_train_data$StudyID %<>% str_remove_all("-")

mad_test_data <- 
  mad_data %>% 
  filter(Type=="Test") %>% 
  select(Type, HyfeID) %>% 
  rename(StudyID = HyfeID)

mad_test_data$Type %<>% tolower()
mad_test_data$StudyID %<>% str_remove_all("-")

tanz_train_data <- 
  tanz_data %>% 
  filter(Type=="Train") %>% 
  select(Type, StudyID)

tanz_train_data$Type %<>% tolower()
tanz_train_data$StudyID %<>% str_remove_all("-")

tanz_test_data <- 
  tanz_data %>% 
  filter(Type=="Test") %>% 
  select(Type, StudyID)

tanz_test_data$Type %<>% tolower()
tanz_test_data$StudyID %<>% str_remove_all("-")

r2d2_train_data %<>% 
  select(Type, StudyID)

r2d2_train_data$Type %<>% tolower()
r2d2_train_data$StudyID %<>% str_remove_all("-")

r2d2_test_data %<>% 
  select(Type, StudyID)

r2d2_test_data$Type %<>% tolower()
r2d2_test_data$StudyID %<>% str_remove_all("-")

# Verify studyIDs correspond to cohorts in the study
id_map$StudyID %>% 
  {sub("R2D2.*", "R2D2", ., perl = T)} %>% 
  {sub("RPTanz.*", "RPTanz", ., perl = T)} %>% 
  {sub("RPMada.*", "RPMada", ., perl = T)} %>% 
  unique()

# Check if studyID in id_map is correctly labeled with "train" or "test" based on classification from original data
id_map$check <- rep("", times = nrow(id_map))

all_original_data <- bind_rows(mad_train_data, mad_test_data, tanz_train_data, tanz_test_data, r2d2_train_data, r2d2_test_data)

for (i in 1:length(id_map$StudyID)) {
  if (id_map$StudyID[i] %in% all_original_data$StudyID & id_map$type[i]==all_original_data$Type[which(id_map$StudyID[i]==all_original_data$StudyID)]) {
    id_map$check = TRUE
  }
}


