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
solicited_curated_metadata <- read.csv(synGet(solicited.meta.curated.id)$path)
# solicited_data <- read.csv(synGet(solicited.raw.curated.id)$path)
long_curated_metadata <- read.csv(synGet(long.meta.curated.id)$path)
# long1_data <- read.csv(synGet(long1.raw.curated.id)$path)
# long2_data <- read.csv(synGet(long2.raw.curated.id)$path)

mad_data <- read_excel(synGet(mad.raw.original.id)$path)
mad_solicited_metadata <- read_excel(synGet(mad.solicited.meta.original.id)$path)
mad_long_metadata <- read_excel(synGet(mad.long.meta.original.id)$path)

tanz_data <- read_excel(synGet(tanz.raw.original.id)$path)
tanz_solicited_metadata <- read_excel(synGet(tanz.solicited.meta.original.id)$path)
tanz_long_metadata <- read_excel(synGet(tanz.long.meta.original.id)$path)

r2d2_data <- bind_rows(read.csv(synGet(r2d2.test.raw.original.id)$path), read.csv(synGet(r2d2.train.raw.original.id)$path))
r2d2_solicited_metadata <- read.csv(synGet(r2d2.solicited.meta.original.id)$path)
r2d2_long_metadata <- read.csv(synGet(r2d2.long.meta.original.id)$path)


# Verification ------------------------------------------------------------

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

r2d2_train_data <- 
  r2d2_data %>% 
  filter(Type=="train") %>% 
  select(Type, StudyID)

r2d2_train_data$Type %<>% tolower()
r2d2_train_data$StudyID %<>% str_remove_all("-")

r2d2_test_data <- 
  r2d2_data %>% 
  filter(Type=="test") %>% 
  select(Type, StudyID)

r2d2_test_data$Type %<>% tolower()
r2d2_test_data$StudyID %<>% str_remove_all("-")

# Verify studyIDs correspond to cohorts in the study
id_map$StudyID %>% 
  {sub("R2D2.*", "R2D2", ., perl = T)} %>% 
  {sub("RPTanz.*", "RPTanz", ., perl = T)} %>% 
  {sub("RPMada.*", "RPMada", ., perl = T)} %>% 
  unique()

# Check if studyIDs in id_map are correctly labeled with "train" or "test" based on classification from original data
id_map$check <- rep("", times = nrow(id_map))

all_original_data_type_ID <- bind_rows(mad_train_data, mad_test_data, tanz_train_data, tanz_test_data, r2d2_train_data, r2d2_test_data)

for (i in 1:length(id_map$StudyID)) {
  if (id_map$StudyID[i] %in% all_original_data_type_ID$StudyID & id_map$type[i]==all_original_data_type_ID$Type[which(id_map$StudyID[i]==all_original_data_type_ID$StudyID)]) {
    id_map$check = TRUE
  }
}

F %in% id_map$check # Evaluates to FALSE, so the check is passed

# Check if curated data contains only participant ID's associated with "train" studyID's from id_map
identical(clinical_curated_data$participant, id_map$participant[which(id_map$type=="train")]) # Evaluates to TRUE, so the check is passed
F %in% (unique(solicited_curated_metadata$participant) %in% id_map$participant[which(id_map$type=="train")]) # Evaluates to FALSE, so check is passed
T %in% (unique(solicited_curated_metadata$participant) %in% id_map$participant[which(id_map$type=="test")]) # Evaluates to FALSE, so check is passed
F %in% (unique(long_curated_metadata$participant) %in% id_map$participant[which(id_map$type=="train")]) # Evaluates to FALSE, so check is passed
T %in% (unique(long_curated_metadata$participant) %in% id_map$participant[which(id_map$type=="test")]) # Evaluates to FALSE, so check is passed

# Verify that curated clinical data column values for each participant match original data column values for corresponding studyID in original data

# First, check that original data variable names are the same between cohorts
identical(colnames(mad_data), colnames(tanz_data)) # TRUE
identical(colnames(mad_data), colnames(r2d2_data)) # FALSE

length(colnames(mad_data)) # 27
length(colnames(r2d2_data)) # 25

colnames(mad_data)[which(!(colnames(mad_data) %in% colnames(r2d2_data)))] # "RedCapID", "HyfeID", "Nightsweat", "Smokeinlastweek"

r2d2_data %<>% 
  rename(Nightsweat = Nightsweats) %>% 
  rename(Smokeinlastweek = Smokedinlastweek)

colnames(mad_data)[which(!(colnames(mad_data) %in% colnames(r2d2_data)))] # "RedCapID", "HyfeID"; R2D2 cohort raw data does not have these variables, but metadata has HyfeID

# Combine all original data into one df
all_original_data <- bind_rows(mad_data, tanz_data, r2d2_data)
all_original_data$StudyID[which(all_original_data$Country=="Madagascar")] <- all_original_data$HyfeID[which(all_original_data$Country=="Madagascar")]

# Compare StudyID and HyfeID values in the original data
all_original_data$StudyID[which(!(all_original_data$StudyID[1:557] %in% all_original_data$HyfeID[1:557]))]
all_original_data$HyfeID[which(!(all_original_data$StudyID[1:557] %in% all_original_data$HyfeID[1:557]))]
# Better to use StudyID instead of HyfeID since HyfeID has errors while StudyID does not

# Keep identical columns present in both original and curated data, then rename column names to match across original and curated data
all_original_data_check <- 
  all_original_data %>% 
  select(-HyfeID) %>% 
  select(Type, StudyID, Sex, Age, Height, Weight, Durationofcough, PriorTB, 
         PriorTBtypePulmonary, PriorTBtypeExtrapulmonary, PriorTBtypeUnknown, 
         Hemoptysis, Heartrate, Temperature, Weightloss, Smokeinlastweek, 
         Fever, Nightsweat, Microbiologicreferencestandard)

all_original_data_check$Type %<>% tolower()
all_original_data_check$StudyID %<>% str_remove_all("-")
all_original_data_check$participant <- rep("", nrow(all_original_data_check))
all_original_data_check %<>% select(Type, StudyID, participant, everything())

for (i in 1:nrow(all_original_data_check)) {
  if (all_original_data_check$StudyID[i] %in% id_map$StudyID) {
    all_original_data_check$participant[i] <- id_map$participant[which(id_map$StudyID==all_original_data_check$StudyID[i])]
  }
  else all_original_data_check$participant[i] <- NA
}

all_original_data_check$Microbiologicreferencestandard %<>% 
  str_remove_all("TB ") %>% 
  str_replace_all(c("Negative" = "0", "Positive" = "1"))

all_original_data_check %<>% 
  drop_na(participant)

colnames(all_original_data_check)[3:ncol(all_original_data_check)] <- colnames(clinical_curated_data)

all_original_data_check[is.na(all_original_data_check)] <- ""
clinical_curated_data[is.na(clinical_curated_data)] <- ""

all_original_data_check$age = as.integer(all_original_data_check$age)
all_original_data_check$reported_cough_dur = as.integer(all_original_data_check$reported_cough_dur)
all_original_data_check$heart_rate = as.integer(all_original_data_check$heart_rate)
all_original_data_check$tb_status = as.integer(all_original_data_check$tb_status)

all_original_data_check$tb_prior_Pul %<>% str_trim("both")
all_original_data_check$hemoptysis %<>% str_trim("both")
all_original_data_check$smoke_lweek %<>% str_trim("both")
all_original_data_check$fever %<>% str_trim("both")
all_original_data_check$weight_loss %<>% str_trim("both")

clinical_curated_data$tb_prior_Pul %<>% str_trim("both")
clinical_curated_data$hemoptysis %<>% str_trim("both")
clinical_curated_data$smoke_lweek %<>% str_trim("both")
clinical_curated_data$fever %<>% str_trim("both")
clinical_curated_data$weight_loss %<>% str_trim("both")

all_original_data_check %<>% 
  str_trim("both") %>% 
  as_tibble()

clinical_curated_data %<>% 
  str_trim("both") %>% 
  as_tibble()

all_original_data_check %<>%
  filter(Type=="train") %>%
  select(-c(Type, StudyID)) %>%
  arrange(participant) #%>%
  # all_equal(arrange(clinical_curated_data))

clinical_curated_data %<>% arrange(participant)

check <- data.frame(check=rep("", times=nrow(all_original_data_check)))

for (i in 1:length(all_original_data_check)) {
  ifelse(all_original_data_check[i,]==clinical_curated_data[i,], check[i] <- TRUE, check[i] <- FALSE)
}

F %in% check # Evaluates to FALSE, so check is passed
