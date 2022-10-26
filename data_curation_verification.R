library(install.load)
install_load("synapser", "dplyr", "tidyr", "readxl", "magrittr", "stringr", "tibble")

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
long_curated_metadata <- read.csv(synGet(long.meta.curated.id)$path)
# solicited_curated_files <- synGet(solicited.raw.curated.id)$path
# long1_curated_files <- synGet(long1.raw.curated.id)$path
# long2_curated_files <- synGet(long2.raw.curated.id)$path

mad_data <- read_excel(synGet(mad.raw.original.id)$path, trim_ws = T)
mad_solicited_metadata <- read_excel(synGet(mad.solicited.meta.original.id)$path, trim_ws = T)
mad_long_metadata <- read_excel(synGet(mad.long.meta.original.id)$path, trim_ws = T)

tanz_data <- read_excel(synGet(tanz.raw.original.id)$path, trim_ws = T)
tanz_solicited_metadata <- read_excel(synGet(tanz.solicited.meta.original.id)$path, trim_ws = T)
tanz_long_metadata <- read_excel(synGet(tanz.long.meta.original.id)$path, trim_ws = T)

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
all_original_data %<>% 
  mutate(StudyID = str_remove_all(StudyID, "-"))

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
         Fever, Nightsweat, Microbiologicreferencestandard) %>% 
  mutate(Type = tolower(Type)) %>% 
  mutate(StudyID = str_remove_all(StudyID, "-")) %>% 
  mutate(participant = rep("", nrow(.))) %>% 
  mutate(Microbiologicreferencestandard = str_remove_all(Microbiologicreferencestandard, "TB ")) %>% 
  mutate(Microbiologicreferencestandard = str_replace_all(Microbiologicreferencestandard, c("Negative" = "0", "Positive" = "1"))) %>% 
  mutate(Microbiologicreferencestandard = as.integer(Microbiologicreferencestandard)) %>% 
  select(Type, StudyID, participant, everything()) %>% 
  mutate(across(where(is.character), str_trim))

clinical_curated_data %<>% 
  mutate(across(where(is.character), str_trim)) %>% 
  arrange(participant)

for (i in 1:nrow(all_original_data_check)) {
  if (all_original_data_check$StudyID[i] %in% id_map$StudyID) {
    all_original_data_check$participant[i] <- id_map$participant[which(id_map$StudyID==all_original_data_check$StudyID[i])]
  }
  else all_original_data_check$participant[i] <- NA
}

all_original_data_check %<>% 
  filter(Type=="train") %>%
  select(-c(Type, StudyID)) %>%
  arrange(participant)

all_original_data_check %<>%
  drop_na(participant)

colnames(all_original_data_check) <- colnames(clinical_curated_data)

all_original_data_check$tb_prior %<>% str_replace_all(c("Unchecked" = "No", "Checked" = "Yes"))
all_original_data_check$tb_prior[is.na(all_original_data_check$tb_prior)] <- "No"
all_original_data_check$tb_prior_Pul %<>% str_replace_all(c("Unchecked" = "No", "Checked" = "Yes"))
all_original_data_check$tb_prior_Extrapul %<>% str_replace_all(c("Unchecked" = "No", "Checked" = "Yes"))
all_original_data_check$tb_prior_Unknown %<>% str_replace_all(c("Unchecked" = "No", "Checked" = "Yes"))
all_original_data_check$tb_prior_Unknown[is.na(all_original_data_check$tb_prior_Unknown)] <- "No"

# all_original_data_check[is.na(all_original_data_check)] <- ""
# clinical_curated_data[is.na(clinical_curated_data)] <- ""

check <- data.frame("1" = rep("", times = nrow(all_original_data_check)))

for (j in 1:nrow(all_original_data_check)) {
  for (k in 1:ncol(all_original_data_check)) {
    ifelse(test = all_original_data_check[j,k]==clinical_curated_data[j,k], 
           yes = check[j,k] <- T, 
           no = check[j,k] <- F)
  }
}

check2 <- data.frame("1" = rep("", times = length(check)))

for (i in 1:length(check)) {
  ifelse(test = F %in% check[,i], 
         yes = check2[i,] <- F, 
         no = check2[i,] <- T)
}

F %in% check2[,1] # Evaluates to TRUE, so need to check which data have discrepancies

which(check[7]==F)

mismatch <- tibble("curated column" = colnames(clinical_curated_data[7]), 
                   "curated participant" = clinical_curated_data[which(check[7]==F),1], 
                   "curated value" = clinical_curated_data[which(check[7]==F),7], 
                   "original column" = colnames(all_original_data[14]), 
                   "original participant" = deframe(all_original_data_check[which(check[7]==F),1]), 
                   "studyID" = "", 
                   "original value" = "")

for (i in 1:nrow(mismatch)) {
  pID <- mismatch$`curated participant`[i]
  mismatch$studyID[i] <- id_map$StudyID[which(id_map$participant==pID)]
  mismatch$`original value`[i] <- all_original_data$PriorTB[which(all_original_data$StudyID==mismatch$studyID[i])]
}

mismatch %<>% select(-`original participant`)

# Check if only training cough files have been included in the curated metadata

# Combine original metadata, select relevant columns, and extract filename
all_original_solicited_metadata <- 
  bind_rows(mad_solicited_metadata, tanz_solicited_metadata, r2d2_solicited_metadata) %>% 
  select(hyfe_id, url_peak, sound_prediction_score) %>% 
  # mutate(participant = "") %>% 
  mutate(url_peak = gsub("^.*\\/.*\\/", "", url_peak, perl = T))

all_original_long_metadata <- 
  bind_rows(mad_long_metadata, tanz_long_metadata, r2d2_long_metadata) %>% 
  select(hyfe_id, url_peak, sound_prediction_score) %>% 
  mutate(url_peak = gsub("^.*\\/.*\\/", "", url_peak, perl = T))

# Store IDs for which metadata files are in curated metadata, assign train/test classification using original HyfeID and id_map, then test for whether "test" class is present
ids_in_solicited_metadata <- enframe(unique(all_original_solicited_metadata$hyfe_id[which(all_original_solicited_metadata$url_peak %in% solicited_curated_metadata$filename)]))

ids_in_solicited_metadata %<>% 
  rename(HyfeID = value) %>% 
  select(-name) %>% 
  mutate(type = "")

for (i in 1:nrow(ids_in_solicited_metadata)) {
  if (ids_in_solicited_metadata$HyfeID[i] %in% id_map$StudyID) {
    ids_in_solicited_metadata$type[i] <- id_map$type[which(id_map$StudyID==ids_in_solicited_metadata$HyfeID[i])]
  }
  else ids_in_solicited_metadata$HyfeID[i] <- NA
}

"test" %in% ids_in_solicited_metadata$type # Evaluates to FALSE, so check is passed

ids_in_long_metadata <- enframe(unique(all_original_long_metadata$hyfe_id[which(all_original_long_metadata$url_peak %in% long_curated_metadata$filename)]))

ids_in_long_metadata %<>% 
  rename(HyfeID = value) %>% 
  select(-name) %>% 
  mutate(type = "")

for (i in 1:nrow(ids_in_long_metadata)) {
  if (ids_in_long_metadata$HyfeID[i] %in% id_map$StudyID) {
    ids_in_long_metadata$type[i] <- id_map$type[which(ids_in_long_metadata$HyfeID[i]==id_map$StudyID)]
  }
  else ids_in_long_metadata$HyfeID[i] <- NA
}

"test" %in% ids_in_long_metadata$type # Evaluates to TRUE, so further verification is needed

tmp <- ids_in_long_metadata$HyfeID[which(ids_in_long_metadata$type=="test")]

tmp2 <- vector()

for (i in 1:length(tmp)) {
  tmp2[i] <- id_map$participant[which(id_map$StudyID==tmp[i])]
}

tmp3 <- vector()

for (i in 1:length(tmp2)) {
  ifelse(test = tmp2 %in% long_curated_metadata$participant, 
         yes = tmp3[i] <- T, 
         no = tmp3[i] <- F)
}

T %in% tmp3 # Evaluates to False, so check is passed

