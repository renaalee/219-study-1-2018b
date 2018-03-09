library(readr)
library(tidyr)
library(stringr)
library(dplyr)

# EEG Data ####

# filter down to critical electrodes to save space in merged file.
# these are the electrodes that we pre-registered as part of the analysis,
# so there is no need to look at the other electrodes we recorded from.
electrodes <- c(33, 39, 45, 70, 11, 83, 62, 122, 115, 108, 42, 93, 129)

all.files <- dir('data/raw/eeg')

all.data <- NA
for(f in all.files){
  subject <- f %>% stringr::str_sub(start=1,end=2)
  stimuli.condition <- f %>% stringr::str_extract("([A-Z])\\w+")
  type.condition <- f %>% stringr::str_extract("([A-Z])\\w+,")
  if(stimuli.condition == "Lang"){
    stimuli.condition <- "Language"
  } else {
    stimuli.condition <- "Music"
  }
  if(type.condition == "Gram,"){
    type.condition <- "Grammatical"
  } else {
    type.condition <- "Ungrammatical"
  }
  file.data <- read_table2(paste0('data/raw/eeg/',f), col_names = as.character(1:129))
  file.data$t <- -100:999
  file.data.tidy <- file.data %>% gather(key="electrode", value=voltage, 1:129) %>% filter(electrode %in% electrodes)
  file.data.tidy$subject <- subject
  file.data.tidy$stimulus.condition <- stimuli.condition
  file.data.tidy$grammar.condition <- type.condition
  if(is.na(all.data)){
    all.data <- file.data.tidy
  } else {
    all.data <- rbind(all.data, file.data.tidy)
  }
}


write_csv(all.data, path="data/generated/eeg_data_tidy.csv")

# Behavioral Data ####

beh.data <- read_csv('data/raw/behavioral/behavioral_data_raw.csv')
beh.data.subset <- beh.data %>% 
  select(subject_id, rt, key_press, stimulus_type, syntax_cat) %>% 
  filter(stimulus_type != 'NULL', syntax_cat %in% c('Grammatical', 'Ungrammatical', 'Filler-Ungram', 'Filler-Gram', 'In-Key', 'Distant-Key') ) %>%
  mutate(correct = (key_press == 65 & (syntax_cat == 'Grammatical' | syntax_cat == 'In-Key' | syntax_cat == 'Filler-Gram') |
                   (key_press == 85 & (syntax_cat == 'Ungrammatical' | syntax_cat == 'Distant-Key' | syntax_cat == 'Filler-Ungram'))))

write_csv(beh.data.subset, path="data/generated/beh_data_tidy.csv")

# Demographic Data ####

dem.data <- read_csv('data/raw/demographics/demographics.csv')
as.Date(dem.data$Birthdate, format="%m/%d/%Y")
