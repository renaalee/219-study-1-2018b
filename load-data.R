library(readr)
library(tidyr)
library(stringr)

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
  file.data.tidy <- file.data %>% gather(key="electrode", value=voltage, 1:129)
  file.data.tidy$subject <- subject
  file.data.tidy$stimulus.condition <- stimuli.condition
  file.data.tidy$grammar.condition <- type.condition
  if(is.na(all.data)){
    all.data <- file.data.tidy
  } else {
    all.data <- rbind(all.data, file.data.tidy)
  }
}

# filter down to critical electrodes to save space

electrodes <- c(33, 39, 45, 70, 11, 83, 62, 122, 115, 108, 42, 93, 129)

library(dplyr)
filtered.data <- all.data %>% filter(electrode %in% electrodes)

# normally I would prefer to save as CSV since it is more portable,
# but the CSV file is > 100MB which makes data sync with GitHub difficult.

write_csv(filtered.data, path="data/generated/eeg_data_tidy.csv")

# so I will use .Rdata format instead, which has much better compression.
#save(all.data, file="data/generated/voltage_data.tidy.Rdata")