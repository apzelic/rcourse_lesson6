## LOAD PACKAGES ####

library(dplyr)
library(purrr)

data_results <- list.files(path = "data/results", full.names = T) %>%
    map(read.table, header = T, sep ="\t") %>%
    reduce(rbind)

#Read in extra data about specific subjects
data_subjects <-read.table("data/rcourse_lesson6_data_subjects.txt", header = TRUE, sep = "\t")

#Read in data about specific items (whatever these are, I don't know)
data_items <- read.table("data/rcourse_lesson6_data_items.txt", header=TRUE, sep = "\t")

## Clean Data ####
# Start by renaming some of the columns (we'll later eliminate colums we con't want with 'select' call)

data_clean = data_results %>%
    rename(trial_number = SimpleRTBLock.TrialNr.) %>%
    rename(congruency = Congruency) %>%
    rename(correct_response = StroopItem.CRESP.) %>%
    rename(given_response = StroopItem.RESP.) %>%
    rename(accuracy = StroopItem.ACC.) %>%
    rename(rt = StroopItem.RT.) %>%
    select(subject_id, block, item, trial_number, congruency,
           correct_response, given_response, accuracy, rt) %>%
    inner_join(data_subjects) %>%
    inner_join(data_items)

#This will separate the experiment time into a "first" half and "second" half as the data was 
# originally gathered over 4 quarters.
data_clean = data_clean %>%
    mutate(half = ifelse(block == "one" | block == "two", "first", "second"))

# Now we'll set criteria for outliers based on grouping by subject, congruency and half and
# then summarizing via computing the mean response time (rt) and the st. dev. of rt.

data_rt_sum <- data_clean %>%
    group_by(subject_id, congruency, half) %>%
    summarise(rt_mean = mean(rt),
              rt_sd = sd(rt))%>%
    ungroup() %>%
    mutate(rt_high = rt_mean + (2 * rt_sd)) %>%
    mutate(rt_low = rt_mean - (2 * rt_sd))

data_accuracy_clean = data_clean %>%
    inner_join(data_rt_sum) %>%
    filter(rt < rt_high) %>%
    filter(rt > rt_low)

data_rt_clean = data_accuracy_clean %>%
    filter(accuracy == 1)







