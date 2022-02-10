library(tidyverse)
library(readxl)

internet_raw <- read_csv("data/on-disk/CACI-access-to-internet/CACI-access-to-internet/vul_zscores.csv/vul_zscores.csv")

# Please be aware that the scores are in a range of 0-1
# A higher score indicates higher vulnerability in this case,
# Opposite to how the decile scores work.
internet_clean <-
    internet_raw %>%
    filter(region == "Wal") %>%
    select(
        postcode = "postcode",
        speed = "vul_dig_brdbnd",   # Lower decile values indicate lower fixed internet speed, most are around 0  # nolint
        online_purchasing = "vul_dig_buyonl",   # Lower decile values indicate less online purchasing, normal checked # nolint
        online_finance = "vul_dig_manca_net",   # Lower decile values indicate less likely to manage current account online, normal checked # nolint
        mobile_phone = "vul_dig_mobnon",    # Lower decile values indicate less likely to have a mobile phone, normal checked # nolint
        internet_users = "vul_dig_netusr",  # Lower decile values indicate less likely to be an internet user, skew to right # nolint
        confuse = "vul_dig_confuse" # Lower decile values indicate more likely to be confused by computers, normal checked # nolint
    )


distinct(internet_clean, speed)
ggplot(internet_clean, aes(speed)) + geom_histogram()