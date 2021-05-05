library(httr)
library(readxl)
library(dplyr)

GET(
  "https://hpspubsrepo.blob.core.windows.net/hps-website/nss/3073/documents/3_genital-chlamydia-gonorrhoea-scotland-2010-2019-tables.xlsx",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

chlamydia <-
  read_excel(tf, sheet = "Table 2", skip = 2) %>% 
  select(nhs_board = `NHS board`, chlamydia_count = `2019`) %>% 
  slice(1:14)

gonorrhoea <-
  read_excel(tf, sheet = "Table 6", skip = 2) %>% 
  select(nhs_board = `NHS Board`, gonorrhoea_count = `2019`) %>% 
  slice(1:14) %>% 
  mutate(gonorrhoea_count = if_else(gonorrhoea_count == "*", NA_character_, gonorrhoea_count)) %>% 
  mutate(gonorrhoea_count = as.double(gonorrhoea_count))
  
sexual_health <-
  chlamydia %>% 
  left_join(
    gonorrhoea,
    by = "nhs_board"
  )