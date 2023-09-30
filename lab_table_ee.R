library( azcore )
library(entimice)
entimice <- eride::eride_connect(instance= "prod")

eride::eride_clone(paths = "root/cdar/d000/d000_r_initiative/ar/r_initiative/common")

files <- c("root/cdar/d000/d000_r_initiative/ar/r_initiative/common/work_kgwc789_eride/program/lb_table.r", 
           "root/cdar/d000/d000_r_initiative/ar/r_initiative/common/work_kgwc789_eride/data/data_area/deid_rslb.sas7bdat")

eride::eride_fetch(paths=files, connection = entimice)

#sample output
# eride EntimICE connection to EntimICE-AZ Production (connected as kgwc789)
# Fetching file root/cdar/d000/d000_r_initiative/ar/r_initiative/common/work_kgwc789_eride/data/data_area/deid_rslb.sas7bdat (1 of 2)  ... OK
# Fetching file root/cdar/d000/d000_r_initiative/ar/r_initiative/common/work_kgwc789_eride/program/lb_table.r (2 of 2)  ... OK



#change the working directory as needed, example below
#setwd ("/scratch/kgwc789/entimice/root/cdar/d000/d000_r_initiative/ar/r_initiative/common/work_kgwc789_eride/program")

#open lb_table.r(needs to created in entimice first before fetch)

#Load packages 
library(azcore)
library(tidyverse)
library(dplyr)
library(r2rtf)

#get file(refer to the fetch location in SCP)
fs <- "/scratch/kgwc789/entimice/root/cdar/d000/d000_r_initiative/ar/r_initiative/common/work_kgwc789_eride/data/data_area/"
#get lab data
rslbl <- haven::read_sas( paste0(fs, "/deid_rslb.sas7bdat"))
#get adsl data(fetch adsl data if needed)
adsl2 <- haven::read_sas( paste0(fs, "/deid_adsl.sas7bdat"))

#dummy data is missing Baseline, week 24, week 52, and End-of-treatment. Thus:
#Visit 3 as Baseline(visit 1 & 2 are missing)
#Visit 12 as week 24
#visit 20 as week 52
#visit 32 as End-of-treatment

#not need for real data. 

visit_count <- rslbl %>%
  group_by(AVISIT) %>%
  summarize(Count = n())

#not need for real data(visit based on count for output data). 
rslbl_0 <- rslbl %>%
  mutate(AVISIT = case_when(
    AVISIT == "Visit 3" ~ "Baseline",
    AVISIT == "Visit 12" ~ "Week 24",
    AVISIT == "Visit 20" ~ "Week 52",
    AVISIT == "Visit 32" ~ "End-of-treatment",
    TRUE ~ AVISIT
  ))


#AZTLB01 shell template for Hematology filter
#filter ANL01FL == "Y", LBCAT == "H", !is.na(AVISIT), where H is Hematology)
rslbl_1 <-rslbl_0 %>%
  filter(
    AVISIT %in% c("Visit 3", "Week 24", "Week 52", "End-of-treatment") &
      ANL01FL == "Y" &
      LBCAT == "H" &
      !is.na(AVISIT))

#or
rslbl_1 <- rslbl_0 %>%
  filter(
    (AVISIT == "Baseline" & ANL01FL == "Y" & LBCAT == "H" & !is.na(AVISIT)) |
      (AVISIT == "Week 24" & ANL01FL == "Y" & LBCAT == "H" & !is.na(AVISIT)) |
      (AVISIT == "End-of-treatment" & ANL01FL == "Y" & LBCAT == "H" & !is.na(AVISIT)) |
      (AVISIT == "Week 52" & ANL01FL == "Y" & LBCAT == "H" & !is.na(AVISIT))
  )


# rslbl_1 <-rslbl_0 %>%
#   filter(ANL01FL == "Y", LBCAT == "H", !is.na(AVISIT))

#join adsl and lb data by USUBJID & STUDYID
#lb_merged <- inner_join(rslbl_1, adsl2, by = c("USUBJID", "STUDYID"))
lb_merged <- inner_join(rslbl_1, adsl2, by = c("USUBJID", "STUDYID"), suffix = c("", ".adsl2"))


lb_merged2 <- lb_merged  %>%
  filter(PARAM == "Blood Basophils (10**9/L)")

#not needed for real data
# Define a custom function to handle missing values in min and max calculations
min_max_with_na <- function(x) {
  if (length(na.omit(x)) > 0) {
    return(min(x, na.rm = TRUE))
  } else {
    return(NA)
  }
}

# Calculate summary statistics with custom function
summary_data2 <- lb_merged %>%
  group_by(PARAM, TRT01P, AVISIT) %>%
  summarize(
    n = n(),
    mean = round(mean(LBSTRESN, na.rm = TRUE), 4),
    sd = round(sd(LBSTRESN, na.rm = TRUE), 4),
    min = min_max_with_na(LBSTRESN),
    q1 = quantile(LBSTRESN, 0.25, na.rm = TRUE),
    median = median(LBSTRESN, na.rm = TRUE),
    q3 = quantile(LBSTRESN, 0.75, na.rm = TRUE),
    max = min_max_with_na(LBSTRESN),
    .groups = "drop"  # Override grouping behavior
  )



#summary data (for real data )
# summary_data2 <- lb_merged %>%
#   group_by(PARAM, TRT01P, AVISIT) %>%
#   summarize(
#     n = n(),
#     mean = round(mean(LBSTRESN, na.rm = TRUE), 4),
#     sd = round(sd(LBSTRESN, na.rm = TRUE), 4),
#     min = min(LBSTRESN, na.rm = TRUE, na.rm = TRUE),
#     q1 = quantile(LBSTRESN, 0.25, na.rm = TRUE),
#     median = median(LBSTRESN, na.rm = TRUE),
#     q3 = quantile(LBSTRESN, 0.75, na.rm = TRUE),
#     max = max(LBSTRESN, na.rm = TRUE, na.rm = TRUE),
#     .groups = "drop"  # Override grouping behavior
#   )



# Calculate the change from baseline for each summary statistic
change_data <- summary_data2 %>%
  filter(AVISIT != "Baseline") %>%
  group_by(PARAM, TRT01P, AVISIT) %>%
  mutate(
    n = first(n) - n() ,
    mean = mean - first(mean),
    sd = sd - first(sd),
    min = min - first(min),
    q1 = q1 - first(q1),
    median = median - first(median),
    q3 = q3 - first(q3),
    max = first(max)- max 
  ) %>%
  ungroup()


#change from baseline rename for 
change_data <- change_data %>%
  rename(c1_n = n,
         c2_mean = mean,
         c3_sd = sd,
         c4_min = min,
         c5_q1 = q1,
         c6_median = median,
         c7_q3 = q3,
         c8_max = max)

# Join the dataframes by PARAM, TRT01P, AVISIT
joined_data <- left_join(summary_data2, change_data, by = c("PARAM", "TRT01P", "AVISIT"))

#replace NA to ""
joined_data1 <- joined_data %>%
  mutate_all(~ ifelse(is.na(.), "", as.character(.)))



# Define the desired order for AVISIT values as per emplate
desired_avisit_order <- c("Baseline", "Week 24", "Week 52", "End-of-treatment")

# Order the joined_data1 dataframe
ordered_data <- joined_data1 %>%
  arrange(PARAM, TRT01P, match(AVISIT, desired_avisit_order))


# Create an empty row with the same column names as your_data
empty_row <- data.frame(matrix("", ncol = ncol(ordered_data)))
colnames(empty_row) <- colnames(ordered_data)

# Combine the empty row, your_data, and new_rows
ordered_data2 <- bind_rows(empty_row, ordered_data) %>%
  arrange(PARAM, TRT01P, AVISIT)



# Create a new column to mark the rows to be blanked
new_rows <- ordered_data2 %>%
  filter(AVISIT == "Baseline") %>%
  mutate(AVISIT = "") %>%
  distinct(PARAM, TRT01P)

final_data <- ordered_data2 %>%
  bind_rows(new_rows) %>%
  arrange(PARAM, TRT01P, AVISIT)

#place AVISIT with TRT01P as per template 
final_data2 <- final_data %>%
  mutate(
    AVISIT = ifelse(is.na(AVISIT) | AVISIT == "", lead(TRT01P), AVISIT),
    TRT01P = ifelse(is.na(TRT01P) | TRT01P == "", lead(TRT01P), TRT01P)
  )

#count for N label 
n_begin <- adsl2 %>%
  group_by(TRT01P) %>%
  summarise(n = n()) %>%
  mutate(Concatenated = paste(TRT01P, n, sep = " N = "))



final_data3 <- final_data2


final_lab <- final_data3 %>%
  left_join(n_begin, by = c("AVISIT" = "TRT01P"))%>%
  mutate(AVISIT = ifelse(!is.na(Concatenated), Concatenated, AVISIT))%>%
  select(-c(n.y, Concatenated))



#replace NA to "", order, and add extra space for table output. 
final_lab <- final_lab %>%
  mutate_all(~ ifelse(is.na(.), "", as.character(.))) %>%
  arrange(PARAM, TRT01P, match(AVISIT, desired_avisit_order)) %>%
  mutate(AVISIT = ifelse(AVISIT %in% desired_avisit_order,
                         paste0("   ", AVISIT), AVISIT))

Lab_rtf <- final_lab %>%
  select(-PARAM, -TRT01P)


Lab_rtf %>%
  rtf_page(orientation = "landscape", border_first = "single", border_last = "single", nrow=19) %>%
  rtf_title(title = "Table 14.1.1", subtitle = c("Lab", "Full Analysis Set"), text_justification = "c", text_font_size = 8) %>%
  rtf_colheader(colheader = " Group |Result  |change   ",
                col_rel_width = c(12, 15, 15),
                text_justification = c("l", "l", "l"),
                border_top = rep("single", 7),
                border_right = rep("", 7),
                border_left = rep("", 7)) %>%
  rtf_colheader(colheader = paste0("|n|mean|sd|min |q1|median|q3|max| n| mean|sd|min|q1|median|q3|max " ),
                col_rel_width =  c(10, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5),
                text_justification = rep("l", 7),
                text_font_size = 8,
                border_top = rep("single", 7),
                border_right = rep("", 7),
                border_left = rep("", 7)) %>%
  rtf_body(as_colheader = FALSE,
           col_rel_width = c(10, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5),
           border_first = rep("single", 7),
           border_last = rep("single", 7),
           border_left = rep("", 7),
           border_right = rep("", 7),
           text_justification = c("l", "c", "c", "c", "c", "c", "c", "c","c", "c", "c", "c", "c", "c", "c", "c","c"),
           text_font_size = 7,
           last_row = FALSE) %>%
  rtf_encode() %>%
  write_rtf("lab_table2.rtf")



