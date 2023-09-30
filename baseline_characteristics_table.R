#Load packages 
library(azcore)
library(tidyverse)
library(dplyr)
library(r2rtf)
#For base
azcore::azcore_bundle_load("base/1.1.0")

adsl2
#add BMI, height & weight (missing from the dummy study)
adsl3 <- adsl2 %>%
  mutate(
    BMIBL = ifelse(!is.na(TRT01PN), runif(n(), min = 13.3, max = 39.5), NA_real_),
    HEIGHTBL = ifelse(!is.na(TRT01PN), runif(n(), min = 139.0, max = 194.0), NA_real_),
    WEIGHTBL = ifelse(!is.na(TRT01PN), runif(n(), min = 36.0, max = 145.0), NA_real_)
  )

# Loop through the table and replace NA values with zero & skip date variables
for (i in 1:nrow(adsl3)) {
  for (j in 1:ncol(adsl3)) {
    if (!(class(adsl3[[j]]) %in% c("Date", "POSIXct")) && is.na(adsl3[i, j])) {
      adsl3[i, j] <- 0
    }
  }
}
# Baseline table function
# Baseline table function

BaselineTables <- function(adsl3, condition_inputs) {
  tables <- list()
  
  for (i in 1:length(condition_inputs)) {
    condition <- condition_inputs[[i]]
    table_name <- paste0("Baseline_Table_", i)
    
    table <- adsl3 %>%
      filter(!!!condition)
    
    # Calculate summary statistics
    result <- table %>%
      group_by(TRT01A) %>%
      summarise(across(c(HEIGHTBL, WEIGHTBL, BMIBL), list(
        N = ~ sum(!is.na(.)),
        Mean = ~ round(mean(., na.rm = TRUE), 1),
        SD = ~ round(sd(., na.rm = TRUE), 1),
        Median = ~ round(median(., na.rm = TRUE), 1),
        Min = ~ round(min(., na.rm = TRUE), 1),
        Max = ~ round(max(., na.rm = TRUE), 1)
      ), .names = "{.col}_{.fn}")) %>%
      ungroup()
    
    # Add total row for summary statistics
    total_row <- table %>%
      summarise(across(c(HEIGHTBL, WEIGHTBL, BMIBL), list(
        N = ~ sum(!is.na(.)),
        Mean = ~ round(mean(., na.rm = TRUE), 1),
        SD = ~ round(sd(., na.rm = TRUE), 1),
        Median = ~ round(median(., na.rm = TRUE), 1),
        Min = ~ round(min(., na.rm = TRUE), 1),
        Max = ~ round(max(., na.rm = TRUE), 1)
      ), .names = "{.col}_{.fn}")) %>%
      mutate(TRT01A = "Total") %>%
      ungroup()
    
    # Combine result and total row
    result <- bind_rows(result, total_row)
    
    # Add summary statistics to the table
    table_summary <- list(data = table, summary = t(result))
    tables[[table_name]] <- table_summary
  }
  
  return(tables)
}



# Specify the data and conditions(eg all sucject, age > 18, ...etc. )
con_data <- adsl3
condition_inputs <- list(
  condition1 = list(adsl3$AGE >= 18, adsl3$SEX == "M"),
  condition2 = list(adsl3$RACE %in% c("ASIAN", "WHITE")),
  condition3 = list(adsl3$AGE >= 18, adsl3$SEX == "F")
)

# Generate baseline tables
baseline_tables <- BaselineTables(con_data, condition_inputs)

baseline_tables
#check condition1, condition2, condition3
#print(baseline_tables$Baseline_Table_1$summary)
#print(baseline_tables$Baseline_Table_2$summary)
#print(baseline_tables$Baseline_Table_3$summary)

bc_condition1 <-baseline_tables$Baseline_Table_1$summary
bc_condition2 <-baseline_tables$Baseline_Table_2$summary
bc_condition3 <-baseline_tables$Baseline_Table_3$summary

bc_condition1
#missing_values <- sum(is.na(con_data$TRT01PN))
#missing_values

convert_summary_table <- function(bc_conditions) {
  # Create an empty data frame to store the final result
  bc_final <- data.frame()
  
  # Loop through each condition and process the summary table
  for (i in seq_along(bc_conditions)) {
    # Convert summary_table to a data frame
    summary_table <- as.data.frame(bc_conditions[[i]])
    colnames(summary_table) <- c("Not Treated", "Placebo bd + Docetaxel 75 mg/m2", "Screen Failure", "Selumetinib 75 mg bd + Docetaxel 75 mg/m2", "Total")
    
    # Remove the first row
    summary_table <- summary_table[-1, ]
    
    # Add a new column with values
    sat_column <- c("N", "Mean", "SD", "Median", "Min", "Max", "N", "Mean", "SD", "Median", "Min", "Max", "N", "Mean", "SD", "Median", "Min", "Max")
    
    Lab_col <- c("Height (cm)", "", "", "", "", "", "Weight (kg)", "", "", "", "", "", "BMI (kg/m2)", "", "", "", "", "")
    
    # Bind the data frames with appropriate labels
    final <- cbind("Label" = Lab_col, "Statistics" = sat_column, summary_table)
    
    # Value to search for
    given_value <- "N"
    
    # Find the rows with the given value
    index <- which(final$Statistics == given_value)
    
    # Create an empty row
    empty_row <- rep("", ncol(final))
    
    # Add a blank line before each occurrence of the given value
    for (j in rev(index)) {
      final <- rbind(final[0:(j - 1), ], empty_row, final[j:nrow(final), ])
    }
    
    # Append the processed summary table to bc_final
    bc_final <- rbind(bc_final, final)
  }
  
  return(bc_final)
}

#bc_conditions <- list(summary_table1, summary_table2, summary_table3)  # Replace with your actual summary tables
bc_conditions <- list(baseline_tables$Baseline_Table_1$summary,
                      baseline_tables$Baseline_Table_2$summary,
                      baseline_tables$Baseline_Table_3$summary)
#result <- convert_summary_table(bc_conditions)

bc_final <- convert_summary_table(bc_conditions)
bc_final
# Add a new column named 'groupn' with all values set to 1
bc_final$groupn <- 1

# Count occurrences of "Max" in the "Statistics" column
max_count <- 0

for (i in seq_len(nrow(bc_final))) {
  if (i <= 22) {
    bc_final$groupn[i] <- 1
  } else if (i <= 43) {
    bc_final$groupn[i] <- 2
  } else if (i <= 64) {
    bc_final$groupn[i] <- 3
  }
}

# Function to generate RTF subline based on groupn
generate_rtf_subline <- function(groupn, condition_name) {
  if (groupn == 1) {
    rtf_subline(text = condition_name)  # For groupn=1, use condition_name as text1
  } else if (groupn == 2) {
    rtf_subline(text = condition_name)  # For groupn=2, use condition_name as text2
  } else {
    ""  # For other groupn values, return an empty string (or add more conditions if needed)
  }
}


n_begin  <- adsl3 %>%
  group_by(TRT01A) %>%
  summarise(n = n())

#N= count
n_tr <- n_begin$n[1]
n_pbo_75 <- n_begin$n[2]
n_sf <- n_begin$n[3]
n_75 <- n_begin$n[4]

#nrow = 29
# Step 1: Separate the "groupn" column
pagination_values <- original_data$groupn

# Step 2: Remove "groupn" column from the data frame
data_without_groupn <- subset(bc_finalt, select = -groupn)

pagination_values <- bc_final $groupn

bc_finalt <- bc_final

bc_final

x <- bc_final %>%
  rtf_page(orientation = "portrait", border_first ="single", border_last = "single", nrow=31) %>%
  rtf_title(title = "Table 14.x.x", subtitle = c("BC", "Full Analysis Set"), text_justification = "c", text_font_size = 9) %>%
  rtf_subline("place---holder", text_font_size = 8) %>%
  rtf_colheader(colheader = " |Statistics | Not Treated | Placebo bd  | Screen Failure | Selumetinib 75 mg bd  | Total",
                col_rel_width = c(8, 5, 5, 5, 5, 5, 5),
                text_justification = c("l", "l", "c", "c", "c", "c", "c"),
                border_top = rep("single", 7),
                border_right = rep("", 7),
                border_left = rep("", 7)) %>%
  rtf_colheader(colheader = paste0(" | | N=",n_tr," | N=",n_pbo_75 ,"| N=", n_sf," | N=",n_75,"| N=",n_tr+n_pbo_75+n_sf+n_75),
                col_rel_width = c(8, 5, 5, 5, 5, 5, 5),
                text_justification = rep("c", 7),
                border_top = rep("", 7),
                border_right = rep("", 7),
                border_left = rep("", 7)) %>%
  rtf_body(as_colheader = FALSE,
           col_rel_width = c(8, 5, 5, 5, 5, 5, 5),
           border_first = rep("single", 7),
           border_last = rep("single", 7),
           border_left = rep("", 7),
           border_right = rep("", 7),
           text_justification = c("l", "l", "c", "c", "c", "c", "c"),
           text_font_size = 7,
           last_row = FALSE) %>%
  rtf_footnote(footnote = c("BMI Body mass index; m2 Square meter; n Number of subjects in analysis; N Number of subjects per treatment group; SD Standard deviation; NC Not Calculated.",
                            "root/cdar/d000/d000_r_initiative/ar/r_initiative/common/dev/program baseline_characteristics_pageby_ee kgwc789",
                            paste0("Generated on: ", current_datetime) ),
               as_table = FALSE,
               text_font_size = 5,
               text_convert = FALSE) %>%
  rtf_page_header() %>%
  rtf_encode(verbose = FALSE, page_footnote = 'all')



pattern <- "place---holder"



i <- 0
while (grepl("place---holder", x$body, fixed = TRUE)) {
  i <- i + 1
  if (i == 1) {
    x$body <- sub("place---holder", glue::glue("AGE >= 18 and SEX == M"), x$body)
  } else if (i == 2) {
    x$body <- sub("place---holder", glue::glue("RACE ASIAN and WHITE"), x$body)
  } else {
    x$body <- sub("place---holder", glue::glue("AGE >= 18 and SEX == F"), x$body)
  }
}

x %>%
  write_rtf("tmp51.rtf")