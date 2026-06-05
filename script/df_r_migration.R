# ============================================================
# Generate Synthetic Teaching Dataset: df_r_migration
# ============================================================

# ---------------------------
# 1. Load required packages
# ---------------------------
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(readr)
library(fs)

# ---------------------------
# 2. Set seed for reproducibility
# ---------------------------
set.seed(123)

# ---------------------------
# 3. Create project folders
# ---------------------------
dir_create("data/raw")
dir_create("data/processed")
dir_create("reports")
dir_create("scripts")

# ---------------------------
# 4. Define base parameters
# ---------------------------
n <- 650

countries <- c(
  "Ireland","United Kingdom","United States","Denmark","Norway",
  "Germany","Singapore","Switzerland","South Korea","Japan",
  "Canada","China","Australia","France","Netherlands",
  "India","Brazil","Mexico","Indonesia","Nigeria"
)

# Weighted sampling (higher-ranked appear more often)
weights <- c(1.8,1.8,1.8,1.6,1.6,1.5,1.5,1.4,1.3,1.3,
             1.2,1.2,1.1,1.1,1.0,0.9,0.9,0.8,0.8,0.7)

regions <- c("Europe","North America","Asia","Oceania","South America","Africa")

job_roles <- c("Analyst","Manager","Clerk","Engineer","Consultant","Student")
industries <- c("Finance","Healthcare","Education","Retail","Tech","Government")

background_clean <- c("Excel","Spreadsheet","Google Sheets","Power BI","Manual Reporting")

learning_modes <- c("Self-paced","Instructor-led","Hybrid")

income_bands <- c("Low","Lower-Middle","Upper-Middle","High")

completion_status <- c("Completed","In Progress","Dropped","Paused")

# ---------------------------
# 5. Payment probability mapping
# ---------------------------
country_payment_scores <- tibble::tribble(
  ~country, ~payment_probability_score,
  "Ireland", 25.8, "United Kingdom", 24.9, "United States", 24.2,
  "Denmark", 23.5, "Norway", 22.1, "Germany", 21.8,
  "Singapore", 20.4, "Switzerland", 19.7, "South Korea", 18.6,
  "Japan", 17.9, "Canada", 17.2, "China", 16.5,
  "Australia", 15.9, "France", 14.1, "Netherlands", 13.4,
  "India", 9.7, "Brazil", 8.9, "Mexico", 7.6,
  "Indonesia", 6.8, "Nigeria", 4.1
)

# ---------------------------
# 6. Generate clean base dataset
# ---------------------------
df_clean <- tibble(
  learner_id = sprintf("L%04d", 1:n),
  learner_name = paste0("Learner_", sprintf("%03d", 1:n)),
  country = sample(countries, n, replace = TRUE, prob = weights),
  region = sample(regions, n, replace = TRUE),
  job_role = sample(job_roles, n, replace = TRUE),
  industry = sample(industries, n, replace = TRUE),
  background = sample(background_clean, n, replace = TRUE),
  excel_years = round(runif(n, 0, 10),1),
  r_experience_level = sample(c("Beginner","Intermediate","Advanced"), n, replace = TRUE, prob = c(0.6,0.3,0.1)),
  enrolment_date = sample(seq.Date(as.Date("2025-01-01"), as.Date("2026-01-01"), by = "day"), n, replace = TRUE),
  first_login_date = enrolment_date + sample(0:10, n, replace = TRUE),
  week = sample(1:12, n, replace = TRUE),
  study_hours = round(rnorm(n, 12, 4),1),
  lessons_completed = round(runif(n, 5, 40)),
  confidence_score = round(runif(n, 40, 100)),
  pre_assessment_score = round(runif(n, 20, 80)),
  project_score = round(runif(n, 50, 100)),
  first_script_completed = sample(c(TRUE, FALSE), n, replace = TRUE),
  used_rstudio = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.8,0.2)),
  used_quarto = sample(c(TRUE, FALSE), n, replace = TRUE),
  used_git = sample(c(TRUE, FALSE), n, replace = TRUE),
  used_renv = sample(c(TRUE, FALSE), n, replace = TRUE),
  preferred_learning_mode = sample(learning_modes, n, replace = TRUE),
  monthly_income_band = sample(income_bands, n, replace = TRUE),
  completion_status = sample(completion_status, n, replace = TRUE, prob = c(0.4,0.3,0.2,0.1)),
  manager_support = sample(c(TRUE, FALSE), n, replace = TRUE),
  notes = sample(c("Good progress","Needs support","Fast learner","Struggling", NA), n, replace = TRUE)
) %>%
  left_join(country_payment_scores, by = "country")

# ---------------------------
# 7. Inject realistic relationships
# ---------------------------
df_clean <- df_clean %>%
  mutate(
    project_score = pmin(100, project_score + study_hours * 0.5),
    lessons_completed = ifelse(manager_support, lessons_completed + 5, lessons_completed)
  )

# ---------------------------
# 8. Create messy version
# ---------------------------
df_messy <- df_clean

# 8.1 Background inconsistencies
background_variants <- c("Excel","excel","MS Excel","SpreadSheet","spreadsheet",
                         "Spreadsheet","Google Sheets","google sheets",
                         "Power BI","power bi","Manual Reporting")

df_messy$background <- sample(background_variants, n, replace = TRUE)

# 8.2 Numbers stored as text
num_to_char <- function(x) ifelse(runif(length(x)) < 0.4, as.character(x), x)

df_messy <- df_messy %>%
  mutate(
    study_hours = num_to_char(study_hours),
    excel_years = num_to_char(excel_years),
    confidence_score = num_to_char(confidence_score),
    project_score = num_to_char(project_score)
  )

# 8.3 Missing values
introduce_na <- function(x, prob = 0.1) {
  x[sample(1:length(x), size = floor(prob * length(x)))] <- NA
  x
}

df_messy <- df_messy %>%
  mutate(
    confidence_score = introduce_na(confidence_score),
    project_score = introduce_na(project_score),
    first_login_date = introduce_na(first_login_date),
    manager_support = introduce_na(manager_support),
    notes = introduce_na(notes)
  )

# 8.4 Duplicate IDs
dup_indices <- sample(1:n, 30)
df_messy$learner_id[dup_indices] <- df_messy$learner_id[dup_indices - 1]

# 8.5 Date inconsistencies
format_mixed_dates <- function(dates) {
  formats <- c("%Y-%m-%d","%d-%m-%Y","%m/%d/%Y","%d %b %Y")
  map_chr(dates, ~ format(.x, sample(formats,1)))
}

df_messy$enrolment_date <- format_mixed_dates(df_messy$enrolment_date)
df_messy$first_login_date <- format_mixed_dates(as.Date(df_messy$first_login_date, origin = "1970-01-01"))

# 8.6 Country case issues
case_variants <- function(x) {
  sample(c(str_to_lower(x), str_to_upper(x), x), 1)
}
df_messy$country <- map_chr(df_messy$country, case_variants)

# 8.7 Text spacing
add_space <- function(x) {
  ifelse(runif(length(x)) < 0.3, paste0(" ", x, " "), x)
}

df_messy <- df_messy %>%
  mutate(across(c(country, background, job_role, industry, completion_status), add_space))

# 8.8 Logical inconsistencies
logical_variants <- c(TRUE, FALSE, "Yes","No","Y","N",1,0)

df_messy <- df_messy %>%
  mutate(across(c(first_script_completed, used_rstudio, used_quarto,
                  used_git, used_renv, manager_support),
                ~ sample(logical_variants, length(.), replace = TRUE)))

# ---------------------------
# 9. Create clean reference version
# ---------------------------
df_clean_ref <- df_messy %>%
  clean_names() %>%
  mutate(
    country = str_to_title(str_trim(country))
  )

# ---------------------------
# 10. Save outputs
# ---------------------------
write_csv(df_messy, "data/raw/df_r_migration_messy.csv")
write_rds(df_messy, "data/processed/df_r_migration_messy.rds")
write_rds(df_clean_ref, "data/processed/df_r_migration_clean_reference.rds")

# ---------------------------
# 11. Data dictionary
# ---------------------------
data_dict <- tibble(
  column = names(df_messy),
  description = c(
    "Unique learner ID","Synthetic learner name","Country",
    "Region","Job role","Industry","Background",
    "Years of Excel experience","R experience level",
    "Enrolment date","First login date","Week",
    "Study hours","Lessons completed","Confidence score",
    "Pre-assessment score","Project score",
    "First script completed","Used RStudio","Used Quarto",
    "Used Git","Used renv","Learning mode",
    "Income band","Payment probability","Completion status",
    "Manager support","Notes"
  )
)

write_csv(data_dict, "reports/df_r_migration_data_dictionary.csv")

# ---------------------------
# 12. Messiness summary
# ---------------------------
messiness_summary <- tibble(
  issue = c("Duplicate IDs","Missing values (confidence_score)",
            "Character numeric fields"),
  count = c(
    sum(duplicated(df_messy$learner_id)),
    sum(is.na(df_messy$confidence_score)),
    sum(map_lgl(df_messy$study_hours, is.character))
  )
)

write_csv(messiness_summary, "reports/df_r_migration_messiness_summary.csv")

# ---------------------------
# 13. Validation checks
# ---------------------------
stopifnot(nrow(df_messy) >= 600)
stopifnot(all(c("learner_id","country","study_hours") %in% names(df_messy)))
stopifnot(length(unique(str_to_title(df_clean_ref$country))) == 20)
stopifnot(sum(duplicated(df_messy$learner_id)) > 0)
stopifnot(sum(is.na(df_messy$confidence_score)) > 0)
stopifnot(any(map_lgl(df_messy$study_hours, is.character)))

# ---------------------------
# 14. Completion message
# ---------------------------
cat("df_r_migration dataset generated successfully.\n")
