library(dplyr)
library(stringr)

file_path <- "/Users/ian/Desktop/CW/ds_salaries.csv"
ds_salaries <- read.csv(file_path)

# str(ds_salaries)

# Filter for outliers
filtered_data <- ds_salaries %>%
  filter(salary_in_usd > quantile(salary_in_usd, 0.05) & salary_in_usd < quantile(salary_in_usd, 0.95))

# Question 1 dataprep
# Summarize salary by experience level
salary_by_experience <- ds_salaries %>%
  group_by(experience_level) %>%
  summarise(
    mean_salary = mean(salary_in_usd, na.rm = TRUE),
    median_salary = median(salary_in_usd, na.rm = TRUE),
    salary_range = max(salary_in_usd, na.rm = TRUE) - min(salary_in_usd, na.rm = TRUE),
    employee_count = n()
  )

# Group by job title
DataScience <- ds_salaries %>%
  group_by(job_title) %>%
  summarise(
    mean_salary = mean(salary_in_usd, na.rm = TRUE),
    median_salary = median(salary_in_usd, na.rm = TRUE),
    salary_range = max(salary_in_usd, na.rm = TRUE) - min(salary_in_usd, na.rm = TRUE) 
  )

#Select top 5 best paid job title
top_5_jobs <- ds_salaries %>%
  group_by(job_title) %>%
  summarise(
    mean_salary = mean(salary_in_usd, na.rm = TRUE),
    max_salary = max(salary_in_usd, na.rm = TRUE),
  ) %>%
  arrange(desc(mean_salary)) %>%
  slice_head(n = 5)

# Question 2
# Select mode of work
mode_of_work <- ds_salaries %>%
  group_by(remote_ratio) %>%
  summarise(
    mean_salary = mean(salary_in_usd, na.rm = TRUE),
    median_salary = median(salary_in_usd, na.rm = TRUE),
    salary_range = max(salary_in_usd, na.rm = TRUE) - min(salary_in_usd, na.rm = TRUE)
  )

# Question 3
# Filter and group the data to analyze company size and employee experience level
company_experience_data <- filtered_data %>%
  group_by(company_size, experience_level) %>%
  summarise(
    employee_count = n(),
    .groups = "drop"
  )

#Calculate ratio
company_experience_ratio <- company_experience_data %>%
  group_by(company_size) %>%
  mutate(ratio = employee_count / sum(employee_count)
  )

# Question 4 prep
# Create a variable name years to store the year i want to get
years <- c(2020, 2021, 2022, 2023)

# calculate the mean, median, range for each year
calculate_stats <- function(year) {
  ds_salaries %>%
    filter(work_year == year) %>%
    group_by(experience_level) %>%
    summarise(
      year = year,
      mean_salary = mean(salary_in_usd, na.rm = TRUE),
      median_salary = median(salary_in_usd, na.rm = TRUE),
      salary_range = max(salary_in_usd, na.rm = TRUE) - min(salary_in_usd, na.rm = TRUE),
      worker_count = n(), .groups = "drop"
    )
}

# Merge the data
result <- bind_rows(lapply(years, calculate_stats))


# Export the summarized data for visualisation
write.csv(filtered_data, "/Users/ian/Desktop/CW/filtered_data.csv", row.names = FALSE)
write.csv(salary_by_experience, "/Users/ian/Desktop/CW/salary_by_experience.csv", row.names = FALSE)
write.csv(top_5_jobs, "/Users/ian/Desktop/CW/top_5_jobs.csv", row.names = FALSE)
write.csv(mode_of_work, "/Users/ian/Desktop/CW/mode_of_work.csv", row.names = FALSE)
write.csv(company_experience_data, "/Users/ian/Desktop/CW/company_experience_data.csv", row.names = FALSE)
write.csv(company_experience_ratio, "/Users/ian/Desktop/CW/company_experience_ratio.csv", row.names = FALSE)
write.csv(result, "/Users/ian/Desktop/CW/salaries_statistics_by_year.csv", row.names = FALSE)