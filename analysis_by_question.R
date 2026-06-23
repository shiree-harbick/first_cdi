# Make sure you install packages
# install.packages("rlang")
# install.packages("sqldf")
# install.packages("stringr")
# install.packages("readr")
# install.packages("gsheet")
# install.packages("devtools")
# install.packages("dplyr")
# install.packages("rstudioapi")

###########################################################################
#  For using Python's code: https://rstudio.github.io/reticulate/index.html
#  (e.g. matplotlib)
# install.packages("reticulate")
#
#  If you don't have conda installed at all just answer Y when it asks about Miniconda.  Otherwise do this first
# use_condaenv("your_conda_env")
# py_install(packages = "matplotlib")
# plt <- import("matplotlib.pyplot",as="plt")
###########################################################################

library(sqldf)
library(stringr)
library(gsheet)
library(reticulate)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the FIRST data from our Google Sheet
#StudentClinician2019All <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1LrUA_a0lsCMsuKaktlQRSm_eyUVr6vjP/edit?usp=sharing&ouid=110614759361893702567&rtpof=true&sd=true")
StudentClinician2019 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1LrUA_a0lsCMsuKaktlQRSm_eyUVr6vjP/edit?usp=sharing&ouid=110614759361893702567&rtpof=true&sd=true#gid=2037829746")
#StudentClinician2020All <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1gRuKxApgIrf1Sg4-F84VNmd5Q_jCKtll/edit?usp=sharing&ouid=110614759361893702567&rtpof=true&sd=true")
StudentClinician2020 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1gRuKxApgIrf1Sg4-F84VNmd5Q_jCKtll/edit?usp=sharing&ouid=110614759361893702567&rtpof=true&sd=true#gid=1458818719")
#StudentClinician2021All <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1uyfJdMDTr-2wudYEIkIyWwQN8AIl4CK7/edit?usp=sharing&ouid=110614759361893702567&rtpof=true&sd=true")
StudentClinician2021 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1uyfJdMDTr-2wudYEIkIyWwQN8AIl4CK7/edit?usp=sharing&ouid=110614759361893702567&rtpof=true&sd=true#gid=358124219")
#StudentClinician2023All <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1B174zwfe-DOK4PuzX83Nsa_VaXA-X_B3/edit?usp=sharing&ouid=110614759361893702567&rtpof=true&sd=true")
StudentClinician2023 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1B174zwfe-DOK4PuzX83Nsa_VaXA-X_B3/edit?usp=sharing&ouid=110614759361893702567&rtpof=true&sd=true#gid=765192931")

# Merge all datasets into one.
StudentClinicianAll <- do.call(
  "rbind", list(
    StudentClinician2019,
    StudentClinician2020,
    StudentClinician2021,
    StudentClinician2023
  )
)

# Check for missing values
#colSums(is.na(StudentClinicianAll))

# Remove rows where post-test (RC1B to RC7B) scores are missing
#StudentClinicianAllCleaned <- StudentClinicianAll %>% filter(!is.na(RC1B) & !is.na(RC2B) & !is.na(RC3B) & !is.na(RC4B) & !is.na(RC5B) & !is.na(RC6B) & !is.na(RC7B))
StudentClinicianAllCleaned <- StudentClinicianAll %>% filter(!is.na(RC1B) & !is.na(RC2B) & !is.na(RC3B) & !is.na(RC4B) & !is.na(RC7B))

# Remove outliers where TotalB = 0
StudentClinicianAllCleaned <- StudentClinicianAllCleaned %>% filter(TotalB != 0)


# Write the combined data to a CSV
write.csv(StudentClinicianAllCleaned, "ANOVA/input/student-clinician-all-cleaned.csv", row.names=FALSE)

# Compute some statistics
groups <- c("0", "1", "4", "8")
for (group in groups) {
  tests <- c("A", "B")
  for (test in tests) {
    #columns <- c("Total", "RC1", "RC2", "RC3", "RC4", "RC5", "RC6", "RC7")
    columns <- c("Total", "RC1", "RC2", "RC3", "RC4", "RC7")
    for (col in columns) {
      col_name = sprintf("%s%s", col, test)
      print(sprintf("Processing group %s, %s", group, col_name))
      group_data = StudentClinicianAllCleaned[StudentClinicianAllCleaned$`Session Group` == group,] 
      results = summary(group_data[[col_name]])
      results["Count"] <- nrow(group_data)
      results["Std. Dev."] <- round(sd(group_data[[col_name]]), 2)
      print(results)
    }
  }
}

# Compute t-test per question per group
#for (question in 1:7) {
for (question in c("1", "2", "3", "4", "7")) {
  pre_col <- paste0("RC", question, "A")
  post_col <- paste0("RC", question, "B")
  for (group in unique(StudentClinicianAllCleaned$`Session Group`)) {
    group_data <- subset(StudentClinicianAllCleaned, `Session Group` == group)
    t_result <- t.test(group_data[[pre_col]], group_data[[post_col]], paired = TRUE)
    print(paste("Group:", group, "Question:", question))
    print(t_result)
  }
}

# Compute difference in pre/post total per group using ANOVA
StudentClinicianAllCleaned$Score_Change <- StudentClinicianAllCleaned$TotalB - StudentClinicianAllCleaned$TotalA
anova_result <- aov(Score_Change ~ as.factor(`Session Group`), data = StudentClinicianAllCleaned)
summary(anova_result)

# Compute the improvement per question per group
#for (question in 1:7) {
for (question in c("1", "2", "3", "4", "7")) {
  improvement_col <- paste0("RC", question, "_Improvement")
  StudentClinicianAllCleaned[[improvement_col]] <- StudentClinicianAllCleaned[[paste0("RC", question, "B")]] - StudentClinicianAllCleaned[[paste0("RC", question, "A")]]
}
#aggregate(. ~ `Session Group`, data = StudentClinicianAllCleaned[,c("Session Group", paste0("RC", 1:7, "_Improvement"))], mean)
aggregate(. ~ `Session Group`, data = StudentClinicianAllCleaned[,c("Session Group", paste0("RC", c(1:4, 7), "_Improvement"))], mean)



