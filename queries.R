# Make sure you install packages
# install.packages("sqldf")
# install.packages("stringr")
# install.packages("gsheet")
# install.packages("reticulate")
# install.packages("devtools")

# For fun...
# devtools::install_github("ciannabp/inauguration")
# library(inauguration)
# inauguration("inauguration_2021")

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

# Load the FIRST data from our Google Sheet
first_assessments <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1roCRVLC7EkDbKj8InxMiEqyQo5N15_JxBqpcKCOAB3E#gid=0")
# Remove rows with empty SubjectId (there "template" rows with values like "<== Select SubjectId" and "<== Enter EvalNum")
first_assessments <- subset(first_assessments, !is.na(SubjectId) & SubjectId != "")

first_subjects <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1roCRVLC7EkDbKj8InxMiEqyQo5N15_JxBqpcKCOAB3E#gid=1543210053")
first_questions <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1roCRVLC7EkDbKj8InxMiEqyQo5N15_JxBqpcKCOAB3E#gid=495058202")
num_words <- sqldf("SELECT COUNT(*) AS cnt FROM first_questions WHERE QuestionId LIKE '1.d%'")$cnt

# Define a function that can run a variety of queries based on a QuestionId prefix and Answer query part
print_cdi_summary <- function(type, groups, question_query, answer_query="", exclude_subjects_query="") {
  print(type)
  print("-----------------------")
  for (group_query in groups) {
    print(group_query)
    for (timepoint in c(1,2,3)) {
      query <- str_interp("
        SELECT SUM(cnt) AS cnt FROM (
          SELECT SubjectId, COUNT(DISTINCT(QuestionId)) AS cnt
            FROM first_assessments
            WHERE QuestionId LIKE '${question_query}' AND
                  EvalNum <= ${timepoint} AND
                  `Group` LIKE '${group_query}%'
                  ${if (nchar(answer_query) > 0) paste('AND ', answer_query) else ''}
                  ${if (nchar(exclude_subjects_query) > 0) paste('AND ', exclude_subjects_query) else ''}
            GROUP by SubjectId
            ORDER BY EvalNum
        )")
      print(sqldf(query)$cnt)
    }
  }
}

# Compute Words... Observations are cumulative.  So we use COUNT(DISTINCT QuestionId) combined with EvalNum IN(...)
# so that we get total unique words from TimeN to TimeM reported as the total for TimeM.
compute_words <- function(name, question_section="%", answer_query="", exclude_subjects_query="") {
  query <- str_interp("
    SELECT SubjectId, `Group`, SUM(Time1) AS ${name}Time1, SUM(Time2) AS ${name}Time2, SUM(Time3) AS ${name}Time3
    FROM (
      SELECT SubjectId, `Group`, COUNT(DISTINCT QuestionId) AS Time1, 0 AS Time2, 0 AS Time3 FROM first_assessments
      WHERE QuestionId LIKE '${question_section}' AND EvalNum = 1 ${if (nchar(answer_query) > 0) paste('AND ', answer_query) else ''} GROUP BY SubjectId
        UNION ALL
      SELECT SubjectId, `Group`, 0 AS Time1, COUNT(DISTINCT QuestionId) AS Time2, 0 AS Time3 FROM first_assessments
      WHERE QuestionId LIKE '${question_section}' AND EvalNum IN (1,2) ${if (nchar(answer_query) > 0) paste('AND ', answer_query) else ''} GROUP BY SubjectId
        UNION ALL
      SELECT SubjectId, `Group`, 0 AS Time1, 0 AS Time2, COUNT(DISTINCT QuestionId) AS Time3 FROM first_assessments
      WHERE QuestionId LIKE '${question_section}' AND EvalNum IN (1,2,3) ${if (nchar(answer_query) > 0) paste('AND ', answer_query) else ''} GROUP BY SubjectId
    )
    ${if (nchar(exclude_subjects_query) > 0) paste('WHERE ', exclude_subjects_query) else ''}
    GROUP BY SubjectId ORDER BY `Group`"
  )

  result <- sqldf(query)
  return(result)
}

merge_and_clean_metrics <- function(dfs) {
  # Merge the frames passed in.
  results <- Reduce(function(x, y) merge(x, y, by = c("SubjectId", "Group"), all = TRUE), dfs)
  
  # Find the numeric columns and replace an NA values with 0
  num_cols <- sapply(results, is.numeric)
  results[num_cols] <- lapply(results[num_cols], function(x) ifelse(is.na(x), 0, x))
  
  return(results)
}

check_word_metrics <- function(word_metrics) {
  # For each metric, check if Time1 < Time2 < Time3
  word_metrics$AllComprehension_increasing <- with(word_metrics, AllComprehensionTime1 <= AllComprehensionTime2 & AllComprehensionTime2 <= AllComprehensionTime3)
  word_metrics$PhrasesUnderstood_increasing <- with(word_metrics, PhrasesUnderstoodTime1 <= PhrasesUnderstoodTime2 & PhrasesUnderstoodTime2 <= PhrasesUnderstoodTime3)
  word_metrics$Imitation_increasing <- with(word_metrics, ImitationTime1 <= ImitationTime2 & ImitationTime2 <= ImitationTime3)
  word_metrics$GesturesUsed_increasing <- with(word_metrics, GesturesUsedTime1 <= GesturesUsedTime2 & GesturesUsedTime2 <= GesturesUsedTime3)
  
  # If you want a single column that is TRUE only if all metrics are strictly increasing for that row:
  word_metrics$all_increasing <- with(
    word_metrics,
    AllComprehension_increasing &
    PhrasesUnderstood_increasing &
    Imitation_increasing &
    GesturesUsed_increasing
  )
  
  # View rows where the condition fails
  bad_rows = word_metrics[!word_metrics$all_increasing, ]
  if (nrow(bad_rows) > 0) {
    print(bad_rows)
  }
  else {
    print("All data is VALID")
  }
}

#####################################################################
# Show results... The code in this section is what generated the tab in the 
# data spreadsheet called "Produced Words Cumulative"... It is what was shared
# with the statistician from The Analysis Factor.

exclude_subjects = "SubjectId NOT IN ('HIGHU', 'SMIAS', 'STEYU')"
groups = c('8 session', '4 session', '4 session on-Tu', '4 session on-Th', '4 session off', '1 session', 'NO session', '8 session 2023', 'NO session 2023')

# Print out word metrics
all_comprehension <- compute_words(
  name="AllComprehension",
  question_section="%",     # All questions
  answer_query="Answer IN ('yes', 'understands', 'says', 'sometimes', 'often')",
  exclude_subjects_query=exclude_subjects
)
phrases_understood <- compute_words(
  name="PhrasesUnderstood",
  question_section="1.b%",  # All of Section 1.b
  answer_query="Answer = 'understands'",
  exclude_subjects_query=exclude_subjects
)
phrases_produced <- compute_words(
  name="PhrasesProduced",
  question_section="1.d%",  # All of Section 1.d
  answer_query="Answer = 'says'",
  exclude_subjects_query=exclude_subjects
)
imitation <- compute_words(
  name="Imitation",
  question_section="1.c.1", # Just question 1.c.1
  answer_query="Answer IN ('sometimes', 'often')",
  exclude_subjects_query=exclude_subjects
)
gestures_used <- compute_words(
  name="GesturesUsed",
  question_section="2.%",   # All of Section 2.
  answer_query="Answer IN ('sometimes', 'often')",
  exclude_subjects_query=exclude_subjects
)
demographic_data <- sqldf(str_interp("SELECT SubjectId, `Group`, Gender, Birthday FROM first_subjects WHERE ${exclude_subjects}"))

# Merge and clean the metrics...
all_word_metrics <- merge_and_clean_metrics(list(demographic_data, all_comprehension, phrases_understood, phrases_produced, imitation, gestures_used))

# Check that all metrics are cumulative i.e. the value at Time1 <= Time2 <= Time3
check_word_metrics(all_word_metrics)

# Write it to a CSV
write.csv(all_word_metrics, file = "~/Downloads/FIRST_word_metrics.csv", row.names = FALSE)

#####################################################################
# Other queries or reports that might be useful or interesting

# Report on various Group-Level Metrics (WARNING: read the function to understand the query constraints)
print_cdi_summary('Words Produced', groups, '1.d%', "Answer = 'says'", exclude_subjects)
print_cdi_summary('Words Understood', groups, '1.d%', exclude_subjects)
print_cdi_summary('Phrases Understood', groups, '1.b%', exclude_subjects)
print_cdi_summary('Total Gestures', groups, '2.%', "Answer IN ('yes','sometimes', 'often')", exclude_subjects)

# Find an outlier in total number of words produced... Just ignore EvalDate and count total
sqldf(str_interp("SELECT SubjectId, COUNT(*) FROM first_assessments WHERE Answer='says' GROUP BY SubjectId"))

# Count how many words DIOCA says at each EvalDate.
sqldf(str_interp("SELECT EvalDate, COUNT(*) FROM first_assessments WHERE Answer='says' AND SubjectId='DIOCA' GROUP BY EvalDate"))

# How many subjects per group?
sqldf(str_interp("SELECT `Group`, COUNT(*) FROM first_subjects GROUP BY `Group`"))

# Average age in months at EvalNum=3 by group?
sqldf(str_interp("SELECT `Group`, AVG(SubjectAgeMonths) FROM first_assessments WHERE EvalNum = 3 GROUP BY `Group`"))
# WITHOUT DIOCA
sqldf(str_interp("SELECT `Group`, AVG(SubjectAgeMonths) FROM first_assessments WHERE EvalNum = 3 AND SubjectId != 'DIOCA' GROUP BY `Group`"))