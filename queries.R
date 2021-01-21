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
first_assessments <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1roCRVLC7EkDbKj8InxMiEqyQo5N15_JxBqpcKCOAB3E")
first_subjects <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1roCRVLC7EkDbKj8InxMiEqyQo5N15_JxBqpcKCOAB3E#gid=1543210053")
first_questions <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1roCRVLC7EkDbKj8InxMiEqyQo5N15_JxBqpcKCOAB3E#gid=495058202")
num_words <- sqldf("SELECT COUNT(*) AS cnt FROM first_questions WHERE QuestionId LIKE '1.d%'")$cnt

# Define a function that can run a variety of queries based on a QuestionId prefix and Answer query part
print_cdi_summary <- function(type,question_query,answer_query="") {
  print(type)
  print("-----------------------")
  for (group_query in c('8 session', '1 session', 'NO session')) {
    print(group_query)
    for (timepoint in c(1,2,3)) {
      query <- str_interp("
        SELECT SUM(cnt) AS cnt FROM (
          SELECT SubjectId, COUNT(DISTINCT(QuestionId)) AS cnt
            FROM first_assessments
            WHERE QuestionId LIKE '${question_query}' AND
                  EvalNum <= ${timepoint} AND
                  `Group` = '${group_query}'
                  ${if (nchar(answer_query) > 0) paste('AND ', answer_query) else ''}
            GROUP by SubjectId
            ORDER BY EvalNum
        )")
      print(sqldf(query)$cnt)
    }
  }
}

# Report on a few bits of data
print_cdi_summary('Words Produced', '1.d%', "Answer = 'says'")
print_cdi_summary('Words Understood', '1.d%')
print_cdi_summary('Phrases Understood', '1.b%')
print_cdi_summary('Total Gestures', '2.%', "Answer IN ('yes','sometimes', 'often')")

# Compute Improvement... A couple of things to NOTE:
#   1. we use SUM(EvalNum = 1) so that we can get a row for EVERY subject even if they have ZERO observations at Timepoint1
#   2. we use COUNT(DISTINCT QuestionId) so that we get a total UNIQUE word count across all Timepoints
compute_words <- function(says = FALSE, total_words) {
  query <- str_interp("
    SELECT SubjectId, `Group`,
                      SUM(EvalNum=1) AS WordsTimepoint1,
                      SUM(EvalNum=1)/${total_words}.0 AS WordsTimepoint1Pct,
                      COUNT(DISTINCT QuestionId) AS WordsTimepoint3,
                      COUNT(DISTINCT QuestionId)/${total_words}.0 AS WordsTimepoint3Pct
                      FROM first_assessments
                      WHERE QuestionId LIKE '1.d%'
                      ${if (says) 'AND Answer = \"says\"' else ''}
                      GROUP BY SubjectId
                      ORDER BY `Group`")
  result <- sqldf(query)
  return(result)
}

compute_improvement <- function(words) {
  improvement <- sqldf("SELECT SubjectId, `Group`, WordsTimepoint3Pct-WordsTimepoint1Pct AS PctImprovement FROM words")
  return(improvement)
}

# Print out the improvement numbers
print(compute_improvement(compute_words(says = FALSE, total_words = num_words)))
print(compute_improvement(compute_words(says = TRUE, total_words = num_words)))

# Just emit summary word count/percentage data
print(compute_words(says = TRUE, total_words = num_words))
