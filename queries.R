library(sqldf)
library(stringr)

# Load a data frame of the FIRST data
first_data <- read.csv("~/Desktop/FIRST-data.csv", stringsAsFactors = FALSE)

# Define a function that can run a variety of queries based on a QuestionId prefix and Answer query part
print_cdi_summary <- function(type,question_query,answer_query="") {
  print(type)
  print("-----------------------")
  for (group_query in c('8 session', '1 session', 'NO session')) {
    print(group_query)
    for (timepoint in c(1,2,3)) {
      if (nchar(answer_query) > 0) {
        query <- str_interp("SELECT SUM(cnt) AS cnt FROM(
                               SELECT SubjectId, COUNT(DISTINCT(QuestionId)) AS cnt FROM
                                 first_data WHERE QuestionId LIKE '${question_query}' AND
                                                  EvalNum <= ${timepoint} AND
                                                  `Group` = '${group_query}' AND
                                                  ${answer_query}
                                            GROUP by SubjectId ORDER BY EvalNum
                            )")
      }
      else {
        query <- str_interp("SELECT SUM(cnt) AS cnt FROM(
                               SELECT SubjectId, COUNT(DISTINCT(QuestionId)) AS cnt FROM
                                 first_data WHERE QuestionId LIKE '${question_query}' AND
                                                  EvalNum <= ${timepoint} AND
                                                  `Group` = '${group_query}'
                                            GROUP by SubjectId ORDER BY EvalNum
                            )")
      }
      print(sqldf(query)$cnt)
    }
  }
}

# Report on a few bits of data
print_cdi_summary('Words Produced', '1.d%', "Answer = 'says'")
print_cdi_summary('Words Understood', '1.d%')
print_cdi_summary('Phrases Understood', '1.b%')
print_cdi_summary('Total Gestures', '2.%', "Answer IN ('yes','sometimes', 'often')")

# TODO change the print_cdi_summary function (or write a different one) to built and plot a data.frame so that we don't have
# to manually copy numbers into Google sheets