library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(tidyjson)
con <- psql(postgres_defaults$hostname, postgres_defaults$port, postgres_defaults$user, postgres_defaults$password, postgres_defaults$database)

# Tests
tb <- src_tbls(con)

# Function to get a List of ids
getIds <- function(split_me) {
  sm <- sapply(split_me, function(x) strsplit(x, "@")[[1]][2])
  as.numeric(unname(sm))
}

# RIR waarde
# P-waarde > 0.6 / 0.7 & < 0.95

# Collect list of graded tests on branches and with version ids
res <- retrieveGradedTests(con) %>%
  collect() %>%
  mutate(version_id = getIds(assessment_id)) %>%
  # Select latest version
  group_by(course_item_id) %>%
  filter(version_id == max(version_id))

# TODO: retrieve question id

# Get quiz responses for quiz, branch & version
res2 <- retrieveGradedTests(con) %>% 
  filter(course_item_id == res$course_item_id[3]) %>%
  # Select the first row
  filter(row_number() == 1) %>%
  select(assessment_id, course_item_id) %>%
  # Join with assessment actions to get the action ids of interest
  inner_join(tbl(con, "assessment_actions") %>%
               # Drop guest user id
               select(-guest_user_id) %>%
               # Select variables of interest
               select(assessment_id, assessment_action_id, assessment_action_start_ts, user_id = ends_with("user_id")), 
             by="assessment_id") %>%
  # Join with assessment responses to get the response ids of interest
  inner_join(tbl(con, "assessment_responses") %>%
               # Select variables of interest
               select(assessment_action_id, assessment_action_version, assessment_question_id, 
                      assessment_response_score, assessment_response_weighted_score), 
             by = "assessment_action_id") %>%
  # Mutate wrong assessment_response_score so we can sum over them
  mutate(is_incorrect = ifelse(assessment_response_score == 0, 1, 0)) %>%
  # Join with course item grades to get student grades
  inner_join(tbl(con, "course_item_grades") %>%
               select(user_id = ends_with("user_id"),
                      course_item_id,
                      course_item_grade_overall),
             by = c("course_item_id", "user_id")) 

res3 <- res2 %>%
  # Group by quiz item
  group_by(assessment_question_id,
           assessment_response_score) %>%
  # Summarize average score
  summarize(avg_score = mean(course_item_grade_overall),
            number_students = n()) %>%
  # Arrange
  arrange(assessment_question_id,
          assessment_response_score) %>%
  # Mutate 1/0 to right/wrong
  mutate(is_correct = ifelse(assessment_response_score == 1, "right", "wrong")) %>%
  # Select
  select(assessment_question_id,
         is_correct,
         avg_score,
         number_students) %>%
  ungroup() %>%
  collect()

# Calculate standard deviation
calcSD <- function(res2) {
  # Compress query by weighting scores
  res4 <- res2 %>%
    group_by(course_item_grade_overall) %>%
    summarize(weigth = n()) %>%
    collect()
  # Expand
  n <- unlist(sapply(1:nrow(res4), function(x) rep(res4$course_item_grade_overall[x], length.out = res4$weigth[x])))
  sd(n)
}

# Calculate p-values
# Definition: p-value is the percentage of people who answered question correctly
pVal <- function(data) {
  # Spread goot v bad answers
  data %>%
    select(is_correct, number_students, assessment_question_id) %>%
    spread(., is_correct, number_students) %>%
    # Calculate pval
    mutate(., pval = right / (right + wrong)) %>%
    select(-right, -wrong)
}

# Calculate Rit-score
ritScore <- function(data) {
  # Pieces
  avg_score_right <- data %>% filter(is_correct == "right") %>% select(avg_score)
  avg_score_wrong <- data %>% filter(is_correct == "wrong") %>% select(avg_score)
  sd_test <- calcSD(res2)
  pValItem <- data %>% filter(is_correct == "right") %>% select(number_students) / 
    (data %>% filter(is_correct == "right") %>% select(number_students) + data %>% filter(is_correct == "wrong") %>% select(number_students))
  q <- (1-pValItem)
  # RIT formula
  ritForm <- function(avg_score_right, avg_score_wrong, sd_test, pValItem, q) {
    ((avg_score_right - avg_score_wrong) / sd_test) * sqrt(pValItem * q)
  }
  ritscore <- ritForm(avg_score_right, avg_score_wrong, sd_test, pValItem, q)
  ritscore <- cbind(unique(data$assessment_question_id), ritscore)
  names(ritscore) <- c("assessment_question_id", "ritscore")
  return(ritscore)
}

# Get pval + ritscore

pval <- pVal(res3)
ritscore <- ritScore(res3)

# Function to classify rit scores & pvalues
classifyRitScores <- function(pval, ritscores) {
  
  # Series of TRUE/FALSE statements to determine boundries
  # Pvalue: between 0.3 & 0.8
  # ritscore: above 0.3
  
  pvalcheck <- pval < 0.3 | pval > 0.8
  ritscorecheck <- ritscores < 0.3
  
  # Classify
  class <- c()
  for(n in 1:length(pvalcheck)) {
    pvalcheck1 <- pvalcheck[n]
    ritscorecheck1 <- ritscorecheck[n]
    if(pvalcheck1 & ritscorecheck1) {
      i <- "Review"
    } else if((pvalcheck1 & !ritscorecheck1) | (ritscorecheck1 & !pvalcheck1)) {
      i <- "Possible review"
    } else {
      i <- "OK"
    }
    class <- c(class, i)
  }

  # Return
  return(class)
}

# Bind together
bound <- pval %>%
  left_join(ritscore, by = "assessment_question_id") %>%
  mutate(color = as.factor(classifyRitScores(pval, ritscore)))

# Plot
p <- ggplot(bound, aes(x=pval, y=ritscore, color = color)) +
  geom_point() +
  theme_cfi_scientific() +
  scale_x_continuous(limits=c(0,1)) +
  theme(axis.text.x = element_text(),
        axis.ticks.x = element_line()) +
  scale_color_manual(values = c("green", "orange", "red"),
                     name  = "Color coding")

ggplotly(p)
