library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(tidyjson)
library(tidyr)
con <- psql(postgres_defaults$hostname, postgres_defaults$port, postgres_defaults$user, postgres_defaults$password, "humanlang")


# Tests
tb <- src_tbls(con)

# RIR waarde
# P-waarde > 0.6 / 0.7 & < 0.95

# Collect list of graded tests on branches and with version ids
res <- retrieveGradedTests(con) %>%
  collect() %>%
  mutate(version_id = getIds(assessment_id)) %>%
  arrange(course_item_name,version_id)

# TODO: retrieve question id
orderedResponses <- tbl(con, "assessment_actions") %>% 
  # Filter for assessment id
  filter(assessment_id == assessment_id_p) %>%
  # Drop guest user id
  select(-guest_user_id) %>%
  # Select variables of interest
  select(user_id = ends_with("user_id"),
         assessment_action_id, 
         assessment_action_ts) %>%
  # Right join with assessment responses
  right_join(tbl(con, "assessment_responses") %>%
               filter(assessment_id == assessment_id_p) %>%
               select(assessment_action_id) %>%
               distinct(),
             by = "assessment_action_id") %>%
  # Group by user
  group_by(user_id) %>%
  # Arrange attempts by timestamp
  arrange(assessment_action_ts) %>%
  # Get action number
  mutate(attempt_number = row_number()) %>%
  # Drop
  ungroup() %>%
  select(assessment_action_id,
         attempt_number) 

# Get quiz responses for quiz, branch & version
r3 <- tbl(con, "assessment_actions") %>%
  # Drop guest user id
  select(-guest_user_id) %>%
  # Select variables of interest
  select(assessment_id, 
         assessment_action_id, 
         assessment_action_start_ts, 
         user_id = ends_with("user_id")) %>%
  # Filter for assessment id
  filter(assessment_id == assessment_id_p) %>%
  # Join with assessment responses to get the response ids of interest
  inner_join(tbl(con, "assessment_responses") %>%
               # Select variables of interest
               select(assessment_action_id, assessment_question_id, 
                      assessment_response_score, assessment_response_weighted_score), 
             by = "assessment_action_id") %>%
  # Make sure that assessment_response_score is either 0 or 1
  mutate(assessment_response_score = ifelse(assessment_response_score < 1, 
                                            0, assessment_response_score)) %>%
  # Mutate wrong assessment_response_score so we can sum over them
  mutate(is_incorrect = ifelse(assessment_response_score == 0, 1, 0)) %>%
  # Join to get attempt numbers
  inner_join(iop, by="assessment_action_id") %>%
  # Filter for attempt number
  filter(attempt_number == 1) %>%
  # Drop
  select(-attempt_number,
         -assessment_id,
         -assessment_action_start_ts,
         -assessment_response_weighted_score) %>%
  # Group group user
  group_by(user_id) %>%
  # Calculate score
  mutate(course_item_grade_overall = sum(assessment_response_score) / n()) %>%
  ungroup() %>%
  collect()

# Create summary stats
sumStats <- r3 %>%
  # Group by quiz item
  group_by(assessment_question_id,
           assessment_response_score) %>%
  # Summarize average score
  summarize(avg_score = mean(course_item_grade_overall),
            number_responses = n()) %>%
  # Arrange
  arrange(assessment_question_id,
          assessment_response_score) %>%
  # Mutate 1/0 to right/wrong
  mutate(is_correct = ifelse(assessment_response_score == 1, "right", "wrong")) %>%
  # Select
  select(assessment_question_id,
         is_correct,
         avg_score,
         number_responses) %>%
  ungroup() %>%
  collect()

# Get sd
sd <- sd(r3$course_item_grade_overall)

# Calculate p-values
# Definition: p-value is the percentage of people who answered question correctly
pVal <- function(data) {
  # Spread goot v bad answers
  data %>%
    select(is_correct, number_responses, assessment_question_id) %>%
    spread(., is_correct, number_responses) %>%
    # Calculate pval
    mutate(., pval = right / (right + wrong)) %>%
    select(-right, -wrong)
}



# Get pval + ritscore

pval <- pVal(res3)
ritscore <- ritScore(res3)



# Bind together
bound <- pval %>%
  left_join(ritscore, by = "assessment_question_id") %>%
  mutate(color = as.factor(classifyRitScores(pval, ritscore)))

# Plot
p <- ggplot(bound, aes(x=pval, y=ritscore, color = color)) +
  geom_point() +
  theme_cfi_scientific() +
  scale_x_continuous(limits=c(0,1),
                     name = "P-value") +
  scale_y_continuous(name = "RIT score") +
  theme(axis.text.x = element_text(),
        axis.ticks.x = element_line(),
        axis.title.x = element_text(),
        axis.title.y = element_text()) +
  scale_color_manual(values = c("green", "orange", "red"),
                     name  = "Color coding")

ggplotly(p)
