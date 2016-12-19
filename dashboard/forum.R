# Forum testing

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(tidyjson)
con <- psql(postgres_defaults$hostname, postgres_defaults$port, postgres_defaults$user, postgres_defaults$password, postgres_defaults$database)

# Tests
tb <- src_tbls(con)

# Get discussion questions
dq <- tbl(con, "discussion_questions") %>%
  # Filter everything with course item id to keep posts initiated by moderators or students
  filter(is.na(course_item_id)) %>%
  # Rempove variables
  select(-discussion_question_context_type, -course_id, -course_module_id,
         -course_item_id, -country_cd, -group_id) %>%
  inner_join(tbl(con, "discussion_course_forums") %>%
               select(discussion_forum_id,
                      discussion_course_forum_title), 
             by = "discussion_forum_id") %>%
  collect() %>%
  # Remove html
  mutate(discussion_question_details = cleanFun(discussion_question_details),
         # Length for each post and title
         question_title_length = nchar(discussion_question_title),
         question_body_length = nchar(discussion_question_details)) %>%
  # Filter where body < 10 characters
  filter(question_body_length > 10)

# Detect language
dq$question_body_language <- detectLanguage(dq$discussion_question_details)$detectedLanguage

# Test
t <- dq %>% filter(question_body_language == "ENGLISH")

# Get discussion answers
da <- tbl(con, "discussion_answers") %>%
  filter(discussion_question_id %in% dq$discussion_question_id) %>%
  collect()

dqv <- tbl(con, "discussion_question_flags") %>%
  collect()
