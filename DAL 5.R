#DAL Part Deux
library(tidyverse)
library(stringr)
library(infer)
view(gss_df)
library(qrmix)
str(gss_df$NEXTGEN)
unique(gss_df$NEXTGEN)
unique(gss_df$INTSPACE)
view(frequency_table_df)
view(chi_square_df)
analysis_df<-gss_df %>% 
  filter(YEAR == 2018 & !is.na(NEXTGEN) & !is.na (INTSPACE) & (NEXTGEN == "Agree" | 
  NEXTGEN == "Strongly Agree" | NEXTGEN == "Strongly Disagree"  | NEXTGEN == "Disagree") &
    INTSPACE == "Moderately Interested" | INTSPACE == "Very Interested" | INTSPACE == "Not at al Interested")) %>% 
  select (INTSPACE, NEXTGEN, WTSSALL) %>% 
  group_by(NEXTGEN, INTSPACE) %>% 
summarise(count=sum(WTSSALL))

analysis_df<-analysis_df %>% 
  mutate(NEXTGEN = str_remove_all(NEXTGEN, "Strongly")
         NEXTGEN = str_to_lower(NEXTGEN),
         NEXTGEN = str_trim(NEXTGEN, side = c("both"))) %>% 
  group_by(NEXTGEN, INTSPACE) %>% 
  summarise(count = sum(count))

analysis_df<- tribble(
  -NEXTGEN, -INTSPACE, -count,
  "agree", "Not at all interested",
  "disagree", "Not at all interested",)

analysis_df<- bind rows(analysis_df, add_rows)


)
