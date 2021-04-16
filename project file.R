library(tidyverse)
library(stargazer)

library(readr)
gtd_raw <- read_csv("gtd_raw.csv")
View(gtd_raw)

# Are there determinants that affect the number of casualties in terror attacks?

# Theory:
# Determinants such as type of weapon used and target area --> more casualties, 
# positive correlation between the types of determinants and number of casualties

#H0: The types of determinants have no effect on the number of fatalities in terrorist attacks.

#Ha: The types of determinants have a positive effect on the number of fatalites in terrorist attacks.

#Dependent variables:
# Number of fatalities in attacks
summary(gtd_raw$nkill)

# Independent variable:
# Type of weapon used
summary(gtd_raw$weaptype1_txt)

# Histogram of data
hist(gtd_raw$weaptype1)
d <- density(gtd_raw$weaptype1)
plot(d)

# Add weapon class ####
unique(gtd_raw$weaptype1_txt)

# Analysis df 
gtd_df <- gtd_raw 

# Group by weap type 1_txt 
gtd_df %>% 
  filter(!is.na(weaptype1_txt)) %>% 
  group_by(weaptype1_txt) %>% 
  count() %>% 
  arrange(desc(n))

# Explosives
explosives_df <- gtd_df %>% 
  filter(weaptype1_txt == "Explosives" |
           weaptype1_txt == "Incendiary" ) %>% 
  mutate('Explosives?' = 1)

nonexplosives <- gtd_df %>% 
  filter(weaptype1_txt != "Explosives" & weaptype1_txt != "Incendiary") %>%
  mutate('Explosives?' = 0)

# Chemical
chemical_df <- gtd_df %>% 
  filter(weaptype1_txt == "Chemical" |
           weaptype1_txt == "Biological" |
        weaptype1_txt == "Radiological"
        ) %>% 
  mutate("Chemical?" = 1)

nonchemical <- gtd_df %>% 
  filter(weaptype1_txt != "Chemical" & weaptype1_txt != "Biological" & 
           weaptype1_txt != "Radiological") %>% 
  mutate( 'nonchemical' = 0)

# Close Quarter
closequarter_df <- gtd_df %>% 
  filter(weaptype1_txt == "Melee" |
           weaptype1_txt == "Sabotage Equipment" |
           weaptype1_txt == "Vehicle" |
           weaptype1_txt == "Firearms"
         ) %>% 
  mutate("CloseQuarter" = 1)

nonclosequarter <- gtd_df %>% 
  filter(weaptype1_txt != "Melee" & weaptype1_txt != "Sabotage Equipment" & weaptype1_txt
         != "Vehicle" & weaptype1_txt != "Firearms") %>% 
  mutate(class = "nonclosequarter" = 0)

# Other
other_df <- gtd_df %>% 
  filter(weaptype1_txt == "Other" |
           weaptype1_txt == "Unknown"|
           weaptype1_txt == "Fake Weapons"
         ) %>% 
  mutate("Other" = 1)

nonother <- gtd_df %>% 
  filter(weaptype1_txt != "Other" & weaptype1_txt != "Unknown" & weaptype1_txt
         != "Fake Weapons") %>% 
  mutate(class = "Nonother" = 0)



view(explosives_df)
view(closequarter)


model1 <- lm(data = gtd_raw, `explosives?` ~ nkill)
summary(simple_model)

stargazer(simple_model, type = 'html', out = gtd_raw <- read_csv("gtd_raw.csv") )

# Script ####

# Analysis df 
gtd_df <- gtd_raw 

# Group by weap type 1_txt 
gtd_df %>% 
  filter(!is.na(weaptype1_txt)) %>% 
  group_by(weaptype1_txt) %>% 
  count() %>% 
  arrange(desc(n))

# Break into Explosives vs NonExplosives 
explosives_df <- gtd_df %>% 
  filter(weaptype1_txt == "Explosives" | weaptype1_txt == "Incendiary") %>%
  mutate(`explosives?` = 1)

nonexplosives <- gtd_df %>% 
  filter(weaptype1_txt != "Explosives" & weaptype1_txt != "Incendiary") %>%
  mutate(`explosives?` = 0)

# Chemical
chemical_df <- gtd_df %>% 
  filter(weaptype1_txt == "Chemical" |
           weaptype1_txt == "Biological" |
           weaptype1_txt == "Radiological"
  ) %>% 
  mutate("Chemical" = 1)

nonchemical <- gtd_df %>% 
  filter(weaptype1_txt != "Chemical" & weaptype1_txt != "Biological" & weaptype1_txt 
         != "Radiological") %>% 
  mutate("Chemical" = 0)


# Bind Rows into analysis df 
analysis_df <- bind_rows(explosives_df, nonexplosives)

# GLM model for year x Explosives 
analysis_df %>% 
  ggplot(aes(x=iyear, y='Explosives'))+
  geom_point()+
  geom_smooth(method="glm", se=FALSE, method.args = list(family = "binomial"))

# GLM model for Nkill x Explosives 
analysis_df %>% 
  ggplot(aes(x=nkill , y= Chemical))+
  geom_point()+
  geom_smooth(method="glm", se=FALSE, method.args = list(family = "binomial"))



