# Use tidyverse library for data manipulation
library(tidyverse)

# Import collection of green skills, from ESCO
green_skills_collection <- read_csv("./Data/greenSkillsCollection_en.csv", col_types = "c") %>% 
  # Rename variables to clarify reference to skills (as opposed to occupations, used in other tables)
  rename("skillUri" = "conceptUri", "skillPreferredLabel" =  "preferredLabel") %>%  
  # Exclude `conceptType`, since it's the same for every record ("KnowledgeSkillCompetence")
  select(-conceptType) 

# Import correspondence between green skills and occupations, from ESCO
occupation_skill_relations <- read_csv("./Data/occupationSkillRelations.csv", col_types = "c")

# Import occupation labels, in English
occupation_labels <- read_csv("./Data/occupations_en.csv") %>% 
  # Rename variables to clarify reference to occupation (as opposed to skills, above)
  rename(
    "occupationUri" =  "conceptUri",
    "occupationPreferredLabel" =  "preferredLabel",
    "altOccupationLabels" = "altLabels"
  ) 

# Join the tables of green skills, occupations, and their labels ----
green_skills_occupations <- full_join(green_skills_collection, occupation_skill_relations, by = "skillUri") %>% 
  full_join(occupation_labels, "occupationUri") 

# Export the table of green skills to occupations
green_skills_occupations %>%
  # Replace the newline character "\n" with "|" wherever it occurs.
  # This makes the resulting CSV file readable by Excel
  mutate(
    across(
      where(~any(str_detect(., "\n"), na.rm = T)), 
      ~str_replace_all(., "\n", "|"))
  ) %>%
  write_csv("./Data/green_skills_occupations.csv")
