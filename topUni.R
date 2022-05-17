# https://cwur.org/ - source

library("tidyverse")
library("readxl")
library("janitor")
library("ggplot2")


# Read file ---------------------------------------------------------------



filename <- "data/topUniversities.xlsx"

read_sheet <- function(filename, sheet){
  
  df <- 
    read_excel(path = filename
               , sheet = sheet
               , col_names = TRUE
               , .name_repair = "universal") %>% 
    as_tibble() %>% 
    na.omit() %>% 
    janitor::clean_names() 
  
  return(df)
  
}

italy <- read_sheet(filename = filename, sheet = 1)  %>% 
  select(world_rank, institution, education_rank, research_rank, score) %>% 
  mutate_at(c("world_rank", "education_rank", "research_rank", "score"), as.numeric)

canada <- read_sheet(filename = filename, sheet = 2)  %>% 
  select(world_rank, institution, education_rank, research_rank, score) %>% 
  mutate_at(c("world_rank", "education_rank", "research_rank", "score"), as.numeric)

uk <- read_sheet(filename = filename, sheet = 3)  %>% 
  select(world_rank, institution, education_rank, research_rank, score) %>% 
  mutate_at(c("world_rank", "education_rank", "research_rank", "score"), as.numeric)

usa <- read_sheet(filename = filename, sheet = 4)  %>% 
  select(world_rank, institution, education_rank, research_rank, score) %>% 
  mutate_at(c("world_rank", "education_rank", "research_rank", "score"), as.numeric)

germany <- read_sheet(filename = filename, sheet = 5)  %>% 
  select(world_rank, institution, education_rank, research_rank, score) %>% 
  mutate_at(c("world_rank", "education_rank", "research_rank", "score"), as.numeric)

france <- read_sheet(filename = filename, sheet = 6)  %>% 
  select(world_rank, institution, education_rank, research_rank, score) %>% 
  mutate_at(c("world_rank", "education_rank", "research_rank", "score"), as.numeric)

china <- read_sheet(filename = filename, sheet = 7)  %>% 
  select(world_rank, institution, education_rank, research_rank, score) %>% 
  mutate_at(c("world_rank", "education_rank", "research_rank", "score"), as.numeric)

japan <- read_sheet(filename = filename, sheet = 8) %>% 
  select(world_rank, institution, education_rank, research_rank, score) %>% 
  mutate_at(c("world_rank", "education_rank", "research_rank", "score"), as.numeric)

chile <- read_sheet(filename = filename, sheet = 9) %>% 
  select(world_rank, institution, education_rank, research_rank, score) %>% 
  mutate_at(c("world_rank", "education_rank", "research_rank", "score"), as.numeric)

pakistan <- read_sheet(filename = filename, sheet = 10) %>% 
  select(world_rank, institution, education_rank, research_rank, score) %>% 
  mutate_at(c("world_rank", "education_rank", "research_rank", "score"), as.numeric)



# overall score -----------------------------------------------------------

countries <- list( italy
                   , canada
                   , uk
                   , usa
                   , germany
                   , france
                   , china
                   , japan
                   , chile
                   , pakistan)

names(countries) <- c(   "italy"
                       , "canada"
                       , "uk"
                       , "usa"
                       , "germany"
                       , "france"
                       , "china"
                       , "japan"
                       , "chile"
                       , "pakistan")

countries_df <- bind_rows(countries, .id = "name")

countries_df <- countries_df %>% 
  mutate(group = ifelse(name == "chile"|name == "china"|name == "pakistan", "other", "g7"))

# l'italia non è diversa da paesi più sviluppati in termini di score assoluto
countries_df %>% 
  ggplot(aes(x = name, y = score, color = group)) +
  geom_boxplot() +
  ggtitle("Overall score", subtitle = "CWUR university ranking 2022-2023")

score_aov <- aov(score ~ name, data = countries_df)
summary(score_aov) #pvalue < .05 quindi esistono differenze tra gruppi
score_aov_tukey <- TukeyHSD(score_aov)$name %>%
  as.data.frame() %>% 
  rownames_to_column("comparison") %>% 
  as_tibble() %>% 
  clean_names() %>% 
  filter(p_adj < 0.05) 

# l'italia non è diversa da paesi più sviluppati in termini di score assoluto
countries_df %>% 
  ggplot(aes(x = name, y = education_rank, color = group)) +
  geom_boxplot() +
  ggtitle("Education rank", subtitle = "CWUR university ranking 2022-2023")

edu_aov <- aov(education_rank ~ name, data = countries_df)
summary(edu_aov) #pvalue < .05 quindi esistono differenze tra gruppi
edu_aov_tukey <- TukeyHSD(edu_aov)$name %>%
  as.data.frame() %>% 
  rownames_to_column("comparison") %>% 
  as_tibble() %>% 
  clean_names() %>% 
  filter(p_adj < 0.05) 

countries_df %>%
  arrange(education_rank) %>% 
  rename(education = education_rank) %>% 
  select(name, education) %>% 
  na.omit() %>% 
  group_by(name) %>% 
  mutate(n = row_number()) %>% 
  ungroup() %>% 
  ggplot(aes(x=n, y = education, color = name)) +
  geom_line() + 
  geom_point()

