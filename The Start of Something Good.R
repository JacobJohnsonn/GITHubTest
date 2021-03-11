# The Start of Something Good
options(scipen = 999)


# ------------------------------------------------------------------- Libraries ---------------------------------------------------------------------------#
############################################################################################################################################################

library(MASS, exclude = "select")
library(magrittr)
library(tidyverse)
library(psych)
library(Hmisc, exclude = c("describe", "summarize"))
library(BDgraph, exclude = "select")
library(ggpubr)
library(ggcorrplot)
library(EnvStats)
library(gt)
library(data.table)
library(reshape)

############################################################################################################################################################


# There is a button in the top right of R Studio that allows you to switch between branches

# Code to pull file information in the working directory
files <- file.info(dir()) %>% mutate(filename = rownames(.))

# Read in the data
movies <- read_csv(files %>% select(filename) %>% filter(grepl(x= filename, pattern = ".csv")) %>% slice(1) %>% pull())
ratings <- read_csv(files %>% select(filename) %>% filter(grepl(x= filename, pattern = ".csv")) %>% slice(2) %>% pull())

# Load in a couple of extra functions
source(file = "Functions_JJ.R")


# summarize the data
movies %>% summary_more()
ratings %>% summary_more()


movie_genres <- movies %>%
  select(genres) %>%
  unique() %>%
  pull() %>% 
  str_split(string =., pattern = "\\|", n = Inf) %>% 
  unlist() %>%
  unique()


movies[movie_genres] <- NA


`%not_in%` <- purrr::negate(`%in%`)

movies %>% summarize(CountOfSeparators = str_count(genres, pattern = "\\|")) %>% arrange(desc(CountOfSeparators)) %>% unique()
data.frame(Names = names(movies)) %>% filter(Names %not_in% c("title", "movieId", "genres")) %>% arrange(Names)

movies <- movies %>% 
  mutate(NotListed = case_when(grepl(x = genres, pattern = "no genres listed", ignore.case = T) ~ 1, TRUE ~ 0)) %>% 
  mutate(Action = case_when(grepl(x = genres, pattern = "action", ignore.case = T) ~ 1, TRUE ~ 0)) %>% 
  mutate(Adventure = case_when(grepl(x = genres, pattern = "advent", ignore.case = T) ~ 1, TRUE ~ 0)) %>% 
  mutate(Animation = case_when(grepl(x = genres, pattern = "animat", ignore.case = T) ~ 1, TRUE ~ 0)) %>% 
  mutate(Children = case_when(grepl(x = genres, pattern = "childr", ignore.case = T) ~ 1, TRUE ~ 0)) %>% 
  mutate(Comedy = case_when(grepl(x = genres, pattern = "comedy", ignore.case = T) ~ 1, TRUE ~ 0)) %>% 
  mutate(Crime = case_when(grepl(x = genres, pattern = "crime", ignore.case = T) ~ 1, TRUE ~ 0)) %>%
  mutate(Documentary = case_when(grepl(x = genres, pattern = "docum", ignore.case = T) ~ 1, TRUE ~ 0)) %>%
  mutate(Drama = case_when(grepl(x = genres, pattern = "drama", ignore.case = T) ~ 1, TRUE ~ 0)) %>%
  mutate(Fantasy = case_when(grepl(x = genres, pattern = "fantasy", ignore.case = T) ~ 1, TRUE ~ 0)) %>% 
  mutate(`Film-Noir` = case_when(grepl(x = genres, pattern = "film-", ignore.case = T) ~ 1, TRUE ~ 0)) %>% 
  mutate(Horror = case_when(grepl(x = genres, pattern = "horror", ignore.case = T) ~ 1, TRUE ~ 0)) %>%
  mutate(IMAX = case_when(grepl(x = genres, pattern = "IMAX", ignore.case = T) ~ 1, TRUE ~ 0)) %>% 
  mutate(Musical = case_when(grepl(x = genres, pattern = "Musical", ignore.case = T) ~ 1, TRUE ~ 0)) %>%
  mutate(Mystery = case_when(grepl(x = genres, pattern = "mystery", ignore.case = T) ~ 1, TRUE ~ 0)) %>% 
  mutate(Romance = case_when(grepl(x = genres, pattern = "Romance", ignore.case = T) ~ 1, TRUE ~ 0)) %>%
  mutate(`Sci-Fi` = case_when(grepl(x = genres, pattern = "sci-", ignore.case = T) ~ 1, TRUE ~ 0)) %>% 
  mutate(Thriller = case_when(grepl(x = genres, pattern = "thrill", ignore.case = T) ~ 1, TRUE ~ 0)) %>%
  mutate(War = case_when(grepl(x = genres, pattern = "war", ignore.case = T) ~ 1, TRUE ~ 0)) %>% 
  mutate(Western = case_when(grepl(x = genres, pattern = "Wester", ignore.case = T) ~ 1, TRUE ~ 0))
  



movies %>%
  select(-c("(no genres listed)")) %>% 
  summarize(across(Adventure:NotListed, ~ sum(.x, na.rm = T))) %>% 
  t() %>% 
  data.frame(Count = .) %>%
  mutate(Name = rownames(.)) %>% 
  select(Name, Count) %>% 
  arrange(desc(Count))





















