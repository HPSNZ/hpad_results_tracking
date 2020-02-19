## Establish a list of Games medallists using gracenote data
# Ben Day
# 19/02/2020

# ---------------------
## INITIALISATION
pckgs <- c("stringr", "purrr", "repurrrsive", "lubridate", "readxl",
           "tidyverse", "ggplot2", "ggthemr", "shiny", "rjson",
           "fuzzyjoin", "stringdist")

lapply(pckgs, require, character.only = TRUE)

setwd("C:/Users/bend/OneDrive - SportNZGroup/Documents/INTELLIGENCE/DATA PROJECTS/20200219 HPAD Results Tracking/hpad_results_tracking")
m1 <- read.delim("data/medallists/medallists_gold.txt", sep = ",", encoding = "UTF-8")
m2 <- read.delim("data/medallists/medallists_silver.txt", sep = ",", encoding = "UTF-8")
m3 <- read.delim("data/medallists/medallists_bronze.txt", sep = ",", encoding = "UTF-8")

# combine datasets
df <- rbind(m1, m2, m3) %>%
  mutate_all(as.character) %>%
  mutate(year = year(dmy(X.U.FEFF.Date))) %>%
  mutate(discipline = str_replace_all(PhaseName, "Cycling - Track ", "")) %>%
  mutate(discipline = sub(" -.*", "", discipline)) %>%
  mutate(teamflag = ifelse(Person.Team == Country | 
                             str_detect(discipline, "Team") == TRUE |
                             str_detect(Person.Team, "/") == TRUE,
                           TRUE, FALSE)) %>%
  # remove observations containing only country medals (keep team members)
  filter(Person.Team != Country) %>%
  mutate(Person.Name = sub("\\s*\\([^\\)]+\\)", "", Person.Team)) %>%
  select(year,
         Rank,
         discipline,
         Person.Name,
         teamflag)

# how many years back?
df <- df %>% filter(year > 1999)

# list of medallists - includess individuals from tandems 
medallists_ind <- unique(df$Person.Name[df$teamflag == FALSE])
medallists_all <- unique(df$Person.Name[str_detect(df$Person.Name, "/") == FALSE])



# -----------------------------
## RECONCILE RESULTS FOR EACH MEDALLIST
alldata <- read.delim("data/alldata.txt", sep = ",", encoding = "UTF-8")

df2 <- alldata %>%
  mutate_all(as.character) %>%
  mutate(Rank = as.numeric(Rank)) %>%
  mutate(year = year(dmy(X.U.FEFF.Date))) %>%
  mutate(discipline = str_replace_all(PhaseName, "Cycling - Track ", "")) %>%
  mutate(juniorsFlag = ifelse(str_detect(discipline, "Juniors") == TRUE,
                              TRUE, FALSE)) %>%
  mutate(worldsFlag = ifelse(str_detect(discipline, "World") == TRUE,
                              TRUE, FALSE)) %>%
  #mutate(competition = sub(".*- ", "", discipline)) %>%
  mutate(discipline = sub(" -.*", "", discipline)) %>%
  mutate(Person.Name = sub("\\s*\\([^\\)]+\\)", "", Person.Team))


# initially filter results to 2016
df2 <- df2 %>% filter(year < 2017)

# Focus on select df2 columns
df2 <- df2 %>%
  select(year,
         discipline,
         Rank,
         Person.Name,
         worldsFlag,
         juniorsFlag)

# Test first medallist results extraction
test <- medallists_ind[3]
demo <- df2 %>%
  filter(Person.Name == test) %>%
  arrange(discipline, year)


ggplot(demo, aes(year, Rank, )) + 
  geom_line(aes(colour = discipline)) + 
  geom_point(aes(colour = juniorsFlag)) + 
  ggtitle(test)
  