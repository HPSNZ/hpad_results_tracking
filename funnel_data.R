## Establish a list of Games medallists using gracenote data
# Ben Day
# 19/02/2020

## INITIALISATION
# ---------------------
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

rm(m1, m2, m3)

# how many years back?
#df <- df %>% filter(year > 1999) ###################### many existing events made first appearance at 2000 Games

# list of medallists - includess individuals from tandems 
medallists_ind <- unique(df$Person.Name[df$teamflag == FALSE])
medallists_all <- unique(df$Person.Name[str_detect(df$Person.Name, "/") == FALSE])


## RECONCILE RESULTS FOR EACH MEDALLIST
# -----------------------------
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

# Handle years with Games and Worlds
`%not_in%` <- purrr::negate(`%in%`)
olympic_years <- unique(df2$year[df2$worldsFlag == FALSE])
df2 <- df2 %>%
  mutate(pinnacleFlag = ifelse((year %in% olympic_years & 
                                 worldsFlag == FALSE) | 
                                 ((year %not_in% olympic_years) &
                                    worldsFlag == TRUE), TRUE, FALSE))

# Ignore constituent Omnium races
df2 <- df2 %>%
  filter(discipline %in% c(
    "Keirin Men", "Keirin Women",
    "Sprint Men", "Sprint Women",
    "Team Sprint Men", "Team Sprint Women",
    "Team Pursuit Men", "Team Pursuit Women",
    "Omnium Men", "Omnium Women",
    "Madison Men", "Madison Women"))

# Duplicate dataset for our athlete tracking (later)
df_all <- df2

# initially filter results to 2016
df2 <- df2 %>% filter(year < 2017)
########################################################## UPDATE THIS POST TOKYO

# Focus on select df2 columns
df2 <- df2 %>%
  select(year,
         discipline,
         Rank,
         Result,
         Person.Name,
         worldsFlag,
         juniorsFlag,
         pinnacleFlag)




# To determine relative years before medal, need to separate all medal winning performances
separate_medals <- df2 %>%
  filter(worldsFlag == FALSE,
         Rank < 4,
         Person.Name %in% medallists_all) %>%
  arrange(Person.Name,
           year,
           discipline)

# Count medallists who also won junior worlds medals
x <- df2 %>%
  filter(juniorsFlag == TRUE,
         Rank < 4,
         Person.Name %in% medallists_all)

medallists_junior <- unique(x$Person.Name)











# Test first medallist results extraction
test <- "Laura Kenny"
demo <- df2 %>%
  filter(Person.Name == test,
         pinnacleFlag == TRUE) %>%
  arrange(discipline, year)

ggplot(demo, aes(year, Rank, )) + 
  geom_line(aes(colour = discipline)) + 
  geom_point(data = subset(demo, juniorsFlag == TRUE), colour = "red") + 
  ggtitle(test)


# First medal for each discipline
first_medal <- separate_medals %>%
  group_by(Person.Name, discipline) %>%   ############# group by only Person.Name for first overall medal only
  filter(year == min(year))


# Calculate relative years out from medal winning performance
df3 <- df2 %>%
  filter(Person.Name %in% medallists_all) %>%
  group_by(discipline) %>%
  mutate(yearsout = year)



# Ensure year of performance < year of medal, for each athlete & discipline
funnel_data <- df3[1,] %>% mutate(yearsout = 1)
funnel_data <- funnel_data[-1,]

for (i in 1:length(medallists_all)) {
  
  p <- medallists_all[i]
  a <- unique(first_medal[first_medal$Person.Name == p, "discipline"])
  
  for (j in 1:lengths(a)) {
    b <- as.numeric(first_medal[first_medal$Person.Name == p & 
                               first_medal$discipline == as.character(a[j,]), "year"])
    # try(df3[df3$Person.Name == p & df3$discipline == as.character(a[j,]),] <-
    #       df3 %>% filter(Person.Name == p,
    #                  discipline == as.character(a[j,]),
    #                  year < as.character(first_medal[first_medal$Person.Name == p & 
    #                                     first_medal$discipline == as.character(a[j,]), "year"]))
    # )
    
    temp <- df3 %>% 
      filter(Person.Name == p,
             discipline == as.character(a[j,]),
             year < b) %>%
      mutate(yearsout = b - year) ############ Considering years leading up to each discipline medal separately (not just using first Games attendance)
    
    funnel_data <- rbind(funnel_data, temp)
    
  }
    
}

funnel_data <- funnel_data %>%
  filter(!is.na(Rank)) %>%
  arrange(Person.Name) %>%
  mutate(discipline2 = str_replace(discipline, " Men" , "")) %>%
  mutate(discipline2 = str_replace(discipline2, " Women" , "")) ################## Group men and women together


# how many medallists
funnel_data %>% select(Person.Name) %>% unique() #a
    # 83 medallists

# how many of these data are juniors comps
funnel_data %>% filter(juniorsFlag == TRUE) %>% select(Person.Name) %>% unique() #b
    # 30 medallists

# how many medallists have juniors experience?
    # ~36% since start of JWC 2006

# how many results are non-pinnacle (i.e. World champs in a Olympic year)
funnel_data %>% group_by(pinnacleFlag) %>% count(pinnacleFlag)
    # ~16%









## CONSTRUCTING THE FUNNEL
## -----------------------
## To visualise the data let's count number of obs for each yearsout/rank combo
funnel_obs <- funnel_data %>%
  group_by(yearsout, Rank) %>%
  count()

## Plot spread of funnel data
ggplot(funnel_obs, aes(-yearsout, Rank)) +
  geom_point(aes(size = n)) +
  #geom_line(aes(colour = discipline)) + 
  #geom_point(data = subset(demo, juniorsFlag == TRUE), colour = "red") + 
  ggtitle("Cycling Funnel Dataset - all medallist pinnacle results leading up to medal") +
  ylab('Pinnacle Event Result') +
  xlab('Years out from Games')

# calculate median, percentiles
calcs <- funnel_data %>%
  group_by(yearsout) %>%
  #filter(discipline2 != "Team Sprint" & discipline2 != "Team Pursuit") %>% ############ filter out disciplines
  filter(yearsout < 9) %>%    ##################### 13 medallists 8 yearsout, 4 or fewer medallists 9 or more yearsout
  summarise(q5 = quantile(Rank, .05),
            q25 = quantile(Rank, .25),
            median = median(Rank),
            q75 = quantile(Rank, .75),
            q95 = quantile(Rank, .95)) %>%
  gather(., `q5`, `q25`, `median`, `q75`, `q95`, key = "metric", value = "cases")

## Plot lines
ggplot(calcs) + geom_line(aes(x = -yearsout, y = cases, colour = metric), linetype = "dashed") +
  #geom_line(aes(subset(measure == "median")), linetype = "dotted") +
  scale_x_continuous(breaks = seq(-10, 0, 1), limits = c(-10, 0)) +
  ggtitle("Cycling Funnel - Path to First Olympic Medal - All Disciplines") +
  ylab('Pinnacle Event Result') +
  xlab('Years out from Games')

# number medallists by yearsout
counts <- funnel_data %>% 
  group_by(yearsout) %>%
  filter(yearsout < 9) %>%    ##################### 13 medallists 8 yearsout, 4 or fewer medallists 9 or more yearsout
  count() %>%
  filter(n > 4)  #################### only using yearsout with 5 or more medallists
counts





## SEPARATE INTO DISCIPLINES
# ---------------------------
## Redo calcs with filtered data

funnel_lines <- function(x) {
  
  #if (is.null(y)) y <- x
  
  # number medallists by yearsout
  counts <- funnel_data %>%
    filter(discipline2 %in% x) %>%
    group_by(yearsout) %>%
    filter(yearsout < 9) %>%
    count() %>%
    filter(n > 4)  #################### only using yearsout with 5 or more medallists
  
  # calculate spread of ranks
  calcs <- funnel_data %>% filter(discipline2 %in% x) %>%
    group_by(yearsout) %>%
    filter(yearsout %in% counts$yearsout) %>%
    summarise(q90 = quantile(Rank, .9),
              q75 = quantile(Rank, .75),
              median = median(Rank),
              q25 = quantile(Rank, .25),
              q10 = quantile(Rank, .1)
              ) %>%
    gather(., `q10`, `q25`, `median`, `q75`, `q90`, key = 'metric', value = "cases")
  
  # plot lines
  g <- ggplot(calcs) + geom_line(aes(x = -yearsout, y = cases, colour = metric), linetype = "dashed") +
    
    scale_x_continuous(breaks = seq(-10, 0, 1), limits = c(-10, 0)) +
    scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 20), sec.axis = dup_axis()) +
    ggtitle("Cycling Funnel - Path to First Olympic Medal") +
    labs(subtitle = paste(x, collapse = ', ')) +
    ylab('Pinnacle Event Result') +
    xlab('Years out from Games')
  
  # set up outputs
  returnlist <- list()
  returnlist$first <- counts
  returnlist$second <- g
  #returnlist$third <- calcs
  return(returnlist)
  
}


## Run function for each discipline
funnel_lines(c("Keirin", "Sprint", "Omnium", "Madison"))
funnel_lines(c("Team Pursuit", "Team Sprint"))
funnel_lines(c())

funnel_lines("Omnium")
funnel_lines("Madison")
funnel_lines("Sprint")



## JUNIOR PROGRESSIONS
# ----------------------
# From Junior to medal
funnel_data_junior <- funnel_data %>%
  filter(juniorsFlag == TRUE)

# Number unique World Juniors attendees
funnel_data_junior %>% select(Person.Name) %>% unique() %>% lengths()
    # 30 medallists

ggplot(funnel_data_junior, aes(x = yearsout, y = Rank)) + #geom_point() +
  geom_jitter(aes(colour = discipline2), width = 0.1, height = 0.1) +
  #facet_wrap(~ discipline2) +   ######################## remove this for all disciplines
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 25, 2), limits = c(0, 25)) +
  ggtitle("Years until first medal by World Juniors result") +
  ylab('World Juniors Result') +
  xlab('Years out from Olympic medal')















## OUR ATHLETES
# ----------------------
df_all <- df_all %>%
  select(year,
         discipline,
         Rank,
         Result,
         Person.Name,
         Country,
         worldsFlag,
         juniorsFlag,
         pinnacleFlag)

nz <- df_all %>% filter(Country == "New Zealand",
                        Person.Name != "New Zealand") %>%
  mutate(teamFlag = str_detect(Person.Name, "/"))

nz_athletes <- nz %>% group_by(Person.Name) %>%
  filter(teamFlag == FALSE) %>%
  count()

olympic_year <- 2020

nz <- nz %>%
  filter(teamFlag == FALSE, ############## filtering out teams at this point
         pinnacleFlag == TRUE,
         !is.na(Rank)) %>% 
  mutate(yearsout = olympic_year - year,
         discipline2 = str_replace(discipline, " Men" , "")) %>%
  mutate(discipline2 = str_replace(discipline2, " Women" , "")) %>%
  arrange(Person.Name)

# Filter only recent results
nz <- nz %>% filter(yearsout < 9)

# Plot onto funnel
funnel <- function(athlete, x) {
  
  #if (is.null(y)) y <- x
  
  # number medallists by yearsout
  counts <- funnel_data %>%
    filter(discipline2 %in% x) %>%
    group_by(yearsout) %>%
    filter(yearsout < 9) %>%
    count()
    #filter(n > 4)  #################### only using yearsout with 3 or more medallists
  
  # calculate spread of ranks
  calcs <- funnel_data %>% filter(discipline2 %in% x) %>%
    group_by(yearsout) %>%
    filter(yearsout %in% counts$yearsout) %>%
    summarise(q90 = quantile(Rank, .9),
              q75 = quantile(Rank, .75),
              median = median(Rank),
              q25 = quantile(Rank, .25),
              q10 = quantile(Rank, .1)) %>%
    gather(., `q10`, `q25`, `median`, `q75`, `q90`, key = 'metric', value = "cases")
  
  
  # results for input athlete
  results <- nz %>% filter(Person.Name == athlete,
                           discipline2 %in% x)
  
  # plot lines
  g <- ggplot() + geom_line(calcs, mapping = aes(x = -yearsout, y = cases, colour = metric), linetype = "dashed") +
    geom_point(results, mapping = aes(x = -yearsout, y = Rank, shape = as.factor(discipline2))) +
    geom_line(results, mapping = aes(x = -yearsout, y = Rank, colour = as.factor(discipline2))) +
    
    scale_x_continuous(breaks = seq(-10, 0, 1), limits = c(-10, 0)) +
    scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 20), sec.axis = dup_axis()) +
    ggtitle("Cycling Funnel - Path to First Olympic Medal") +
    labs(subtitle = paste(x, collapse = ', ')) +
    ylab('Pinnacle Event Result') +
    xlab('Years out from Games') +
    labs(colour = "Percentile", shape = "Discipline")
  
  # set up outputs
  returnlist <- list()
  returnlist$first <- counts
  returnlist$second <- g
  #returnlist$third <- calcs
  return(returnlist)
  
}

funnel("Sam Webster", c("Keirin", "Sprint", "Omnium", "Madison"))
funnel("Sam Webster", c("Team Pursuit", "Team Sprint"))

funnel("Edward Dawkins", c("Keirin", "Sprint", "Omnium", "Madison"))
funnel("Edward Dawkins", c("Team Pursuit", "Team Sprint"))

funnel("Natasha Hansen", c("Keirin", "Sprint", "Omnium", "Madison"))
funnel("Natasha Hansen", c("Team Pursuit", "Team Sprint"))







## EXPORT FUNNEL TABLE
# ------------------------
# Quick tidy of data before export

export_funnel_data <- funnel_data %>%
  ungroup %>%
  mutate(discipline = str_replace(discipline, " Men" , "")) %>%
  mutate(discipline = str_replace(discipline, " Women" , "")) %>%
  select(-discipline2) %>%
  group_by(yearsout, discipline) %>%
  filter(yearsout < 9) %>%
  select(-Person.Name,
         -year) %>%
  mutate(Result = as.numeric(ifelse(str_detect(Result, ":"), 
                                    seconds(ms(Result)),
                                    Result)))  %>%
  mutate(Result = as.numeric(ifelse(str_detect(Result, "."), 
                                    seconds(Result),
                                    Result)))

# Rename columns fit for Power BI use
names(export_funnel_data) <- c("Funnel Disciplines",
                               "Pinnacle Result",
                               "Pinnacle Time",
                               "worldsFlag",
                               "juniorsFlag",
                               "pinnacleFlag",
                               "Years Out from Games")

# Write csv
write.csv(export_funnel_data, paste0("funnel_data_", olympic_year, ".csv"))



## EXPORT NZ ATHLETE DATA
# ------------------------
# Quick tidy of data before export
export_nz_data <- nz %>%
  select(-Country,
         -discipline) %>%
  mutate(Result = as.numeric(ifelse(str_detect(Result, ":"), 
                                    seconds(ms(Result)),
                                    Result)))  %>%
  mutate(Result = as.numeric(ifelse(str_detect(Result, "."), 
                                    seconds(Result),
                                    Result)))

# Rename columns fit for Power BI use
names(export_nz_data) <- c("Year",
                               "Pinnacle Result",
                               "Pinnacle Time",
                               "Athlete",
                               "worldsFlag",
                               "juniorsFlag",
                               "pinnacleFlag",
                               "teamFlag",
                               "Years Out from Games",
                               "Discipline")

# Write csv
write.csv(export_nz_data, "funnel_data_nzathletes.csv")



#

