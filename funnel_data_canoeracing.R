## Establish a list of Games medallists using gracenote data
# Ben Day
# 26/03/2020

## INITIALISATION
# ---------------------
wd <- getwd()
pckgs <- c("stringr", "purrr", "repurrrsive", "lubridate", "readxl",
           "tidyverse", "ggplot2", "ggthemr", "shiny", "rjson",
           "fuzzyjoin", "stringdist")

lapply(pckgs, require, character.only = TRUE)
`%not_in%` <- purrr::negate(`%in%`) #function

# Import datasets
setwd("C:/Users/bend/OneDrive - SportNZGroup/Documents/INTELLIGENCE/DATA PROJECTS/20200219 HPAD Results Tracking/hpad_results_tracking")
df <- read.delim("data/canoe gracenote/medallists.csv", sep = ",", encoding = "UTF-8")
alldata <- read.delim("data/canoe gracenote/alldata.csv", sep = ",", encoding = "UTF-8")


#
## BEGIN TIDY
# -------------------
# Clean df
df <- df %>%
  mutate_all(as.character) %>%
  mutate(Rank = as.numeric(Rank),
         Year = as.numeric(Year)) %>%
  filter(Person.Team != Country,
         Person != "") %>%
  rename(teamFlag = "Team.Members") %>%
  #mutate(Year = Year(dmy(X.U.FEFF.Date))) %>%
  #mutate(Discipline = str_replace_all(PhaseName, "Cycling - Track ", "")) %>%
  #mutate(Discipline = sub(" -.*", "", Discipline)) %>%
  #mutate(teamFlag = ifelse(Person.Team == Country | str_detect(Discipline, "Team") == TRUE |
  #                           str_detect(Person.Team, "/") == TRUE, TRUE, FALSE)) %>%
  # remove observations containing only country medals (keep team members)
  #mutate(Person = sub("\\s*\\([^\\)]+\\)", "", Person.Team)) %>%
  select(Year,
         Rank,
         Discipline,
         Gender,
         Person,
         teamFlag)

# list of medallists - includess individuals from tandems 
medallists_ind <- unique(df$Person[df$teamFlag == "No"])
medallists_all <- unique(df$Person)
medallists_all <- medallists_all[medallists_all != ""]

# how many Years back?
#df <- df %>% filter(Year > 1999) ###################### many existing events made first appearance at 2000 Games



# Clean alldata
df2 <- alldata %>%
  mutate_all(as.character) %>%
  mutate(Rank = as.numeric(Rank),
         Year = as.numeric(Year)) %>%
  rename(teamFlag = "Team.Members") %>%
  mutate(juniorsFlag = ifelse(str_detect(CompetitionSet, "Juniors") == TRUE,
                              TRUE, FALSE)) %>%
  mutate(worldsFlag = ifelse(str_detect(Competition, "World") == TRUE,
                              TRUE, FALSE))
  #mutate(competition = sub(".*- ", "", Discipline)) %>%
  #mutate(Discipline = sub(" -.*", "", Discipline)) %>%
  #mutate(Person = sub("\\s*\\([^\\)]+\\)", "", Person.Team))


# Handle Years with Games and Worlds
olympic_Years <- unique(df2$Year[df2$worldsFlag == FALSE])
df2 <- df2 %>%
  mutate(pinnacleFlag = ifelse((Year %in% olympic_Years & 
                                 worldsFlag == FALSE) | 
                                 ((Year %not_in% olympic_Years) &
                                    worldsFlag == TRUE), TRUE, FALSE))

# Filter only Olympic disciplines (2020)
df2 <- df2 %>%
  filter(Discipline %in% c(
    "C1 200m", "C1 1000m",
    "C2 500m", "C2 1000m",
    "K1 200m", "K1 500m", "K1 1000m",
    "K2 500m", "K2 1000m",
    "K4 500m"
    ))

# Duplicate dataset for our athlete tracking (later)
df_all <- df2

# initially filter results to 2016
df2 <- df2 %>% filter(Year < 2017)
########################################################## UPDATE THIS POST TOKYO

# Focus on select df2 columns
df2 <- df2 %>%
  select(Year,
         Discipline,
         Gender,
         Class,
         Rank,
         Result,
         Person,
         Country,
         worldsFlag,
         juniorsFlag,
         pinnacleFlag)


# To determine relative Years before medal, need to separate all medal winning performances
separate_medals <- df2 %>%
  filter(worldsFlag == FALSE,
         Rank < 4,
         Person %in% medallists_all) %>%
  arrange(Person,
           Year,
           Discipline)

# Count medallists who also won junior worlds medals
x <- df2 %>%
  filter(juniorsFlag == TRUE,
         Rank < 4,
         Person %in% medallists_all)
medallists_junior <- unique(x$Person)


# Test first medallist results extraction
test <- "Lisa Carrington"
demo <- df2 %>%
  filter(Person == test,
         pinnacleFlag == TRUE) %>%
  arrange(Discipline, Year)

ggplot(demo, aes(Year, Rank)) + 
  geom_line(aes(colour = Discipline)) + 
  geom_point(data = subset(demo, juniorsFlag == TRUE), colour = "red") + 
  ggtitle(test)


# First medal for each Discipline
first_medal <- separate_medals %>%
  group_by(Person, Discipline) %>%   ############# group by only Person for first overall medal only
  filter(Year == min(Year))


# Calculate relative Years out from medal winning performance
df3 <- df2 %>%
  filter(Person %in% medallists_all) %>%
  group_by(Discipline) %>%
  mutate(Yearsout = Year)


# Ensure Year of performance < Year of medal, for each athlete & Discipline
funnel_data <- df3[1,] %>% mutate(Yearsout = 1)
funnel_data <- funnel_data[-1,]

for (i in 1:length(medallists_all)) {
  
  p <- medallists_all[i]
  a <- unique(first_medal[first_medal$Person == p, "Discipline"])
  
  for (j in 1:lengths(a)) {
    b <- as.numeric(first_medal[first_medal$Person == p & 
                               first_medal$Discipline == as.character(a[j,]), "Year"])
    temp <- df3 %>% 
      filter(Person == p,
             Discipline == as.character(a[j,]),
             Year < b) %>%
      mutate(Yearsout = b - Year) ############ Considering Years leading up to each Discipline medal separately (not just using first Games attendance)
    
    funnel_data <- rbind(funnel_data, temp)
    
  }
    
}











#
## NUMBER OF OBSERVATIONS
# ---------------------------
funnel_data <- funnel_data %>%
  filter(!is.na(Rank)) %>%
  arrange(Person)

# how many medallists
funnel_data %>% group_by(Person) %>% select(Person) %>% unique() #a
    # 283 medallists

# how many of these data are juniors comps
funnel_data %>% filter(juniorsFlag == TRUE) %>% group_by(Person) %>% select(Person) %>% unique() #b
    # 17 medallists
# but JWC only started in 2007... so not the right calculation



## CONSTRUCTING THE FUNNEL
## -----------------------
# To visualise the data let's count number of obs for each Yearsout/rank combo
funnel_obs <- funnel_data %>%
  group_by(Yearsout, Rank) %>%
  count()

# Plot spread of funnel data
ggplot(funnel_obs, aes(-Yearsout, Rank)) +
  geom_point(aes(size = n)) +
  #geom_line(aes(colour = Discipline)) + 
  #geom_point(data = subset(demo, juniorsFlag == TRUE), colour = "red") + 
  ggtitle("Canoe Sprint Funnel Dataset - all medallist pinnacle results leading up to medal") +
  ylab('Pinnacle Event Result') +
  xlab('Years out from Games')

# calculate median, percentiles
calcs <- funnel_data %>%
  group_by(Yearsout) %>%
  filter(Yearsout < 11) %>%    ##################### 12 medallists 10 Yearsout, 7 or fewer medallists 11 or more Yearsout
  summarise(q10 = quantile(Rank, .1),
            q25 = quantile(Rank, .25),
            median = median(Rank),
            q75 = quantile(Rank, .75),
            q90 = quantile(Rank, .9)) %>%
  gather(., `q10`, `q25`, `median`, `q75`, `q90`, key = "metric", value = "cases")

## Plot lines
ggplot(calcs) + geom_line(aes(x = -Yearsout, y = cases, colour = metric), linetype = "dashed") +
  #geom_line(aes(subset(measure == "median")), linetype = "dotted") +
  scale_x_continuous(breaks = seq(-10, 0, 1), limits = c(-10, 0)) +
  scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 20), sec.axis = dup_axis()) +
  ggtitle("Canoe Racing Funnel - Path to First Olympic Medal - All Disciplines") +
  ylab('Pinnacle Event Result') +
  xlab('Years out from Games')

# number medallists performances by Yearsout
counts <- funnel_data %>% 
  group_by(Yearsout) %>%
  filter(Yearsout < 11) %>%    ##################### 13 medallists 8 Yearsout, 4 or fewer medallists 9 or more Yearsout
  count() %>%
  filter(n > 4)  #################### only using Yearsout with 5 or more medallists

# number of medallist performances by gender, discipline
funnel_data %>% 
  group_by(Discipline, Gender) %>%
  filter(Yearsout < 11) %>%    ##################### 13 medallists 8 Yearsout, 4 or fewer medallists 9 or more Yearsout
  count(sort = TRUE)









## SEPARATE INTO DISCIPLINES
# ---------------------------
## Redo calcs with filtered data

funnel_lines <- function(x) {

  # number medallists by Yearsout
  counts <- funnel_data %>%
    filter(Discipline %in% x) %>%
    group_by(Yearsout) %>%
    filter(Yearsout < 11) %>%
    count() %>%
    filter(n > 4)  #################### only using Yearsout with 5 or more medallists
  
  # calculate spread of ranks
  calcs <- funnel_data %>% filter(Discipline %in% x) %>%
    group_by(Yearsout) %>%
    filter(Yearsout %in% counts$Yearsout) %>%
    summarise(q90 = quantile(Rank, .9),
              q75 = quantile(Rank, .75),
              median = median(Rank),
              q25 = quantile(Rank, .25),
              q10 = quantile(Rank, .1)
              ) %>%
    gather(., `q10`, `q25`, `median`, `q75`, `q90`, key = 'metric', value = "cases")
  
  # plot lines
  g <- ggplot(calcs) + geom_line(aes(x = -Yearsout, y = cases, colour = metric), linetype = "dashed") +
    scale_x_continuous(breaks = seq(-10, 0, 1), limits = c(-10, 0)) +
    scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 20), sec.axis = dup_axis()) +
    ggtitle("Canoe Racing Funnel - Path to First Olympic Medal") +
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


# Run function for each Discipline
funnel_lines("C1 200m")
funnel_lines("C1 1000m")
funnel_lines("C2 500m")
funnel_lines("C2 1000m")
funnel_lines("K1 200m")
funnel_lines("K1 500m")
funnel_lines("K1 1000m")
funnel_lines("K2 500m")
funnel_lines("K2 1000m")
funnel_lines("K4 500m")

# Group by boat
funnel_lines(c("K1 200m", "K1 500m", "K1 1000m"))
funnel_lines(c("K2 500m", "K2 1000m"))
funnel_lines(c("C1 200m", "C1 1000m"))
funnel_lines(c("C2 500m", "C2 1000m"))
funnel_lines("K4 500m")

# Group by distance
funnel_lines(c("C1 200m", "K1 200m"))
funnel_lines(c("C1 500m", "C2 500m", "K1 500m", "K2 500m", "K4 500m"))
funnel_lines(c("C1 1000m", "C2 1000m", "K1 1000m", "K2 1000m"))




######## SURPRISES
funnel_data %>% filter(Discipline == "C1 200m") %>% group_by(Person) %>% count()
funnel_data %>% filter(Discipline == "K1 200m") %>% group_by(Person) %>% count()
funnel_data %>% filter(Discipline == "K1 1000m") %>% group_by(Person) %>% count()


## JUNIOR PROGRESSIONS
# ----------------------
# From Junior to medal
funnel_data_junior <- funnel_data %>%
  filter(juniorsFlag == TRUE)

# Number unique World Juniors attendees
funnel_data_junior %>% select(Person) %>% unique() %>% lengths()
    # 19 medallists

ggplot(funnel_data_junior, aes(x = Yearsout, y = Rank)) + #geom_point() +
  geom_jitter(aes(colour = Discipline), width = 0.1, height = 0.1) +
  #facet_wrap(~ Discipline2) +   ######################## remove this for all Disciplines
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 25, 2), limits = c(0, 25)) +
  ggtitle("Years until first medal by World Juniors result") +
  ylab('World Juniors Result') +
  xlab('Years out from Olympic medal')















## OUR ATHLETES
# ----------------------
df_all <- df_all %>%
  select(Year,
         Discipline,
         Rank,
         Result,
         Person,
         Country,
         Gender,
         worldsFlag,
         juniorsFlag,
         pinnacleFlag)

nz <- df_all %>% filter(Country == "New Zealand",
                        Person != "New Zealand") %>%
  mutate(teamFlag = str_detect(Person, "/"))

nz_athletes <- nz %>% group_by(Person) %>%
  filter(teamFlag == FALSE) %>%
  count()

olympic_Year <- 2020

nz <- nz %>%
  filter(teamFlag == FALSE, ############## filtering out teams at this point
         pinnacleFlag == TRUE,
         !is.na(Rank)) %>% 
  mutate(Yearsout = olympic_Year - Year) %>%
  #       Discipline2 = str_replace(Discipline, " Men" , "")) %>%
  #mutate(Discipline2 = str_replace(Discipline2, " Women" , "")) %>%
  arrange(Person)

# Filter only recent results
nz <- nz %>% filter(Yearsout < 11)


















# Plot onto funnel
funnel <- function(athlete, x) {
  
  #if (is.null(y)) y <- x
  
  # number medallists by Yearsout
  counts <- funnel_data %>%
    filter(Discipline %in% x) %>%
    group_by(Yearsout) %>%
    filter(Yearsout < 11) %>%
    count() %>%
    filter(n > 4)  #################### only using Yearsout with 3 or more medallists
  
  # calculate spread of ranks
  calcs <- funnel_data %>% filter(Discipline %in% x) %>%
    group_by(Yearsout) %>%
    filter(Yearsout %in% counts$Yearsout) %>%
    summarise(q90 = quantile(Rank, .9),
              q75 = quantile(Rank, .75),
              median = median(Rank),
              q25 = quantile(Rank, .25),
              q10 = quantile(Rank, .1)) %>%
    gather(., `q10`, `q25`, `median`, `q75`, `q90`, key = 'metric', value = "cases")
  
  
  # results for input athlete
  results <- nz %>% filter(Person == athlete,
                           Discipline %in% x)
  
  # plot lines
  g <- ggplot() + geom_line(calcs, mapping = aes(x = -Yearsout, y = cases, colour = metric), linetype = "dashed") +
    geom_point(results, mapping = aes(x = -Yearsout, y = Rank, shape = as.factor(Discipline))) +
    geom_line(results, mapping = aes(x = -Yearsout, y = Rank, colour = as.factor(Discipline))) +
    
    scale_x_continuous(breaks = seq(-10, 0, 1), limits = c(-10, 0)) +
    scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 20), sec.axis = dup_axis()) +
    ggtitle("Canoe Racing Funnel - Path to First Olympic Medal") +
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

funnel("Lisa Carrington", c("K1 200m", "K1 500m"))
funnel("Kayla Imrie", c("K1 200m", "K4 500m"))








## EXPORT FUNNEL TABLE
# ------------------------
# Quick tidy of data before export

export_funnel_data <- funnel_data %>%
  ungroup %>%
  group_by(Yearsout, Discipline) %>%
  filter(Yearsout < 11,
         Person != "") %>%
  select(-Person,
         -Year,
         -Country,
         -Class) %>%
  mutate(Result = as.numeric(ifelse(str_detect(Result, ":"), 
                                    seconds(ms(Result)),
                                    Result)))  %>%
  mutate(Result = as.numeric(ifelse(str_detect(Result, "."), 
                                    seconds(Result),
                                    Result)))

# Rename columns fit for Power BI use
names(export_funnel_data) <- c("Funnel Disciplines",
                               "Gender",
                               "Pinnacle Result",
                               "Pinnacle Time",
                               "worldsFlag",
                               "juniorsFlag",
                               "pinnacleFlag",
                               "Years Out from Games")

# Write csv
write.csv(export_funnel_data, paste0(wd, "/data/canoe funnel data/funnel_data_", olympic_Year, ".csv"))



## EXPORT NZ ATHLETE DATA
# ------------------------
# Quick tidy of data before export
export_nz_data <- nz %>%
  filter(Person != "") %>%
  select(-Country) %>%
  mutate(Result = as.numeric(ifelse(str_detect(Result, ":"), 
                                    seconds(ms(Result)),
                                    Result)))  %>%
  mutate(Result = as.numeric(ifelse(str_detect(Result, "."), 
                                    seconds(Result),
                                    Result)))

# Rename columns fit for Power BI use
names(export_nz_data) <- c("Year",
                           "Discipline",
                               "Pinnacle Result",
                               "Pinnacle Time",
                               "Athlete",
                               "Gender",
                               "worldsFlag",
                               "juniorsFlag",
                               "pinnacleFlag",
                               "teamFlag",
                               "Years Out from Games")

# Write csv
write.csv(export_nz_data, paste0(wd, "/data/canoe funnel data/funnel_data_nzathletes.csv"))



#

