library(tidyverse)
library(ggplot2)

sc_15 <- read.csv("R_Baseball/Final_Proj/sc_framing_15.csv")
sc_16 <- read.csv("R_Baseball/Final_Proj/sc_framing_16.csv")
sc_17 <- read.csv("R_Baseball/Final_Proj/sc_framing_17.csv")
sc_18 <- read.csv("R_Baseball/Final_Proj/sc_framing_18.csv")
bp_15 <- read.csv("R_Baseball/Final_Proj/bp_framing_15.csv")
bp_16 <- read.csv("R_Baseball/Final_Proj/bp_framing_16.csv")
bp_17 <- read.csv("R_Baseball/Final_Proj/bp_framing_17.csv")
bp_18 <- read.csv("R_Baseball/Final_Proj/bp_framing_18.csv")
fg_15 <- read.csv("R_Baseball/Final_Proj/fg_framing_15.csv")
fg_16 <- read.csv("R_Baseball/Final_Proj/fg_framing_16.csv")
fg_17 <- read.csv("R_Baseball/Final_Proj/fg_framing_17.csv")
fg_18 <- read.csv("R_Baseball/Final_Proj/fg_framing_18.csv")


## Statcast Datasets--------------------------------------------------------

# Combine first and last name into NAME column
# Remove irrelevenat columns from the datasets
sc_15 <- sc_15 %>%
  mutate(NAME = paste(first_name, last_name),
         sc_runs = runs_extra_strikes) %>%
  select(NAME, sc_runs)

sc_16 <- sc_16 %>%
  mutate(NAME = paste(first_name, last_name),
         sc_runs = runs_extra_strikes) %>%
  select(NAME, sc_runs)

sc_17 <- sc_17 %>%
  mutate(NAME = paste(first_name, last_name),
         sc_runs = runs_extra_strikes) %>%
  select(NAME, sc_runs)

sc_18 <- sc_18 %>%
  mutate(NAME = paste(first_name, last_name),
         sc_runs = runs_extra_strikes) %>%
  select(NAME, sc_runs)

# Remove empty space in front of NAME
sc_15$NAME <- substring(sc_15$NAME, 2)
sc_16$NAME <- substring(sc_16$NAME, 2)
sc_17$NAME <- substring(sc_17$NAME, 2)
sc_18$NAME <- substring(sc_18$NAME, 2)


# Join tables for year-to-year correlations
sc_yty1 <- inner_join(sc_15, sc_16, by = 'NAME')
sc_yty2 <- inner_join(sc_16, sc_17, by = 'NAME')
sc_yty3 <- inner_join(sc_17, sc_18, by = 'NAME')

# Combine year-to-year tables and rename columns
sc_yty <- rbind(sc_yty1, sc_yty2, sc_yty3)
names(sc_yty) <- c('NAME', 'runs_year1', 'runs_year2')

# Produce scatter plot and regression line
ggplot(sc_yty, aes(x = runs_year1, y = runs_year2)) +
  geom_point() + geom_smooth(method = lm) +
  xlim(-23, 32) + ylim(-23, 32) +
  xlab("Year 1 Framing Runs") + ylab("Year 2 Framing Runs") +
  ggtitle("Statcast Framing Runs Year-to-Year Correlation")

# Calculate correlation coefficient
sc_yty %>% summarize(correlation = cor(runs_year1, runs_year2))


## Baseball Prospectus Datasets---------------------------------------------

# Remove four catchers who did not meet SC qualifier
bp_15 <- bp_15[-c(54, 56, 57, 59),]

# Convert NAME column from interger to character
bp_15$NAME <- as.character(bp_15$NAME)
bp_16$NAME <- as.character(bp_16$NAME)
bp_17$NAME <- as.character(bp_17$NAME)
bp_18$NAME <- as.character(bp_18$NAME)

# Change Casali's first name from Curtis to Curt
bp_16$NAME[30] <- "Curt Casali"

# Remove irrelevenat columns from the datasets
bp_15 <- bp_15 %>% mutate(bp_runs = Framing.Runs) %>%
  select(NAME, bp_runs)
bp_16 <- bp_16 %>% mutate(bp_runs = Framing.Runs) %>%
  select(NAME, bp_runs)
bp_17 <- bp_17 %>% mutate(bp_runs = Framing.Runs) %>%
  select(NAME, bp_runs)
bp_18 <- bp_18 %>% mutate(bp_runs = Framing.Runs) %>%
  select(NAME, bp_runs)


# Join tables for year-to-year correlations
bp_yty1 <- inner_join(bp_15, bp_16, by = 'NAME')
bp_yty2 <- inner_join(bp_16, bp_17, by = 'NAME')
bp_yty3 <- inner_join(bp_17, bp_18, by = 'NAME')

# Combine year-to-year tables and rename columns
bp_yty <- rbind(bp_yty1, bp_yty2, bp_yty3)
names(bp_yty) <- c('NAME', 'runs_year1', 'runs_year2')

# Produce scatter plot and regression line
ggplot(bp_yty, aes(x = runs_year1, y = runs_year2)) +
  geom_point() + geom_smooth(method = lm) +
  xlim(-23, 32) + ylim(-23, 32) +
  xlab("Year 1 Framing Runs") + ylab("Year 2 Framing Runs") +
  ggtitle("Baseball Prospectus Framing Runs Year-to-Year Correlation")

# Calculate correlation coefficient
bp_yty %>% summarize(correlation = cor(runs_year1, runs_year2))


## FanGraphs Datasets-------------------------------------------------------

# Remove catchers who did not meet SC qualifier
fg_15 <- fg_15[-c(35, 37, 40, 45, 46, 47, 49),]
fg_16 <- fg_16[-c(29, 48, 49),]
fg_17 <- fg_17[-c(62),]
fg_18 <- fg_18[-c(37, 39, 44, 51),]

# Remove irrelevenat columns from the datasets
fg_15 <- fg_15 %>%
  mutate(NAME = Name, fg_runs = FRM) %>%
  select(NAME, fg_runs)
fg_16 <- fg_16 %>%
  mutate(NAME = Name, fg_runs = FRM) %>%
  select(NAME, fg_runs)
fg_17 <- fg_17 %>%
  mutate(NAME = Name, fg_runs = FRM) %>%
  select(NAME, fg_runs)
fg_18 <- fg_18 %>%
  mutate(NAME = Name, fg_runs = FRM) %>%
  select(NAME, fg_runs)

# Convert Name column from interger to character
fg_15$NAME <- as.character(fg_15$NAME)
fg_16$NAME <- as.character(fg_16$NAME)
fg_17$NAME <- as.character(fg_17$NAME)
fg_18$NAME <- as.character(fg_18$NAME)


# Join tables for year-to-year correlations
fg_yty1 <- inner_join(fg_15, fg_16, by = 'NAME')
fg_yty2 <- inner_join(fg_16, fg_17, by = 'NAME')
fg_yty3 <- inner_join(fg_17, fg_18, by = 'NAME')

# Combine year-to-year tables and rename columns
fg_yty <- rbind(fg_yty1, fg_yty2, fg_yty3)
names(fg_yty) <- c('NAME', 'runs_year1', 'runs_year2')

# Produce scatter plot and regression line
ggplot(fg_yty, aes(x = runs_year1, y = runs_year2)) +
  geom_point() + geom_smooth(method = lm) +
  xlim(-23, 32) + ylim(-23, 32) +
  xlab("Year 1 Framing Runs") + ylab("Year 2 Framing Runs") +
  ggtitle("FanGraphs Framing Runs Year-to-Year Correlation")

# Calculate correlation coefficient
fg_yty %>% summarize(correlation = cor(runs_year1, runs_year2))


## Correlation Analysis-----------------------------------------------------

# Join framing runs of all three sources by year
framing_15 <- list(sc_15, bp_15, fg_15) %>%
  reduce(inner_join, by = 'NAME')
framing_16 <- list(sc_16, bp_16, fg_16) %>%
  reduce(inner_join, by = 'NAME')
framing_17 <- list(sc_17, bp_17, fg_17) %>%
  reduce(inner_join, by = 'NAME')
framing_18 <- list(sc_18, bp_18, fg_18) %>%
  reduce(inner_join, by = 'NAME')

# Combine all four years
framing_all <- rbind(framing_15, framing_16, framing_17, framing_18)


# Function to produce scatter plot
graph <- function(year, site1, site2) {
  
  site1 = paste(site1, '_runs', sep = "")
  site2 = paste(site2, '_runs', sep = "")
  
  if (site1 == 'sc_runs') {source1 = 'Statcast'}
  if (site1 == 'bp_runs') {source1 = 'Baseball Prospectus'}
  if (site1 == 'fg_runs') {source1 = 'FanGraphs'}
  
  if (site2 == 'sc_runs') {source2 = 'Statcast'}
  if (site2 == 'bp_runs') {source2 = 'Baseball Prospectus'}
  if (site2 == 'fg_runs') {source2 = 'FanGraphs'}
  
  site1 = year[, site1]
  site2 = year[, site2]
  df = deparse(substitute(year))
  season = paste('20', substr(df, 9, 10), sep = "")
  
  if (season == '20al') {season = '2015~2018'}
  
  ggplot(year, aes(x = site1, y = site2)) +
    geom_point() + geom_smooth(method = lm) +
    xlim(-20, 40) + ylim(-25, 40) +
    xlab(paste(source1, 'Framing Runs')) +
    ylab(paste(source2, 'Framing Runs')) +
    ggtitle(paste(source1, 'vs.', source2, 'Framing Runs,', season))
}

graph(framing_15, 'sc', 'bp')
graph(framing_16, 'sc', 'bp')
graph(framing_17, 'sc', 'bp')
graph(framing_18, 'sc', 'bp')
graph(framing_all, 'sc', 'bp')

graph(framing_15, 'sc', 'fg')
graph(framing_16, 'sc', 'fg')
graph(framing_17, 'sc', 'fg')
graph(framing_18, 'sc', 'fg')
graph(framing_all, 'sc', 'fg')

graph(framing_15, 'bp', 'fg')
graph(framing_16, 'bp', 'fg')
graph(framing_17, 'bp', 'fg')
graph(framing_18, 'bp', 'fg')
graph(framing_all, 'bp', 'fg')


framing_15 %>% summarize(correlation = cor(sc_runs, bp_runs))
framing_15 %>% summarize(correlation = cor(sc_runs, fg_runs))
framing_15 %>% summarize(correlation = cor(fg_runs, bp_runs))

framing_16 %>% summarize(correlation = cor(sc_runs, bp_runs))
framing_16 %>% summarize(correlation = cor(sc_runs, fg_runs))
framing_16 %>% summarize(correlation = cor(fg_runs, bp_runs))

framing_17 %>% summarize(correlation = cor(sc_runs, bp_runs))
framing_17 %>% summarize(correlation = cor(sc_runs, fg_runs))
framing_17 %>% summarize(correlation = cor(fg_runs, bp_runs))

framing_18 %>% summarize(correlation = cor(sc_runs, bp_runs))
framing_18 %>% summarize(correlation = cor(sc_runs, fg_runs))
framing_18 %>% summarize(correlation = cor(fg_runs, bp_runs))

framing_all %>% summarize(correlation = cor(sc_runs, bp_runs))
framing_all %>% summarize(correlation = cor(sc_runs, fg_runs))
framing_all %>% summarize(correlation = cor(fg_runs, bp_runs))
