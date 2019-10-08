library(tidyverse)
install.packages("htmlwidgets")
getwd()
dir <-"/R_Projects/Thrive_mano/Projects" 
setwd(dir)
# ---Importing a datafile from dslabs

p <- system.file("extdata", package="dslabs")
list.files(p)

# -- getting fullpath
filename <- "murders.csv"
fullpath <- file.path(p,filename)
fullpath

#-- copying to working directory
file.copy(fullpath,getwd())
file.exists(filename)

#---read files
library(tidyverse)
library(readxl)
read_line("murders.csv",n_max=3)
'??readlines'
dat <- read.csv(fullpath,stringsAsFactors = FALSE)
class(dat$state)

#-----Download from Internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

#---test
library(tidyverse)
install.packages("dplyr")
library(dplyr)
urlt <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
datat <- read_csv(urlt,col_names = FALSE)
str(datat)

url <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_annmean_mlo.txt"
co2_mauna_loa <- read_table(url,col_names = TRUE,skip = 56)
head(co2_mauna_loa)
nrow(co2_mauna_loa %>% select(year, mean, unc))

#-reshaping test
d <- read.csv("E:/R_Projects/Thrive_mano/Data/time.csv",stringsAsFactors = FALSE)
tidy_data <- d %>%
  gather(year, time, `X2015`:`X2017`)
tidy_data %>% spread(year, time)
tidy_data %>% spread(time, year, `X2015`:`X2017`)
dis <- read.csv("E:/R_Projects/Thrive_mano/Data/disease.csv",stringsAsFactors = FALSE)
dat_tidy <- dis %>%
  gather(key = disease, value = count, HepatitisA:Rubella)

dis_m <- read.csv("E:/R_Projects/Thrive_mano/Data/time_merge.csv",stringsAsFactors = FALSE)
tidy_data_m <- dis_m %>%
  gather(key = 'key', value = 'value', -age_group) %>%
  separate(col = key, into = c('year', 'variable_name'), sep = '_') %>% 
  spread(key = variable_name, value = value)

dis_height <- read.csv("E:/R_Projects/Thrive_mano/Data/height.csv",stringsAsFactors = FALSE)
tidy_data <- dis_height %>%
separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge")%>%
spread(key = variable_name, value = value)

co2

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- co2_wide %>%
  gather(month,co2,-year)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants) %>%
  spread(gender, admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp

tmp2 <- unite(tmp, column_name, c(key, gender))

tmp2 %>%  spread(column_name, value)


tab1 <- data.frame(state = c('Alabama','Alaska','Arizona','Delaware','District of Columbia'), population = c(4779736,710231,6392017,897934,601723))

tab2 <- data.frame(state = c('Alabama','Alaska','Arizona','California','Colorado','Connecticut'), electoral_votes = c(9,3,11,55,9,7))

dim(tab1)

dim(tab2)
dat <- left_join(tab1, tab2, by = 'state')
dim(dat)
  dat <- semi_join(tab1, tab2, by = 'state')
  dat


  dat <- bind_cols(tab1,tab2)
  
  df1 <- data.frame(x = c('a','b'), y = c('a','a'))
  df2 <- data.frame(x = c('a','a'), y = c('a','b'))
  df1
  df2
  
  final <- intersect(df1, df2)
  final
  
  final <- union(df1, df2)
  final
  
  final <- setdiff(df1, df2)
  final
  
  final <- setequal(df1, df2)
  final
  
  install.packages("Lahman")
  library(Lahman)
  
  top <- Batting %>% 
    filter(yearID == 2016) %>%
    arrange(desc(HR)) %>%    # arrange by descending HR count
    slice(1:10)    # take entries 1-10
  top %>% as_tibble()
  
  Master %>% as_tibble()
  
  top_names <- top %>% left_join(Master) %>%
    select(playerID)
  
  Salaries
  
  top_salary <- Salaries %>% filter(yearID == 2016) %>%
    right_join(top_names)%>%
    select(nameFirst, nameLast, teamID, HR, salary)
  
  award_16 <- AwardsPlayers %>% filter(yearID == 2016) %>% select(playerID)
  top_names %>% left_join(award_16)
  
  intersect(top_names,award_16)
  setdiff(award_16,top_names)
  
  
  #--------Webscraping--------------
  library(rvest)
  url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
  h <- read_html(url)
  class(h)
  h
  tab <- h %>% html_nodes("table")
  html_text(tab[[20]])
  html_table(tab[[20]])
  html_table(tab[[length(tab)-1]])
  tab_1 <-html_table(tab[[10]])
  tab_1 <- tab_1[-1,]
  tab_1 <- tab_1[,-1]
  names(tab_1) <- c("Team", "Payroll", "Average")
  tab_2 <-html_table(tab[[19]])
  tab_2 <- tab_2[-1,]
  names(tab_2) <- c("Team", "Payroll", "Average")
  tab_1 %>% full_join(tab_2,by = 'Team')
  
  
  library(rvest)
  library(tidyverse)
  url <-"https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
  href <- read_html(url)
  ref <- href %>% html_nodes("table")
  html_table(ref[[6]],fill=TRUE)
  length(ref)
  
  head(dat)
  
  dot <- data.frame(month = c("January"), Sales = c("$128,568"), Profit = c("$16,234"))
  
  class(dot$Sales)
  dot %>% mutate_at(2:3, parse_number)
  dot %>% mutate_at(2:3, as.numeric)
  dot %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
    mutate_at(2:3, as.numeric)
  
  
  yes <- c("5", "6", "5'10", "5 feet", "4'11")
  no <- c("", ".", "Five", "six")
  s <- c(yes, no)
  pattern <- "\\d"
  # [56] means 5 or 6
  str_view(s, "[56]")
  # [4-7] means 4, 5, 6 or 7
  yes <- as.character(4:7)
  no <- as.character(1:3)
  s <- c(yes, no)
  str_detect(s, "[4-7]")
  # ^ means start of string, $ means end of string
  pattern <- "^\\d$"
  yes <- c("1", "5", "9")
  no <- c("12", "123", " 1", "a4", "b")
  s <- c(yes, no)
  str_view(s, pattern)
  
  pattern <- "^\\d{1,2}$"
  yes <- c("1", "5", "9", "12")
  no <- c("123", "a4", "b")
  str_view(c(yes, no), pattern)
  
  pattern <- "^[4-7]'\\d{1,2}\"$"
  yes <- c("5'7\"", "6'2\"",  "5'12\"")
  no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
  str_detect(yes, pattern)
  str_detect(no, pattern)
  
  
  pattern <- ","
  yes <- c("180 cm", "70 inches")
  no <- c("180", "70''")
  s <- c(yes, no)
  str_detect(s, "cm") | str_detect(s, "inches")
  str_detect(s, "cm|inches")
  # highlight the first occurrence of a pattern
  str_view(s, pattern)
  # highlight all instances of a pattern
  str_view_all(s, pattern)
  
  
  pattern_without_groups <- "^[4-7],\\d*$"
  pattern_with_groups <-  "^([4-7]),(\\d*)$"
  yes <- c("5,9", "5,11", "6,", "6,1")
  no <- c("5'9", ",", "2,8", "6.1.1")
  s <- c(yes, no)
  
  str_detect(s, pattern_without_groups)
  str_detect(s, pattern_with_groups)
  
  str_match(s, pattern_with_groups)
  str_extract(s, pattern_with_groups)
  
  
  not_inches <- function(x, smallest = 50, tallest = 84) {
    inches <- suppressWarnings(as.numeric(x))
    ind <- is.na(inches) | inches < smallest | inches > tallest 
    ind
  }
  
  not_inches <- function(x, smallest = 50, tallest = 84) {
    inches <- suppressWarnings(as.numeric(x))
    ind <- is.na(inches) | inches < smallest | inches > tallest 
    ind
  }
  
  s <- c("70","5 ft","4'11","",".","Six feet")
  pattern <- "\\d\\d|ft"
  str_view_all(s, pattern)
  
  animals <- c("cat", "puppy", "Moose", "MONKEY")
  pattern <- "[a-z]{4,5}"
  str_detect(animals, pattern)
  
  animals <- c("moose", "monkey", "meerkat", "mountain lion")
  pattern <- 'mo*'
  pattern <- 'mo?'
  pattern <- 'mo+'
  pattern <- 'moo*'
  str_detect(animals, pattern)
  
  schools <- c("U. Kentucky","Univ New Hampshire","Univ. of Massachusetts","University Georgia","U California","California State University")
  
  schools %>% 
    str_replace("Univ\\.?|U\\.?", "University ") %>% 
    str_replace("^University of |^University ", "University of ")
  
  schools %>% 
    str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
    str_replace("^University of |^University ", "University of ")
  
  problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
  pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
  pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
  str_replace(problems, pattern_with_groups, "\\1'\\2")
  
  yes <- c("5 feet 7inches", '5 7')
  no <- c("5ft 9 inches", "5 ft 9 inches")
  s <- c(yes, no)
  
  converted <- s %>% 
    str_replace("feet|foot|ft", "'") %>% 
    str_replace("inches|in|''|\"", "") %>% 
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

  
  pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
  str_detect(converted, pattern)
  