library(tidyverse)
library(dslabs)
library(broom)
install.packages("htmlwidgets")
install.packages("textdata")
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


data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany") & !is.na(fertility)) %>%
  select(country, year, fertility)

new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)
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
  data(gapminder)
  mute <- gapminder %>% filter(region=="Caribbean") %>%
    mutate(country = recode(country, 
                            'Antigua and Barbuda'="Barbuda",
                            'Dominican Republic' = "DR",
                            'St. Vincent and the Grenadines' = "St. Vincent",
                            'Trinidad and Tobago' = "Trinidad")) %>%
    ggplot(aes(year, life_expectancy, color = country)) +
    geom_line()
  
    schedule <- data.frame(day = c('Monday', 'Tuesday'), staff = c('Mandy, Chris and Laura','Steve, Ruth and Frank'))
  
  str_split(schedule$staff, ", | and ")
  str_split(schedule$staff, ",\\s|\\sand\\s")
  
  
  tidy <- schedule %>% 
    mutate(staff = str_split(staff, ", | and ")) %>% 
    unnest()
  
  tidy <- schedule %>% 
    mutate(staff = str_split(staff, ", | and ", simplify = TRUE)) %>% 
    unnest()
  
  library(rvest)
  library(tidyverse)
  library(stringr)
  library(lubridate)
  url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
  tab <- read_html(url) %>% html_nodes("table")
  polls <- tab[[7]] %>% html_table(fill = TRUE)
  str(polls)
  polls <- polls[-1,]
  head(polls)
  polls[[1]]
  names(polls) <- c("dates", "remain", "leave", "undecided",  "samplesize", "pollster","notes", "poll_type", "lead")
  str_detect(polls$undecided, 'NA')
  polls %>% filter (str_detect(polls$remain, "%"))
  parse_number(polls$remain)/100
  
  temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
  end_date <- sapply(temp, function(x) x[length(x)])
  
  dates <- c("09-01-02", "01-12-07", "02-03-04")
  #ymd(dates)
  #mdy(dates)
  dmy(dates)
  
  library(dslabs)
  data(brexit_polls)
  head(brexit_polls)
  str(brexit_polls$startdate)
  brexit_polls %>% filter(month(brexit_polls$startdate) ==4)
  
  brexit_polls %>% round_date(brexit_polls$enddate,unit = "week",week_start = getOption("lubridate.week.start",7))
  
  nrow(brexit_polls %>% mutate( rounded =round_date(ymd(brexit_polls$enddate), "week")) %>% filter (rounded < ymd("2016-06-13")))
  
  sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12")
  
  brexit_polls %>% mutate(weekday = weekdays(brexit_polls$enddate, abbreviate = FALSE)) %>% filter(weekday =="Thursday")
  
  data(movielens)
  
  table(year(as_datetime(movielens$timestamp)))
  
  table(hour(as_datetime(movielens$timestamp)))  
  
  install.packages("gutenbergr")
  install.packages("tidytext")
  library(tidyverse)
  library(gutenbergr)
  library(tidytext)
  options(digits = 3)
  
  head(gutenberg_metadata)
  str(gutenberg_metadata)
  gutenberg_metadata %>% filter (str_detect(gutenberg_metadata$title, 'Pride and Prejudice')) %>% select (gutenberg_id,title)
  gutenberg_works(title == 'Pride and Prejudice') %>% select (gutenberg_id,title)
  
  
    words <- gutenberg_download(1342) %>%
    unnest_tokens(word, text)
    
    check <- words %>% anti_join(stop_words)
     words %>% stop_words(1342)
  
     words %>%
       count(word) %>%
       top_n(1, n) %>%
       pull(n)
     
  word_counts <-data.frame(words %>%
    anti_join(stop_words, by = "word") %>%
    count(word, sort = TRUE))
  
  afinn <- get_sentiments("afinn")
  
  afinn_sentiments <- word_counts%>%
    inner_join(afinn)
  
  nrow(afinn_sentiments %>% filter(value ==4))
  
  
  #---------------
  book <- gutenberg_download(1342)
  words <- book %>%
    unnest_tokens(word, text)
  
  words <- words %>% anti_join(stop_words)
  
  words <- words %>%
    filter(!str_detect(word, "\\d"))
  
  afinn <- get_sentiments("afinn")
  afinn_sentiments <- words %>% inner_join(afinn, by = "word")
  nrow(afinn_sentiments)
  nrow(afinn_sentiments %>% filter(str_detect(afinn_sentiments$value, "\\-")))
  nrow(afinn_sentiments %>% filter(value < 0))
  mean(afinn_sentiments$value > 0)
  
  
  
  #-------Final assessment in Wrangling------
  install.packages("pdftools")
  library(tidyverse)
  library(pdftools)
  options(digits = 3)
  fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
  system("cmd.exe", input = paste("start", fn))
  
  #1me11
  #865404030660581
  #865404030660599
  
  library(pdftools)
  pdf_file <- "https://github.com/ropensci/tabulizer/raw/master/inst/examples/data.pdf"
  txt <- pdf_text(fn)
  str(x)
  cat(txt[9])
  x <- str_split(txt[9],'\n')
  length(x)
  
  s <- x[[1]]
  class(s)
  
  s <- str_trim(s, "both")
  
 
  header_index <- str_which(s, "2015")
  
  header_index <- str_which(s, "2015")[1]
  header <- s[header_index]
  str_split(header,'\\s{2,}',simplify=TRUE)
  
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  month
  
  tail_index  <- str_which(s, "Total")
  tail_index
  
  n <- str_count(s,"\\d+")
  str_which(n, 1)
  
  
  s <- s[-(header_index-1)]
  
  n <- str_count(s,"\\d+")
  
  s[1]
  s[7]
  
  s <- s[-5]
  s <- s[-7]
  
  str(s)
  
  out <- c(1:header_index, which(n==1), tail_index:length(s))
  s <- s[-out]
  length(s)
  
  s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
  colnames(s) <- c('day', header)
  class(s)
  
  tab <- s %>% 
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate_all(as.numeric)
  mean(tab$"2015")
  
  tab <- tab %>% gather(year, deaths, -day) %>%
    mutate(deaths = as.numeric(deaths))
  tab
  
  tab %>% filter(!(year %in% (2018)) ) %>% ggplot(aes(day, deaths, color=year)) + 
          geom_line() + ylim(0, 110)
  
  
  tab %>% filter(year < 2018) %>% 
    ggplot(aes(day, deaths, color = year)) +
    geom_line() +
    geom_vline(xintercept = 20) +
    geom_point() + ylim(0, 110)
  
  
  #------Linear Regression
  
  library(Lahman)
  library(tidyverse)
  library(dslabs)
  Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
    ggplot(aes(AB_per_game, R_per_game)) + 
    geom_point(alpha = 0.5)
  
  Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(X_per_game = X3B / G, Derror_per_game = X2B / G) %>%
    ggplot(aes(X_per_game, Derror_per_game)) + 
    geom_point(alpha = 0.5)
  
  library(tidyverse)
  install.packages('HistData')
  library(HistData)
  data("GaltonFamilies")
  data(galton_heights)
  
  set.seed(1983)
  galton_heights <- GaltonFamilies %>%
    filter(gender == "male") %>%
    group_by(family) %>%
    sample_n(1) %>%
    ungroup() %>%
    select(father, childHeight) %>%
    rename(son = childHeight)
  
  # means and standard deviations
  galton_heights %>%
    summarize(mean(father), sd(father), mean(son), sd(son))
  # scatterplot of father and son heights
  galton_heights %>%
    ggplot(aes(father, son)) +
    geom_point(alpha = 0.5)
  
  B <- 1000
  N <- 25
  R <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>%
      summarize(r = cor(father, son)) %>%
      pull(r)
  })
  qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
  
  # expected value and standard error
  mean(R)
  sd(R)
  
  B <- 1000
  N <- 25
  R <- replicate(B, {
    sample_n(Teams %>% filter(yearID %in% 1961:2001), N, replace = TRUE) %>%
      summarize(r = cor(AB / G, R / G)) %>%
      pull(r)
  })
  qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
  
  # expected value and standard error
  mean(R)
  sd(R)
  
  
  Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
  cor(Teams_small$X2B/Teams_small$G, Teams_small$X3B/Teams_small$G)
  
  set.seed(1989) #if you are using R 3.5 or earlier
  set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
  library(HistData)
  data("GaltonFamilies")
  
  female_heights <- GaltonFamilies%>%    
    filter(gender == "female") %>%    
    group_by(family) %>%    
    sample_n(1) %>%    
    ungroup() %>%    
    select(mother, childHeight) %>%    
    rename(daughter = childHeight)
  
  mu_x <- mean(female_heights$mother)
  mu_y <- mean(female_heights$daughter)
  s_x <- sd(female_heights$mother)
  s_y <- sd(female_heights$daughter)
  r <- cor(female_heights$mother, female_heights$daughter)
  m_1 <-  r * s_y / s_x
  b_1 <- mu_y - m_1*mu_x
  (m_1*60)+b_1
  
  
  galton_heights %>%
    mutate(z_father = round((father - mean(father)) / sd(father))) %>%
    filter(z_father %in% -2:2) %>%
    ggplot() +  
    stat_qq(aes(sample = son)) +
    facet_wrap( ~ z_father)
  
  B <- 1000
  N <- 50
  lse <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>%
      mutate(father = father - mean(father)) %>%
      lm(son ~ father, data = .) %>% .$coef 
  })
  
  cor(lse[1,], lse[2,]) 
  
  
  #----Linear Regression
  
  set.seed(1983)
  galton_heights <- GaltonFamilies %>%
    filter(gender == "male") %>%
    group_by(family) %>%
    sample_n(1) %>%
    ungroup() %>%
    select(father, childHeight) %>%
    rename(son = childHeight)
  rss <- function(beta0, beta1, data){
    resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
    return(sum(resid^2))
  }
  
  # plot RSS as a function of beta1 when beta0=25
  beta1 = seq(0, 1, len=nrow(galton_heights))
  results <- data.frame(beta1 = beta1,
                        rss = sapply(beta1, rss, beta0 = 25))
  results %>% ggplot(aes(beta1, rss)) + geom_line() + 
    geom_line(aes(beta1, rss))
  
  beta1 = seq(0, 1, len=nrow(galton_heights))
  results <- data.frame(beta1 = beta1,
                        rss = sapply(beta1, rss, beta0 = 36))
  results %>% ggplot(aes(beta1, rss)) + geom_line() + 
    geom_line(aes(beta1, rss), col=2)
  
  
  Teams <- Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(BB_per_game = BB / G, R_per_game = R / G, HR_per_game = HR / G)
    
  lm (R_per_game~BB_per_game+HR_per_game,data=Teams)%>% .$coef
  
  B <- 1000
  N <- 100
  lse <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>% 
      lm(son ~ father, data = .) %>% .$coef 
  })
  
  lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
  
  set.seed(1989) #if you are using R 3.5 or earlier
  set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
  library(HistData)
  data("GaltonFamilies")
  options(digits = 3)    # report 3 significant digits
  female_heights <- GaltonFamilies %>%     
    filter(gender == "female") %>%     
    group_by(family) %>%     
    sample_n(1) %>%     
    ungroup() %>%     
    select(mother, childHeight) %>%     
    rename(daughter = childHeight) 
  
  female_heights %>% lm(mother~ daughter, data=.)
  
  rss <- function(beta0, beta1, data){
    resid <- female_heights$mother - (beta0+beta1*female_heights$daughter)
    return(sum(resid^2))
  }
  
  # plot RSS as a function of beta1 when beta0=25
  beta1 = seq(0, 1, len=nrow(female_heights))
  results <- data.frame(beta1 = beta1,
                        rss = sapply(beta1, rss, beta0 = 25))
  results %>% ggplot(aes(beta1, rss)) + geom_line() + 
    geom_line(aes(beta1, rss))
  #------------------
  model <- lm(mother ~ daughter, data = female_heights)
  predictions <- predict(model, interval = c("confidence"), level = 0.95)
  data <- as.tibble(predictions) %>% bind_cols(daughter = female_heights)
  
  ggplot(data, aes(x = father, y = fit)) +
    geom_line(color = "blue", size = 1) + 
    geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
    geom_point(data = galton_heights, aes(x = father, y = son))
  
  
  
  library(Lahman)
  bat_02 <- Batting %>% filter(yearID == 2002) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    select(playerID, singles, bb)
  
  bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    group_by(playerID) %>% 
    summarise(mean_singles = mean(singles), mean_bb = mean(bb))
  
  
  bat_99_01 %>%
    filter(mean_bb > 0.2) %>%
    distinct(playerID, mean_singles, mean_bb, .keep_all = FALSE)
  
  innerjoin <- bat_02 %>% inner_join(bat_99_01)
  innerjoin%>% summarise(cor(bb,mean_bb))
  innerjoin %>% ggplot(aes(singles,mean_singles)) + geom_point()
  innerjoin %>% ggplot(aes(bb,mean_bb)) + geom_point()
  
  get_slope <- function(x, y) cor(x, y) / (sd(x) * sd(y))
  
  bb_slope <- innerjoin %>% 
    summarize(slope = get_slope(mean_singles,singles))
  
  
  innerjoin %>% lm(singles~ mean_singles, data=.)
  innerjoin %>% lm(bb~ mean_bb, data=.)
  
  dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(HR = HR/G,
           R = R/G) %>%
    select(lgID, HR, BB, R) 
  
  dat %>% 
    group_by(lgID) %>% 
    do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
    filter(term == "HR")
  
  
  library(tidyverse)
  library(HistData)
  data("GaltonFamilies")
  set.seed(1) # if you are using R 3.5 or earlier
  set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
  galton <- GaltonFamilies %>%
    group_by(family, gender) %>%
    sample_n(1) %>%
    ungroup() %>% 
    gather(parent, parentHeight, father:mother) %>%
    mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
    unite(pair, c("parent", "child"))
  
  galton %>%
    group_by(pair) %>%
    summarize(cor = cor(parentHeight, childHeight)) %>%
    filter(cor == min(cor))
  
  galton %>%  group_by(pair) %>%
    do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
    filter(term == "parentHeight") %>%
    mutate(diff= conf.high - conf.low) %>%
    select(pair, estimate, conf.low, conf.high, diff)
  
  
  galton %>%
    group_by(pair) %>%
    do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
    filter(term == "parentHeight" & p.value < .05)
  
  
  
  
  
  
  pa_per_game <- Batting %>% 
    filter(yearID == 2002) %>% 
    group_by(teamID) %>%
    summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
    .$pa_per_game %>% 
    mean
  
  lm(R ~ 2 + 4 + 1 + 0 + 1)
  lm(R ~ 1 + 6 + 2 + 1)
  
  fit <- Teams %>% filter(yearID==1971) %>% 
    mutate(BB=BB/G,singles=(H-X2B-X3B-HR)/G,doubles=X2B/G,triples=X3B/G,HR=HR/G,R=R/G) %>% 
    do(tidy(lm(R~BB+HR, data=.)))
  
  Teams %>%
    filter(yearID == 1971) %>%
    lm(R ~ BB + HR, data = .) %>%
    tidy() %>%
    filter(term == "BB") %>%
    pull(estimate)
  
  res <- Teams %>%
    filter(yearID %in% 1961:2018) %>%
    group_by(yearID) %>%
    do(tidy(lm(R ~ BB + HR, data = .))) %>%
    ungroup() 
  res %>%
    filter(term == "BB") %>%
    ggplot(aes(yearID, estimate)) +
    geom_point() +
    geom_smooth(method = "lm")
  
  fit<- Teams %>% +
    + filter(yearID %in% 1961:2018) %>% +
    + group_by(yearID) %>% +
    + do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE))
  
  fit2 <- fit %>% filter(term == "BB") 
  fit3 <- lm(estimate ~ yearID, data= fit2)
  
  Teams %>%
    filter(yearID %in% 1961:2018) %>%
    group_by(yearID) %>%
    do(tidy(lm(R ~ BB + HR, data = .))) %>%
    ungroup() %>%
    filter(term=='BB') %>%
    lm(estimate ~ yearID, data=.) %>%
    tidy()
  
  Teams %>%
    filter(yearID == 1971) %>%
    lm(R ~ BB + HR, data = .) %>%
    tidy() %>%
    filter(term == "BB")
  
  #--------------------------------
  library(tidyverse)
  library(broom)
  library(Lahman)
  Teams_small <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(avg_attendance = attendance/G)
  
  runs <- Teams_small %>% mutate(rpg = R/G, hrpg = HR/G) %>% lm(avg_attendance ~ hrpg, data = .)
  
  tidy(runs)
  
  wins <- Teams_small %>% lm(avg_attendance ~ W, data = .)
  tidy(wins)
  
  Year <- Teams_small %>% lm(avg_attendance ~ yearID, data = .)
  tidy(Year)
  
  T <- Teams_small %>% mutate(rpg = R/G, hrpg = HR/G) 
  
  T%>% cor(W, rpg, use = "everything")
  
  cor(R/G, W)
  lm(W, R/G, data = Teams_small)
  
  cor(Teams_small$W, Teams_small$HR/Teams_small$G)
  cor(16149,15840)
  
  Strata_teams %>%  
    group_by(Strata) %>% cor(HR/G, avg_attendance)
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))
  
  Strata_teams <- Teams_small %>% mutate(Strata = round(W/10,0))%>%
    group_by(Strata)%>%
    filter(Strata %in% 5:10) 
  Strata_teams%>% filter(Strata == 8 )
  nrow(Strata_teams)
  
  
  dbstrat <- Strata_teams%>%
    mutate(HRG = HR/G,  RG = R/G)%>%
    do(tidy(lm(avg_attendance ~ RG, data = .),conf.int = TRUE))%>%
    filter(term == 'RG')
  
  
  Teams_small <- Teams %>% filter(yearID %in% 1961:2001) %>% 
    mutate(AT = attendance/G,HRG=HR/G,RG=R/G) %>%
    select(AT,HRG,W,RG,yearID) %>% 
    do(tidy(lm(AT~W+HRG+RG+yearID,data=.))) 
  Teams_small
  
  Team_model <- Teams %>% filter(yearID %in% 1961:2001) %>% 
    mutate(AT = attendance/G,HRG=HR/G,RG=R/G)
  
  model <- lm(AT~W+HRG+RG+yearID, data = Team_model)
  solution_02 <- data.frame(W = 80,HRG = 1.2,RG = 5, yearID = 2002)
  pred <- predict(model,solution_02)
  pred <- predict(model,Teams_small)
  predictions <- predict(model, interval = c("confidence"), level = 0.95)
  data <- as.tibble(predictions) %>% bind_cols(daughter = female_heights)
  data %>% summarise(avg=mean(fit))
  
  cor(pred, pred_original)
  x1=80
  x2=1.2
  x3=5
  x4=2002
  model$coefficients[1]+(x1*model$coefficients[2])+(x2*model$coefficients[2])+(x3*model$coefficients[3])+(x4*model$coefficients[4])
  
  Teams %>% 
    mutate(AT = attendance/G,HRG=HR/G,RG=R/G) %>%
    select(AT,HRG,W,RG,yearID) %>% 
    do(tidy(lm(AT~W+HRG+RG+yearID,data=.))) 
  
  Teams_small2002 <- Teams %>% filter(yearID %in% 2002) %>% 
  mutate(AT = attendance/G,HRG=HR/G,RG=R/G) 
  pred_original <- predict(model, Teams_small2002) 
  cor(pred_original,Teams_small2002$AT)
  
  library(dslabs)
  data("research_funding_rates")
  Gath <- research_funding_rates %>% mutate(notawarded_men = applications_men - awards_men, notawarded_women = applications_women - awards_women) %>%
    select(awards_men, awards_women,notawarded_men, notawarded_women)%>% 
    #spread(awards_men, awards_women,men_not_awarded,Women_not_awarded)
  gather (key, value)
  
  Sep <- Gath %>% separate(col = key, into = c('Awards', 'Gender'), sep = '_')
  
  Sep %>% group_by(Gender,Awards)%>% summarise(V = sum(value)) %>% spread(Gender, V)
  
  #---Site solution
  two_by_two <- research_funding_rates %>% 
    select(-discipline) %>% 
    summarize_all(funs(sum)) %>%
    summarize(yes_men = awards_men, 
              no_men = applications_men - awards_men, 
              yes_women = awards_women, 
              no_women = applications_women - awards_women) %>%
    gather %>%
    separate(key, c("awarded", "gender")) %>%
    spread(gender, value)
  two_by_two