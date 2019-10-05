library(tidyverse)
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
  
  final <- setdiff(df1, df2)
  final
  
  install.packages("Lahman")
  library(Lahman)
  
  top <- Batting %>% 
    filter(yearID == 2016) %>%
    arrange(desc(HR)) %>%    # arrange by descending HR count
    slice(1:10)    # take entries 1-10
  top %>% as_tibble()
  
  Master %>% as_tibble()

