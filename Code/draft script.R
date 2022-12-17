# load in the data
load("/Users/abasshkembi/Dropbox (University of Michigan)/BIOSTAT625 Final Project/BIOSTAT625-Final-Project/Data/MSHA.RData")
library(tidyverse)
library(tm)

#save data for rmarkdown analysis
save(MSHA2, year_df, logratio, subunit_df, 
     file = "/Users/abasshkembi/Dropbox (University of Michigan)/BIOSTAT625 Final Project/BIOSTAT625-Final-Project/Data/RMD_data.RData"
 )


# save dataet as tibble
MSHA <- MSHA %>% as_tibble()

#check the possible injury types
MSHA %>% group_by(DEGREE_INJURY_CD, DEGREE_INJURY) %>% count()

#removing 00- accident only; 08 - injuries due to natural causes; and ? - no value found
# making extreme events 01 - fatalities and 02 - permanent disability
MSHA1 <- MSHA %>% filter(!DEGREE_INJURY_CD %in% c('00','08','?')) %>% mutate(EXTREME=as.numeric((DEGREE_INJURY_CD %in% c('01','02'))))


#check the possible subunits
MSHA1 %>% count(SUBUNIT, EXTREME) %>% arrange(-n)

#check the possible degrees of injury
MSHA1 %>% count(DEGREE_INJURY) %>% arrange(-n)

#check the possible years
MSHA1 %>% count(CAL_YR, EXTREME) %>% print(n = 44)

#select columns we need for analysis
MSHA2 <- MSHA1 %>%
  select(DOCUMENT_NO, CAL_YR, SUBUNIT, OCCUPATION, NARRATIVE, EXTREME, DEGREE_INJURY, DEGREE_INJURY_CD)

#clean the occupations
MSHA2 <- MSHA2 %>%
  mutate(
    OCCUPATION = tolower(OCCUPATION),
    OCCUPATION = str_replace_all(OCCUPATION, "/", " "),
    OCCUPATION = str_remove_all(OCCUPATION, ","),
    OCCUPATION = str_remove_all(OCCUPATION, "-"),
    OCCUPATION = str_remove_all(OCCUPATION, " \\(.+\\)"),
    OCCUPATION = str_replace_all(OCCUPATION, "  {2,}", " ")
  )

#arrange by documebnt no.
MSHA2 <- MSHA2 %>% arrange(DOCUMENT_NO)



# create term document matrix

tdm_occ <- as.matrix(text2map::dtm_builder(MSHA2, OCCUPATION, DOCUMENT_NO))

trainMatrix <- as.data.frame(tdm_occ)
tokenlist <- data.frame(X = 1:ncol(trainMatrix), X0 = colnames(tdm_occ))
trainCategory <- as.data.frame(select(MSHA2, DOCUMENT_NO, CAL_YR, SUBUNIT, EXTREME))


##### CONSTANTS
# get total # of training examples
numTrain = nrow(trainMatrix)

# get Laplace smoothing factor
V = length(tokenlist$X0)

# count documents
extreme_counts <- plyr::count(trainCategory$EXTREME)

# add laplace smoother for nonextreme events
laplace_smoother <- round(extreme_counts$freq[1]/extreme_counts$freq[2])

# create a column for total amount of words per injury report
trainMatrix$wordcount = rowSums(trainMatrix)

# create vectors of extreme and non-extreme training examples
extremevector <- trainMatrix[trainCategory$EXTREME == 1,]
nonextremevector <- trainMatrix[trainCategory$EXTREME == 0, ]

# make denominators
denomextreme = V + sum(extremevector$wordcount)
denomnonextreme = V*laplace_smoother + sum(nonextremevector$wordcount)

# get rid of word count columns
extremevector$wordcount <- NULL
nonextremevector$wordcount <- NULL


# make vector of numerators for extreme and non-extreme, do laplace smoothing
# first entry is # of occurrences for word X0 in respective category
sumextreme <- apply(extremevector, MARGIN = 2, sum) + 1
sumnonextreme <- apply(nonextremevector, MARGIN = 2, sum) + laplace_smoother

# make vectors
phiextreme <- c(sumextreme/denomextreme)
phinonextreme <- c(sumnonextreme/denomnonextreme)

#note: we chose not to use the log-ratio, just the ratio
logratio <- phiextreme/phinonextreme
logratio = sort(logratio, decreasing = TRUE)
print(logratio)










######################## by year
# repeating the analysis for each year


years <- 2000:2021

year_df <- NULL

for(i in 1:length(years)) {
  current_year <- years[i]
  
  i_2000 <- which(trainCategory$CAL_YR == current_year)
  
  trainMatrix_yr <- trainMatrix[i_2000, ]
  trainMatrix_yr <- trainMatrix_yr[, colSums(trainMatrix_yr != 0) > 0]
  tokenlist_yr <- data.frame(X = 1:ncol(trainMatrix_yr), X0 = colnames(trainMatrix_yr))
  trainCategory_yr <- trainCategory[i_2000, ]
  
  
  ##### CONSTANTS
  # get total # of training examples
  numTrain = nrow(trainMatrix_yr)
  
  # get Laplace smoothing factor
  V = length(tokenlist_yr$X0)
  
  # count documents
  extreme_counts <- plyr::count(trainCategory_yr$EXTREME)
  
  # add laplace smoother for nonextreme events
  laplace_smoother <- round(extreme_counts$freq[1]/extreme_counts$freq[2])
  
  # create a column for total amount of words per injury report
  trainMatrix_yr$wordcount = rowSums(trainMatrix_yr)
  
  # create vectors of extreme and non-extreme training examples
  extremevector <- trainMatrix_yr[trainCategory_yr$EXTREME == 1,]
  nonextremevector <- trainMatrix_yr[trainCategory_yr$EXTREME == 0, ]
  
  # make denominators
  denomextreme = V + sum(extremevector$wordcount)
  denomnonextreme = V*laplace_smoother + sum(nonextremevector$wordcount)
  
  # get rid of word count columns
  extremevector$wordcount <- NULL
  nonextremevector$wordcount <- NULL
  
  
  # make vector of numerators for extreme and non-extreme, do laplace smoothing
  # first entry is # of occurrences for word X0 in respective category
  sumextreme <- apply(extremevector, MARGIN = 2, sum) + 1
  sumnonextreme <- apply(nonextremevector, MARGIN = 2, sum) + laplace_smoother
  
  # make vectors
  phiextreme <- c(sumextreme/denomextreme)
  phinonextreme <- c(sumnonextreme/denomnonextreme)
  
  
  ratio <- phiextreme/phinonextreme
  ratio = sort(ratio, decreasing = TRUE)
  
  df_ratio <- tibble(year = current_year, token = names(ratio), ratio = ratio)
  
  year_df <- rbind(year_df, df_ratio)
  
  print(paste0("Year: ", current_year))
}





######################## by subunit
# repeat analaysis for each subunit

# vector of each subunit
subunits <- unique(MSHA1[MSHA1$SUBUNIT_CD %in% c(30,1,2,3,17, 6),9])$SUBUNIT

subunit_df <- NULL

for(i in 1:length(subunits)) {
  current_subunit <- subunits[i]
  
  i_2000 <- which(trainCategory$SUBUNIT == current_subunit)
  
  trainMatrix_yr <- trainMatrix[i_2000, ]
  trainMatrix_yr <- trainMatrix_yr[, colSums(trainMatrix_yr != 0) > 0]
  tokenlist_yr <- data.frame(X = 1:ncol(trainMatrix_yr), X0 = colnames(trainMatrix_yr))
  trainCategory_yr <- trainCategory[i_2000, ]
  
  
  ##### CONSTANTS
  # get total # of training examples
  numTrain = nrow(trainMatrix_yr)
  
  # get Laplace smoothing factor
  V = length(tokenlist_yr$X0)
  
  # count documents
  extreme_counts <- plyr::count(trainCategory_yr$EXTREME)
  
  # add laplace smoother for nonextreme events
  laplace_smoother <- round(extreme_counts$freq[1]/extreme_counts$freq[2])
  
  # create a column for total amount of words per injury report
  trainMatrix_yr$wordcount = rowSums(trainMatrix_yr)
  
  # create vectors of extreme and non-extreme training examples
  extremevector <- trainMatrix_yr[trainCategory_yr$EXTREME == 1,]
  nonextremevector <- trainMatrix_yr[trainCategory_yr$EXTREME == 0, ]
  
  # make denominators
  denomextreme = V + sum(extremevector$wordcount)
  denomnonextreme = V*laplace_smoother+ sum(nonextremevector$wordcount)
  
  # get rid of word count columns
  extremevector$wordcount <- NULL
  nonextremevector$wordcount <- NULL
  
  
  # make vector of numerators for extreme and non-extreme, do laplace smoothing
  # first entry is # of occurrences for word X0 in respective category
  sumextreme <- apply(extremevector, MARGIN = 2, sum) + 1
  sumnonextreme <- apply(nonextremevector, MARGIN = 2, sum) + laplace_smoother
  
  # make vectors
  phiextreme <- c(sumextreme/denomextreme)
  phinonextreme <- c(sumnonextreme/denomnonextreme)
  
  
  ratio <- phiextreme/phinonextreme
  ratio = sort(ratio, decreasing = TRUE)
  
  df_ratio <- tibble(subunit = current_subunit, token = names(ratio), ratio = ratio)
  
  subunit_df <- rbind(subunit_df, df_ratio)
  
  print(paste0("Subunit: ", current_subunit))
}

  


















# run correlations for yearly trend for the top 10 most indicative tokens


#superintendent
cor.test(year_df %>% filter(token %in% names(logratio[1]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[1]) ) %>% .$ratio)

#auger
cor.test(year_df %>% filter(token %in% names(logratio[2]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[2]) ) %>% .$ratio)

#stoper
cor.test(year_df %>% filter(token %in% names(logratio[3]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[3]) ) %>% .$ratio)

#tender
cor.test(year_df %>% filter(token %in% names(logratio[4]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[4]) ) %>% .$ratio)

#shaftcrew
cor.test(year_df %>% filter(token %in% names(logratio[5]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[5]) ) %>% .$ratio)

#washer
cor.test(year_df %>% filter(token %in% names(logratio[6]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[6]) ) %>% .$ratio)

#shaft
cor.test(year_df %>% filter(token %in% names(logratio[7]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[7]) ) %>% .$ratio)

#iron
cor.test(year_df %>% filter(token %in% names(logratio[8]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[8]) ) %>% .$ratio)

#chute
cor.test(year_df %>% filter(token %in% names(logratio[9]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[9]) ) %>% .$ratio)

#grizzly
cor.test(year_df %>% filter(token %in% names(logratio[10]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[10]) ) %>% .$ratio)










