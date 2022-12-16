load("/Users/abasshkembi/Dropbox (University of Michigan)/BIOSTAT625 Final Project/BIOSTAT625-Final-Project/Data/MSHA.RData")
library(tidyverse)
library(tm)


MSHA <- MSHA %>% as_tibble()

#check the possible injury types
MSHA %>% select(DEGREE_INJURY_CD, DEGREE_INJURY) %>% distinct()

#removing 00- accident only; 08 - injuries due to natural causes; and ? - no value found
# making extreme events 01 - fatalities and 02 - permanent disability
MSHA1 <- MSHA %>% filter(!DEGREE_INJURY_CD %in% c('00','08','?')) %>% mutate(EXTREME=as.numeric((DEGREE_INJURY_CD %in% c('01','02'))))


#removing 00- accident only; 08 - injuries due to natural causes; and ? - no value found, and extreme cases (fatalities - 01, permanent disability - 02, occupational illness - 07)
# remove injuries to non-employees since we don't know what type of injury it is (09)
# making extreme events 03 - days away from work and 04 - days away from work and restricted activity
MSHA1 <- MSHA %>% filter(!DEGREE_INJURY_CD %in% c('00','08','?', "01", "02", "07", "09")) %>% mutate(EXTREME=as.numeric((DEGREE_INJURY_CD %in% c('03','04'))))



#check the possible subunits
MSHA1 %>% count(SUBUNIT, EXTREME) %>% arrange(-n)

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

MSHA2 <- MSHA2 %>% arrange(DOCUMENT_NO)



# create term document matrix

tdm_occ <- as.matrix(text2map::dtm_builder(MSHA2, OCCUPATION, DOCUMENT_NO))

trainMatrix <- as.data.frame(tdm_occ)
tokenlist <- data.frame(X = 1:ncol(trainMatrix), X0 = colnames(tdm_occ))
trainCategory <- as.data.frame(select(MSHA2, DOCUMENT_NO, CAL_YR, EXTREME))

#tdm_occ_final <- cbind(id = rownames(tdm_occ), as.data.frame(tdm_occ))

colnames(tdm_occ)



##### CONSTANTS
# get total # of training examples
numTrain = nrow(trainMatrix)

# get Laplace smoothing factor
V = length(tokenlist$X0)

# get prior y_hat MLE, # of spam emails divided by total
y_hat <- plyr::count(trainCategory$EXTREME)$freq[2]/numTrain

# create a column for total amount of words per email # had c(-1) in columns
trainMatrix$wordcount = rowSums(trainMatrix)

# create vectors of spam and non-spam training examples
extremevector <- trainMatrix[trainCategory$EXTREME == 1,]
nonextremevector <- trainMatrix[trainCategory$EXTREME == 0, ]

# make denominators
denomextreme = V + sum(extremevector$wordcount)
denomnonextreme = V + sum(nonextremevector$wordcount)

# get rid of word count columns
extremevector$wordcount <- NULL
nonextremevector$wordcount <- NULL


# make vector of numerators for spam and not spam, do laplace smoothing
# first entry is # of occurrences for word X0 in respective category
sumextreme <- apply(extremevector, MARGIN = 2, sum) + 1
sumnonextreme <- apply(nonextremevector, MARGIN = 2, sum) + 1

# make vectors
phiextreme <- c(sumextreme/denomextreme)
phinonextreme <- c(sumnonextreme/denomnonextreme)



i <- which((as.vector(sumextreme) > 1) & (as.vector(sumextreme + sumnonextreme - 2) > 4))

logratio <- log(phiextreme/phinonextreme)
logratio = sort(logratio, decreasing = TRUE)
print(logratio)










######################## by year

i_2000 <- which(trainCategory$CAL_YR == 2000)

trainMatrix_yr <- trainMatrix[i_2000, ]
tokenlist_yr <- data.frame(X = 1:ncol(trainMatrix_yr), X0 = colnames(trainMatrix_yr))
trainCategory_yr <- trainCategory[i_2000, ]


##### CONSTANTS
# get total # of training examples
numTrain = nrow(trainMatrix_yr)

# get Laplace smoothing factor
V = length(tokenlist_yr$X0)

# get prior y_hat MLE, # of spam emails divided by total
y_hat <- plyr::count(trainCategory_yr$EXTREME)$freq[2]/numTrain

# create a column for total amount of words per email # had c(-1) in columns
trainMatrix_yr$wordcount = rowSums(trainMatrix_yr)

# create vectors of spam and non-spam training examples
extremevector <- trainMatrix_yr[trainCategory_yr$EXTREME == 1,]
nonextremevector <- trainMatrix_yr[trainCategory_yr$EXTREME == 0, ]

# make denominators
denomextreme = V + sum(extremevector$wordcount)
denomnonextreme = V + sum(nonextremevector$wordcount)

# get rid of word count columns
extremevector$wordcount <- NULL
nonextremevector$wordcount <- NULL


# make vector of numerators for spam and not spam, do laplace smoothing
# first entry is # of occurrences for word X0 in respective category
sumextreme <- apply(extremevector, MARGIN = 2, sum) + 1
sumnonextreme <- apply(nonextremevector, MARGIN = 2, sum) + 1

# make vectors
phiextreme <- c(sumextreme/denomextreme)
phinonextreme <- c(sumnonextreme/denomnonextreme)


logratio <- log(phiextreme/phinonextreme)
logratio = sort(logratio, decreasing = TRUE)
print(logratio)







































#fatalities
MSHA3 <- MSHA2 %>%
  mutate(
    NARRATIVE = str_remove_all(NARRATIVE, "[[:punct:]]"),
    NARRATIVE = str_remove_all(NARRATIVE, "[[:digit:]]+"),
    NARRATIVE = str_replace_all(NARRATIVE, "[[:space:]]+", " "),
    NARRATIVE = str_remove_all(NARRATIVE, "[[:space:]]$"),
    NARRATIVE = tolower(NARRATIVE)
  )


Filter(length, str_extract_all(MSHA2$NARRATIVE, "dead"))
Filter(length, str_extract_all(MSHA2$NARRATIVE, "death"))
Filter(length, str_extract_all(MSHA2$NARRATIVE, "fatal"))
Filter(length, str_extract_all(MSHA2$NARRATIVE, "fatality"))
Filter(length, str_extract_all(MSHA2$NARRATIVE, "fatality"))
Filter(length, str_extract_all(MSHA2$NARRATIVE, "fatally"))
Filter(length, str_extract_all(MSHA2$NARRATIVE, "died"))



