load("/Users/abasshkembi/Dropbox (University of Michigan)/BIOSTAT625 Final Project/BIOSTAT625-Final-Project/Data/MSHA.RData")
library(tidyverse)
library(tm)

rmd_files <- list(MSHA2, )

save(MSHA2, year_df, logratio, subunit_df, file = "/Users/abasshkembi/Dropbox (University of Michigan)/BIOSTAT625 Final Project/BIOSTAT625-Final-Project/Data/RMD_data.RData"
 )


MSHA <- MSHA %>% as_tibble()

#check the possible injury types
MSHA %>% group_by(DEGREE_INJURY_CD, DEGREE_INJURY) %>% count()

#removing 00- accident only; 08 - injuries due to natural causes; and ? - no value found
# making extreme events 01 - fatalities and 02 - permanent disability
MSHA1 <- MSHA %>% filter(!DEGREE_INJURY_CD %in% c('00','08','?')) %>% mutate(EXTREME=as.numeric((DEGREE_INJURY_CD %in% c('01','02'))))


#removing 00- accident only; 08 - injuries due to natural causes; and ? - no value found, and extreme cases (fatalities - 01, permanent disability - 02, occupational illness - 07)
# remove injuries to non-employees since we don't know what type of injury it is (09)
# making extreme events 03 - days away from work and 04 - days away from work and restricted activity
#MSHA1 <- MSHA %>% filter(!DEGREE_INJURY_CD %in% c('00','08','?', "01", "02", "07", "09")) %>% mutate(EXTREME=as.numeric((DEGREE_INJURY_CD %in% c('03','04'))))



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
trainCategory <- as.data.frame(select(MSHA2, DOCUMENT_NO, CAL_YR, SUBUNIT, EXTREME))

#tdm_occ_final <- cbind(id = rownames(tdm_occ), as.data.frame(tdm_occ))

colnames(tdm_occ)



##### CONSTANTS
# get total # of training examples
numTrain = nrow(trainMatrix)

# get Laplace smoothing factor
V = length(tokenlist$X0)

# count documents
extreme_counts <- plyr::count(trainCategory$EXTREME)

# add laplace smoother for nonextreme events
laplace_smoother <- round(extreme_counts$freq[1]/extreme_counts$freq[2])

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
sumnonextreme <- apply(nonextremevector, MARGIN = 2, sum) + laplace_smoother

# make vectors
phiextreme <- c(sumextreme/(denomextreme + ncol(extremevector)))
phinonextreme <- c(sumnonextreme/(denomnonextreme + ncol(extremevector)*laplace_smoother))



#i <- which((as.vector(sumextreme) > 1) & (as.vector(sumextreme + sumnonextreme - 2) > 4))

logratio <- phiextreme/phinonextreme
logratio = sort(logratio, decreasing = TRUE)
print(logratio)










######################## by year



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
  sumnonextreme <- apply(nonextremevector, MARGIN = 2, sum) + laplace_smoother
  
  # make vectors
  phiextreme <- c(sumextreme/(denomextreme + ncol(extremevector)))
  phinonextreme <- c(sumnonextreme/(denomnonextreme + ncol(extremevector)*laplace_smoother))
  
  
  ratio <- phiextreme/phinonextreme
  ratio = sort(ratio, decreasing = TRUE)
  
  df_ratio <- tibble(year = current_year, token = names(ratio), ratio = ratio)
  
  year_df <- rbind(year_df, df_ratio)
  
  print(paste0("Year: ", current_year))
}

names(logratio[1:5])
  

year_df %>% filter(token %in% names(logratio[1:10]) ) %>%  #.$token %>% unique
  mutate(token = factor(toupper(token), levels = toupper(names(logratio[1:10])))) %>%
  ggplot(aes(x = year, y = ratio)) +
  geom_line() +
  geom_point(size = 0.6) +
  stat_smooth(geom = "line", se=F, method = "lm", color = "hotpink", size = 2, alpha = 0.5)+
  geom_hline(yintercept = 1, linetype = 2) +
  facet_wrap(~token, ncol = 5) +
  theme_bw() +
  labs(x = "Year", y = "Likelihood ratio of extreme injury")

year_df %>% filter(year == 2016) %>% filter(token == "face")


year_df %>% 
  group_by(year) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  filter(token %in% names(logratio[1:10]) ) %>%  #.$token %>% unique
  ggplot(aes(x = year, y = rank)) +
  geom_line() +
  stat_smooth(geom = "line", se=F, method = "lm", color = "hotpink", size = 2, alpha = 0.5)+
  #geom_point() +
  scale_y_reverse() +
  #stat_smooth(geom = "line", se=F, method = "lm", color = "hotpink", size = 2, alpha = 0.5)+
  #geom_hline(yintercept = 1, linetype = 2) +
  facet_wrap(~token, ncol = 5) +
  theme_bw()




######################## by subunit



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
  sumnonextreme <- apply(nonextremevector, MARGIN = 2, sum) + laplace_smoother
  
  # make vectors
  phiextreme <- c(sumextreme/(denomextreme + ncol(extremevector)))
  phinonextreme <- c(sumnonextreme/(denomnonextreme + ncol(extremevector)*laplace_smoother))
  
  
  ratio <- phiextreme/phinonextreme
  ratio = sort(ratio, decreasing = TRUE)
  
  df_ratio <- tibble(subunit = current_subunit, token = names(ratio), ratio = ratio)
  
  subunit_df <- rbind(subunit_df, df_ratio)
  
  print(paste0("Subunit: ", current_subunit))
}



subunit_df %>% 
  group_by(subunit) %>%
  slice_max(order_by = ratio, n = 5) %>%
  ungroup() %>%
  ggplot(aes(x = fct_reorder(token, ratio), y = ratio)) +
  geom_col() +
  facet_wrap(~subunit, scales = "free_y") +
  coord_flip()
  























MSHA2 %>%
  mutate(SUBUNIT = "Total") %>%
  group_by(SUBUNIT) %>%
  summarise(n = n(), extreme = sum(EXTREME)) %>%
  ungroup() %>%
  rbind(MSHA2 %>% filter(SUBUNIT %in% subunits) %>% group_by(SUBUNIT) %>% summarise(n = n(), extreme = sum(EXTREME))) %>%
  rbind(MSHA2 %>% mutate(CAL_YR2 = ceiling((CAL_YR - 1999)/5)) %>% mutate(CAL_YR2 = ifelse(CAL_YR2 > 4, CAL_YR2-1, CAL_YR2)) %>%
          group_by(CAL_YR2) %>% summarise(n = n(), extreme = sum(EXTREME)) %>% ungroup() %>% rename("SUBUNIT" = "CAL_YR2")
          ) %>%
  mutate(perc = round(extreme/n*100, 1)) %>%
  mutate(extreme = paste0(extreme, " (", perc, "%)")) %>% select(-perc)




logratio[1:10]

sum(logratio > 1)
length(logratio)

logratio[(length(logratio) - 9):length(logratio)]


year_df %>%
  group_by(year) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  filter(rank == 1) %>%
  print(n = nrow(.))









library(lme4)
library(lmerTest)

year_df %>% filter(token %in% names(logratio[1]) ) %>%
  lm(ratio ~ year, data = .) %>%
  summary()

year_df %>% filter(token %in% names(logratio[2]) ) %>%
  lm(ratio ~ year, data = .) %>%
  summary()

year_df %>% filter(token %in% names(logratio[3]) ) %>%
  lm(ratio ~ year, data = .) %>%
  summary()

year_df %>% filter(token %in% names(logratio[4]) ) %>%
  lm(ratio ~ year, data = .) %>%
  summary()

year_df %>% filter(token %in% names(logratio[5]) ) %>%
  lm(ratio ~ year, data = .) %>%
  summary()

year_df %>% filter(token %in% names(logratio[6]) ) %>%
  lm(ratio ~ year, data = .) %>%
  summary()

year_df %>% filter(token %in% names(logratio[7]) ) %>%
  lm(ratio ~ year, data = .) %>%
  summary()

year_df %>% filter(token %in% names(logratio[8]) ) %>%
  lm(ratio ~ year, data = .) %>%
  summary()

year_df %>% filter(token %in% names(logratio[9]) ) %>%
  lm(ratio ~ year, data = .) %>%
  summary()

year_df %>% filter(token %in% names(logratio[10]) ) %>%
  lm(ratio ~ year, data = .) %>%
  summary()

cor.test(year_df %>% filter(token %in% names(logratio[1]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[1]) ) %>% .$ratio)

cor.test(year_df %>% filter(token %in% names(logratio[2]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[2]) ) %>% .$ratio)

cor.test(year_df %>% filter(token %in% names(logratio[3]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[3]) ) %>% .$ratio)

cor.test(year_df %>% filter(token %in% names(logratio[4]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[4]) ) %>% .$ratio)

cor.test(year_df %>% filter(token %in% names(logratio[5]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[5]) ) %>% .$ratio)

cor.test(year_df %>% filter(token %in% names(logratio[6]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[6]) ) %>% .$ratio)

cor.test(year_df %>% filter(token %in% names(logratio[7]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[7]) ) %>% .$ratio)

cor.test(year_df %>% filter(token %in% names(logratio[8]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[8]) ) %>% .$ratio)

cor.test(year_df %>% filter(token %in% names(logratio[9]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[9]) ) %>% .$ratio)

cor.test(year_df %>% filter(token %in% names(logratio[10]) ) %>% .$year,
    year_df %>% filter(token %in% names(logratio[10]) ) %>% .$ratio)










