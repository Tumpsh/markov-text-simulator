library(plyr)
library(dplyr)

## Read text document as vector of characters
file_name <- "montecristo.txt"
mc_chars <- unlist(strsplit(readChar(file_name, file.info(file_name)$size), split = NULL))

## Replace weird characters with *
mc_alphanum <- gsub("[^[:alnum:].?! \"\']", "*", mc_chars)

letter_pairs <- data.frame("first" = tolower(mc_alphanum), 
                           "second" = c(tolower(mc_alphanum)[2:length(mc_alphanum)], " "), 
                           "third" = c(tolower(mc_alphanum)[3:length(mc_alphanum)], " ", " "), 
                           "fourth" = c(tolower(mc_alphanum)[4:length(mc_alphanum)], " ", " ", " "), stringsAsFactors = FALSE)
letter_pairs <- letter_pairs %>% mutate(first_duo = paste0(first, second), 
                                        second_duo = paste0(third, fourth))

pair_tallys <- letter_pairs %>% group_by(first, second) %>% tally()
duo_pair_tallys <- letter_pairs %>% group_by(first_duo, second_duo) %>% tally()

## Fill single char transition matrix and clean

tmat <- data.frame()
for(rownum in 1:nrow(pair_tallys)){
  row <- pair_tallys[rownum, ]
  tmat[row$first, row$second] <- row$n
  print(paste(rownum, row))
}

# for(cha1 in unique(letter_pairs$first)){
#   for(cha2 in unique(letter_pairs$first)){
#     count <- pair_tallys[pair_tallys$first == cha1 & pair_tallys$second == cha2, "n"]
#     if(nrow(count) == 1){
#     tmat[cha1, cha2] <- count
#     }
#   }
# }

tmat[is.na(tmat)] <- 0
tmat$total <- Reduce("+", tmat[, 1:ncol(tmat)])

## Writing a sample text starting with "t"

text_current <- "t"
recent_char <- "t"
for(i in 1:1000){
  distr <- tmat[recent_char, 1:(ncol(tmat) - 1)]/tmat[recent_char, "total"]
  distr[is.na(distr)] <- 0
  recent_char <- sample(colnames(tmat[, 1:(ncol(tmat) - 1)]), 1, prob = distr)
  text_current <- paste0(text_current, recent_char)
  print(text_current)
}

## Fill duo char transition matrix and clean

duo_tmat <- data.frame()

for(rownum in 1:nrow(duo_pair_tallys)){
  row <- duo_pair_tallys[rownum, ]
  duo_tmat[row$first_duo, row$second_duo] <- row$n
  print(paste(rownum, row))
}

duo_tmat[is.na(duo_tmat)] <- 0
duo_tmat$total <- Reduce("+", duo_tmat[, 1:ncol(duo_tmat)])


## Writing a sample text starting with "th"

text_current <- "th"
recent_duo <- "th"
for(i in 1:200){
  distr <- duo_tmat[recent_duo, 1:(ncol(duo_tmat) - 1)]/duo_tmat[recent_duo, "total"]
  distr[is.na(distr)] <- 0
  recent_duo <- sample(colnames(duo_tmat[, 1:(ncol(duo_tmat) - 1)]), 1, prob = distr)
  text_current <- paste0(text_current, recent_duo)
  print(text_current)
}
  
