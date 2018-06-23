library(plyr)
library(dplyr)

## Read text document as vector of words
file_name <- "montecristo.txt"
mc_words <- unlist(strsplit(readChar(file_name, file.info(file_name)$size), split = c(" ", "\n", "\t")))

## Replace weird characters with *
mc_alphanum <- gsub(" ", "", mc_words)
mc_alphanum <- gsub("[^[:alnum:]]", "", mc_words)

mc_alphanum <- mc_alphanum[nchar(mc_alphanum) > 0]

word_pairs <- data.frame("first" = tolower(mc_alphanum), 
                        "second" = c(tolower(mc_alphanum)[2:length(mc_alphanum)], " "),
                        stringsAsFactors = FALSE)
pair_tallys <- word_pairs %>% group_by(first, second) %>% tally()
pair_tallys <- pair_tallys[pair_tallys$n > 5, ]

## Fill single word transition matrix and clean

tmat <- data.frame()
for(rownum in 1:nrow(pair_tallys)){
  row <- pair_tallys[rownum, ]
  tmat[row$first, row$second] <- row$n
  print(paste(rownum, row))
}

tmat[is.na(tmat)] <- 0
tmat$total <- Reduce("+", tmat[, 1:ncol(tmat)])

## Writing a sample text starting with "the"

text_current <- "the"
recent_word <- "the"
i <- 0
distr <- tmat[recent_word, 1:(ncol(tmat) - 1)]/tmat[recent_word, "total"]
distr[is.na(distr)] <- 0
while(i < 100){
  temp_recent_word <- sample(colnames(tmat[, 1:(ncol(tmat) - 1)]), 1, prob = distr)
  temp_distr <- tmat[temp_recent_word, 1:(ncol(tmat) - 1)]/tmat[temp_recent_word, "total"]
  temp_distr[is.na(temp_distr)] <- 0
  if(sum(temp_distr) < 1){
    text_current <- text_current <- paste0(text_current, ".")
    recent_word <- sample(rownames(tmat), 1, prob = tmat$total/sum(tmat$total))
    next
  }
  recent_word <- temp_recent_word
  distr <- temp_distr
  text_current <- paste(text_current, recent_word)
  print(text_current)
  i <- i+1
}

