
setwd('C:\\Users\\Weber\\Desktop\\大學\\政大110下\\R\\HW5')
reviews <- read.csv("Womens Clothing E-Commerce Reviews.csv", encoding = "UTF-8")
#str(reviews)
#reviews$Review.Text
library(dplyr)
library(tm)
recommended <- filter(reviews, Recommended.IND == 1)
unrecommended <- filter(reviews, Recommended.IND == 0)

x1 = Corpus(VectorSource(recommended$Review.Text))
x1 = tm_map(x1, tolower)
x1 <- tm_map(x1, content_transformer(tolower))
x1 <- tm_map(x1, removePunctuation)
x1 <- tm_map(x1, removeWords, stopwords("english"))
x1 = tm_map(x1, stemDocument)
x1[[8]][1]
x1_tdm <- TermDocumentMatrix(x1)
inspect(x1_tdm)
review_recomm_m <- as.matrix(x1_tdm)
freq_recomm_df <- rowSums(review_recomm_m)
freq_recomm_df <- sort(freq_recomm_df, decreasing = T)
freq_recomm_df[1:10]
barplot(freq_recomm_df[1:20], col = "royalblue", las = 2)
freq_recomm_df <- data.frame(word = names(freq_recomm_df), num = freq_recomm_df)

x2 = Corpus(VectorSource(unrecommended$Review.Text))
x2 = tm_map(x2, tolower)
x2 <- tm_map(x2, content_transformer(tolower))
x2 <- tm_map(x2, removePunctuation)
x2 <- tm_map(x2, removeWords, stopwords("english"))
x2 = tm_map(x2, stemDocument)
x2[[8]][1]
x2_tdm <- TermDocumentMatrix(x2)
inspect(x2_tdm)
review_unrecomm_m <- as.matrix(x2_tdm)
freq_unrecomm_df <- rowSums(review_unrecomm_m)
freq_unrecomm_df <- sort(freq_unrecomm_df, decreasing = T)
freq_unrecomm_df[1:10]
barplot(freq_unrecomm_df[1:20], col = "royalblue", las = 2)
freq_unrecomm_df <- data.frame(word = names(freq_unrecomm_df), num = freq_unrecomm_df)

library(wordcloud2)
wordcloud2(freq_recomm_df, size=0.5)
wordcloud2(freq_unrecomm_df, size=0.5)

#  web crawler  #
#--------------------------
library(rvest)
library(magrittr)
library(httr)
library("jiebaR")
library(stringr)
cc<-worker()
#---------------
###PTT
PTTKorea <- "https://www.ptt.cc/bbs/KoreaStar/index.html"
pttContent <- read_html(PTTKorea)
post_title <- pttContent %>% html_nodes(".title") %>% html_text()
post_title
d=gsub(pattern = "\n\t+\n\t+", post_title, replacement = "")

cc[d]
content <- str_remove_all(d, "[0-9a-zA-Z]+?")
cc[content]

filter<-c("公告")
f=filter_segment(cc[content] , filter)
sort(table(f),decreasing = T)

newd=data.frame(table(f))
head(newd[order(newd$Freq,decreasing = TRUE),],20)

newdd=newd[order(newd$Freq,decreasing = TRUE),]
wordcloud2(newdd)
