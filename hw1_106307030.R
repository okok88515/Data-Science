install.packages("tidyverse")
library(tidyverse)
setwd('C:\\Users\\Weber\\Desktop\\大學\\政大110下\\R\\HW1')
product_list <- read.csv("product_list.csv")
salesdata <- read.csv("salesdata.csv")
client_list <- read.csv("client_list.csv")

#Q1
product_list
product_list <- product_list %>% separate(Item, into=c("Product", "Item"), sep = "_", convert = TRUE)


#Q2
full.table <- salesdata %>% full_join(product_list) %>% full_join(client_list)

#Q3
full.table <- full.table %>% mutate(spend = UnitPrice * Quantity)

#Q4
highclass <- full.table %>% filter(Membership %in% c("gold", "diamond"))
lowclass <- full.table %>% filter(Membership != "gold" & Membership != "diamond")

highclass %>% 
  summarise(mean(Age))

lowclass %>% 
  summarise(mean(Age))

highclass %>%  
  group_by(Gender) %>%
  tally()

highclass %>%
  group_by(Region) %>%
  tally() %>%
  arrange(desc(n))

highclass %>% 
  summarise(mean(spend))

lowclass %>%  
  group_by(Gender) %>%
  tally()

lowclass %>%
  group_by(Region) %>%
  tally() %>%
  arrange(desc(n))

lowclass %>% 
  summarise(mean(spend))

#Q5
female <- full.table %>% filter(Gender == "female")

female %>%
  summarise(mean(Age))

female %>%
  group_by(Region) %>%
  tally() %>%
  arrange(desc(n))

female %>%
  summarise(mean(spend))

total_spend.table <- female %>%
  group_by(Item) %>%
  summarise(total_spend = sum(spend)) %>%
  arrange(desc(total_spend)) 

total_spend.table <- total_spend.table%>% 
  mutate(rank = dense_rank(desc(total_spend)),
         per = (100*total_spend/sum(total_spend)) %>% round(1),
         txt = paste0(Item, ': ', total_spend, ' (', per, '%)')) 

ggplot(total_spend.table,aes(fill=Item)) +
  geom_bar(aes(x='', y=per), stat='identity') +
  coord_polar("y", start=0) +
  xlab('') + ylab('Percentage (%)') +
  labs(title=paste0('Top 6 Products the Female Spend on')) +
  scale_fill_discrete(name='Item', labels=total_spend.table$txt)
