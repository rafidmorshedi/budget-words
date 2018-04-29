#get the right packages
pkgs <- c("readr","ggplot2","tidytext","dplyr","wordcloud","RColorBrewer","reshape2")
lapply(pkgs,library,character.only = TRUE)

#read in the speech
speech_2017 <- read_file("data/2017_aus_budget_speech.txt")
speech_2015 <- read_file("data/2015_aus_budget_speech.txt")

#convert it to a data frame
speech <- data_frame(year = c(2017,2015), text = c(speech_2017,speech_2015))

#unnest th etwords and remove stop words
tokens <- speech %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)
tokens

#word count
word_count <- tokens %>% 
  count(word,year) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(word = factor(word, levels = rev(unique(word))))
# print(word_count,n=40)

#quick plot to see what's ahead
plot_wc <- word_count %>% 
  group_by(year) %>% 
  top_n(20,n) %>% 
  ungroup() %>% 
  arrange(year,n) %>% 
  mutate(order = row_number())

ggplot(plot_wc, aes(x = order, y = n))+
  geom_col()+
  scale_x_continuous(breaks = plot_wc$order, labels = plot_wc$word, expand = c(0,0))+
  facet_wrap(~year,scales = "free")+
  coord_flip()

#here's the word loud

pdf("2017_budget_wordcloud.pdf", width=16.53,height=11.69)
  wordcloud(word_count$word,word_count$n, scale=c(8,.2),min.freq=3, max.words=Inf, random.order=FALSE, rot.per=.15, colors=brewer.pal(4, "Dark2"))
dev.off()

#comparison cloud
pdf("2015_2017_budget_compcloud.pdf", width=16.53,height=11.69)
word_count %>% 
  acast(word ~ year, val.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 300,scale = c(8,0.2))
dev.off()