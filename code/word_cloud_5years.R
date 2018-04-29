#get the right packages
pkgs <- c("readr","ggplot2","tidytext","dplyr","wordcloud","RColorBrewer","reshape2","gridExtra","grid","lattice")
lapply(pkgs,library,character.only = TRUE)

#read in the speech
speech_2017 <- read_file("data/2017_aus_budget_speech.txt")
speech_2016 <- read_file("data/2016_aus_budget_speech.txt")
speech_2015 <- read_file("data/2015_aus_budget_speech.txt")
speech_2014 <- read_file("data/2014_aus_budget_speech.txt")
speech_2013 <- read_file("data/2013_aus_budget_speech.txt")

#convert it to a data frame
speech <- data_frame(year = c(2017:2013), 
                     text = c(speech_2017,speech_2016,speech_2015,speech_2014,speech_2013),
                     party = c("Liberal","Liberal","Liberal","Liberal","Labour"),
                     treasurer = c("Scott Morrison","Scott Morrison","J. B. Hockey","J. B. Hockey","Wayne Swan"))


#unnest the words and remove stop words
tokens <- speech %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)
tokens

#word count
word_count <- tokens %>% 
  count(word,year,treasurer,party) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(word = factor(word, levels = rev(unique(word))),
         key = paste(year,treasurer,party,sep = " - ")) %>%
  bind_tf_idf(word,key,n)
print(word_count,n=40)

plot(word_count$n,word_count$tf)

#quick plot to see what's ahead
plot_wc <- word_count %>% 
  group_by(year) %>% 
  top_n(20,n) %>% 
  ungroup() %>% 
  arrange(year,n) %>% 
  mutate(order = row_number())

#bar plot of the top words by year
# ggplot(plot_wc, aes(x = order, y = n))+
#   geom_col()+
#   scale_x_continuous(breaks = plot_wc$order, labels = plot_wc$word, expand = c(0,0))+
#   facet_wrap(~year,scales = "free")+
#   coord_flip()

#here's the word loud

# pdf("2017_budget_wordcloud.pdf", width=16.53,height=11.69)
#   wordcloud(word_count$word,word_count$n, scale=c(8,.2),min.freq=3, max.words=Inf, random.order=FALSE, rot.per=.15, colors=brewer.pal(4, "Dark2"))
# dev.off()

#comparison cloud
pdf("5_budget_compcloud.pdf", width=16.53,height=11.69)
word_count %>% 
  acast(word ~ year, value.var = "tf", fill = 0) %>% 
  comparison.cloud(max.words = 300,scale = c(8,0.5))
dev.off()