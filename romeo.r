gutenberg_works(str_detect(title,'Romeo'))

# geting sentiments Library

nrc<-get_sentiments('nrc')

joy_sad<-nrc%>%
  filter(sentiment == 'joy' | sentiment == 'sadness')

Romeo<-gutenberg_download(1513)

Romeo$line<-1:5268

Romeo$gutenberg_id<-NULL

Romeo_words<-Romeo%>%
  unnest_tokens(word,text)

Romeo_joy_sad<-inner_join(Romeo_words,joy_sad)

Romeo_frequency<-Romeo_joy_sad%>%
  group_by(word)%>%
  summarise(frequency = n())

Romeo_words<-Romeo_words%>%
  filter(!(word %in% stop_words$word))

Romeo_freq<-Romeo_words%>%
  group_by(word)%>%
  summarise(frequency = n())

wordcloud(Romeo_frequency$word,Romeo_frequency$frequency)
#wordcloud2(Romeo_freq, color = "random-light", backgroundColor = "grey")
#wordcloud2(Romeo_freq, minRotation = -pi/6, maxRotation = -pi/6, minSize = 10,
#           rotateRatio = 1)
wordcloud2(Romeo_freq, figPath = "shake.png",color = "white",backgroundColor = "darkblue")


Romeo_words$groups<-Romeo_words$line%/%80


affin<-get_sentiments('afinn')

Romeo_words<-inner_join(Romeo_words,affin)


Romeo_senti_score<-Romeo_words%>%
  group_by(groups)%>%
  summarise(Romeo_score=sum(score))

ggplot()+
  geom_point(data=Romeo_senti_score,aes(x=groups,y=Romeo_score))+
  geom_line(data=Romeo_senti_score,aes(x=groups,y=Romeo_score))
