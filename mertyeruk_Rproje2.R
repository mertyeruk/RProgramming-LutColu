install.packages("tm")
install.packages("RCurl")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("funModeling")
install.packages("lubridate")
install.packages("stringr")
install.packages("tidytext")
install.packages("wordcloud2")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tibble")
install.packages("tidyr")
install.packages("readr")
install.packages("readxl")
install.packages("ggpubr")
install.packages("formattable")
install.packages("ggstance")
install.packages("psych")
install.packages("GGaly")
install.packages("rstatix")
install.packages("sentimentr")
install.packages("webshot")
install.packages("htmlwidgets")
install.packages("syuzhet")
install.packages("nabor")
install.packages("data.table")
install.packages("gutenbergr")
install.packages("stopwords")
install.packages("NLP")

library(NLP)
library(tm)
library(RCurl)
library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(funModeling)
library(lubridate)
library(stringr)
library(tidytext)
library(wordcloud2)
library(wordcloud)
library(RColorBrewer)
library(tibble)
library(tidyr)
library(readr)
library(readxl)
library(ggpubr)
library(formattable)
library(ggstance)
library(psych)
library(GGaly)
library(rstatix)
library(sentimentr)
library(webshot)
library(htmlwidgets)
library(syuzhet)
library(nabor)
library(data.table)
library(gutenbergr)
library(stopwords)

lutc <- read_csv(file.choose())

view(lutc)


# URL linkelrinin temizlenme iselemi yapiliyor
lutc$textOriginal<-str_replace_all(lutc$textOriginal,"http[^[:space:]]*","")


# hashtag "@" ve "#" isaretlerinin kaldirilmasi
lutc$textOriginal<-str_replace_all(lutc$textOriginal,"#\\S+","")
lutc$textOriginal<-str_replace_all(lutc$textOriginal,"@\\S+","")
view(lutc)

# Noktalama isaretlerinin kaldirilmasi
lutc$textOriginal<-str_replace_all(lutc$textOriginal, "[[:punct:][:blank:]]+", " ")


# tüm harfler kucuk garfe donusturuluyor
lutc$textOriginal<-str_to_lower(lutc$textOriginal,"tr")
view(lutc$textOriginal)
# rakamlarin temizleme islemi yapilir
lutc$textOriginal<-removeNumbers(lutc$textOriginal)



#ASCII formatina uymayankarakterleri tezmizleme islemi yapiliyor
lutc$textOriginal<-str_replace_all(lutc$textOriginal,"[<].*[<]"," ")

lutc$textOriginal<-gsub("\ uFFFD","",lutc$textOriginal,fixed = TRUE)
lutc$textOriginal<-gsub("\ n","",lutc$textOriginal,fixed = TRUE)

view(lutc$textOriginal)


#Alfabetik olamyan harfleri temizleme islemi
lutc$textOriginal<-str_replace_all(lutc$textOriginal,"[^[:alnum:]]"," ")

#stopwords islemleri gerceklestirirlir
liste=c("ve", "bir", "bu", "en", "o", "ya", "için", "de", "da", "ben", "var", "sen", "gibi", "çok", "daha", "ama", "olsun",
        "hiç", "bi", "her", "yok", "seni", "yine", "ki", "bile", "ederim", "sana", "böyle", "mi", "kadar")
liste



# gereksiz tekrarlamalar ve baglaclarin kaldirilma islemi yapilir
lutc$textOriginal<-removeWords(lutc$textOriginal,liste)

temiz_t <- lutc  %>% select(textOriginal) %>% mutate(linenumber = row_number()) %>% unnest_tokens(word,textOriginal)


head(temiz_t)

#Twitlerde toplamda 250'den daha fazla kullanılan kelimelerin listelenmesi
temiz_t %>%
  count(word, sort = TRUE) %>%
  filter(n > 250) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + theme_minimal() +
  ggtitle("Yorumlarda en çok kullanılan kelimeler")


library(wordcloud)
library(RColorBrewer)
#Kelime bulutu 
temiz_t %>% 
  count(word) %>% 
  with(wordcloud(word,n, max.words=100,colors= brewer.pal(8,"Dark2")))
set.seed(123)

#kelime bulutu.  
for (text in temiz_t) {
  # metni duygu analizi için hazırlayın
  sentiment = get_sentiment(text)
  # ekrana yazdırın veya veri setinize ekleyin
  print(sentiment$sentiment)
}

