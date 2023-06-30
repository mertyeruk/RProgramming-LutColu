

install.packages("textdata")
library(textdata)
get_sentiments("afinn")

# duygu analizi için gereken kütüphaneleri yükleme
library(tidyverse)
library(tidytext)
library(sentimentr)
library(textdata)
library(tm)

# duygu analizi için gereken ayarları yapma
lutc_sentiment <- lutc %>%
  select(textOriginal) %>% # burada, duygu analizi yapmak istediğiniz sütunu seçin
  unnest_tokens(word, textOriginal) %>%
  filter(!word %in% stop_words$word) %>% # burada, durdurma kelimelerini filtreleme
  anti_join(get_sentiments("bing")) %>% # burada, Bing duygu sözlüğü kullanılıyor
  count(word, sort = TRUE)

# sonuçları görüntüleme
lutc_sentiment

# duygu analizi sonuçlarını grafiklerle gösterme
lutc_sentiment %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip()


#grafik oluştururken hata oluşursa bunu çözmek için
lutc_sentiment <- lutc %>%
  select(textOriginal) %>% # burada, duygu analizi yapmak istediğiniz sütunu seçin
  unnest_tokens(word, textOriginal) %>%
  filter(!word %in% stop_words$word) %>% # burada, durdurma kelimelerini filtreleme
  anti_join(get_sentiments("bing")) %>% # burada, Bing duygu sözlüğü kullanılıyor
  count(word, sort = TRUE)

lutc_sentiment

# duygu puanlarını gösteren bar chart oluşturma ilk 20 kelime
lutc_sentiment %>%
  mutate(word = reorder(word, n)) %>%
  slice(1:20) %>%  # ilk 20 kelimeyi seçme
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip()


# duygu puanlarını gösteren bar chart oluşturma
# lutc_sentiment %>%
#  mutate(word = reorder(word, n)) %>%
#  ggplot(aes(x = word, y = n)) +
#  geom_col() +
#  coord_flip()

# duygu kategorilerinin dağılımını gösteren pie chart oluşturma ilk 20 kelime
lutc_sentiment %>%
  mutate(word = reorder(word, n)) %>%
  slice(1:20) %>%  # ilk 20 kelimeyi seçme
  ggplot(aes(x = "", y = n, fill = word)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme(axis.text.x = element_blank())


# duygu kategorilerinin dağılımını gösteren pie chart oluşturma
#lutc_sentiment %>%
#  mutate(word = reorder(word, n)) %>%
#  ggplot(aes(x = "", y = n, fill = word)) +
 # geom_bar(width = 1, stat = "identity") +
#  coord_polar("y", start = 0) +
 # theme(axis.text.x = element_blank())
#lutc_sentiment <- lutc %>%
 # select(textOriginal) %>% # burada, duygu analizi yapmak istediğiniz sütunu seçin
  #unnest_tokens(word, textOriginal) %>%
  #filter(!word %in% stop_words$word) %>% # burada, durdurma kelimelerini filtreleme
  #anti_join(get_sentiments("bing")) %>% # burada, Bing duygu sözlüğü kullanılıyor
  #count(word, sort = TRUE)

#lutc_sentiment


#lutc_sentiment kelime filtreleme
lutc_sentiment %>%
  filter(!word %in% c("ve", "bir", "bu", "en", "o", "ya", "için", "de", "da", "ben", "var", "sen", "gibi", "çok", "daha", "ama", "olsun",
                      "hiç", "bi", "her", "yok", "seni", "yine", "ki", "bile", "ederim", "sana", "böyle", "mi", "kadar"))


# İlk 20 kelime için box plot oluşturma
lutc_sentiment %>%
  top_n(10, n) %>%
  ggplot(aes(x = word, y = n)) +
  geom_boxplot()

#box_plot
lutc_sentiment %>%
  top_n(10, n) %>%
  filter(!word %in% c("ve", "bir", "bu", "en", "o", "ya", "için", "de", "da", "ben", "var", "sen", "gibi", "çok", "daha", "ama", "olsun",
                      "hiç", "bi", "her", "yok", "seni", "yine", "ki", "bile", "ederim", "sana", "böyle", "mi", "kadar", "izlerken")) %>%
  ggplot(aes(y = n, x = word)) +
  geom_boxplot()



#Emoji olmayan verileri ayrı bir veri setine atama
lutc_no_emoji <- lutc %>%
  select(textDisplay) %>%
  mutate(emoji_count = str_count(textDisplay, pattern = "[^[:ascii:]]")) %>%
  filter(emoji_count == 0)


#Emoji türlerinin bar chart oluşturma
lutc_emoji_types %>%
  ggplot(aes(x = emoji_type, y = count)) +
  geom_col()


#emoji anliziiiiiad
lutc_emoji_types <- lutc %>%
  select(textDisplay) %>%
  mutate(emoji_count = str_count(textDisplay, pattern = "[^[:ascii:]]")) %>%
  group_by(emoji_count) %>%
  summarize(count = n()) %>%
  rename(emoji_type = emoji_count) %>%
  mutate(emoji_type = ifelse(emoji_type == 0, "No Emoji",
                             ifelse(emoji_type == 1, "1 Emoji",
                                    ifelse(emoji_type >= 2, "2 or More Emojis", NA))))

#emoji line chart
lutc_emoji_counts <- lutc %>%
  select(textDisplay) %>%
  mutate(emoji_count = str_count(textDisplay, pattern = "[^[:ascii:]]")) %>%
  group_by(emoji_count) %>%
  summarize(count = n())

lutc_emoji_counts %>%
  ggplot(aes(x = emoji_count, y = count)) +
  geom_line()


# pozitif ve negatif kelime listelerini oluşturun
pos_words <- c("güzel", "mutlu", "başarılı", "iyi", "memnun", "keyifle", "helal", "tebrik", "güzel", "kaliteli", "teşekkürler")
neg_words <- c("kötü", "mutsuz", "başarısız", "kötü", "memnun değil", "beğenmedim", "kalitesiz", "")

# pozitif ve negatif sayılarını sıfırlayın
pos_count <- 0
neg_count <- 0

# verinizdeki kelimeleri gezinerek pozitif ve negatif sayılarını artırın
for (word in temiz_t$word) {
  if (word %in% pos_words) {
    pos_count <- pos_count + 1
  } else if (word %in% neg_words) {
    neg_count <- neg_count + 1
  }
}

# pozitif ve negatif sayılarını yazdırın
print(paste("Pozitif sayısı:", pos_count))
print(paste("Negatif sayısı:", neg_count))



# Duygu puanlarını gösteren box plot oluşturma
lutc_sentiment %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_boxplot() +
  coord_flip()

lutc_sentiment <- lutc %>%
  select(textOriginal) %>%
  unnest_tokens(word, textOriginal) %>%
  filter(!word %in% stop_words$word) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment_score = mean(score), sort = TRUE)

ggplot(data = lutc_sentiment, aes(x = sentiment_score, y = word)) +
  geom_boxplot()


# Veri setinizdeki tüm kelimelere ait duygu puanlarını tespit etme  hata veriyorrrr
lutc_sentiment <- lutc %>%
  select(textOriginal) %>%
  unnest_tokens(word, textOriginal) %>%
  filter(!word %in% stop_words$word) %>%
  anti_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0) %>%
  top_n(5) # burada, ilk 20 kelimeyi seçin



#emoji sayısını bulma
lutc %>%
  select(textDisplay) %>%
  mutate(emoji_count = str_count(textDisplay, pattern = "[^[:ascii:]]")) %>%
  group_by(textDisplay) %>%
  summarize(mean_emoji_count = mean(emoji_count)) %>%
  slice(1:2) %>% # burada, ilk 20 sonucu seçin
  ggplot(aes(x = textDisplay, y = mean_emoji_count)) +
  geom_col()


#emoji tipleri
lutc_emoji_types <- lutc %>%
  select(textDisplay) %>%
  mutate(emoji_count = str_count(textDisplay, pattern = "[^[:ascii:]]")) %>%
  group_by(emoji_count) %>%
  summarize(count = n()) %>%
  rename(emoji_type = emoji_count)



#emoji ile analiz   
lutc_emoji <- lutc %>%
  select(textOriginal) %>% # burada, duygu analizi yapmak istediğiniz sütunu seçin
  unnest_tokens(word, textOriginal, drop = FALSE) %>%
  filter(word %in% emoji) %>%
  count(word, sort = TRUE) %>%
  head(20)

#Emoji duygu analizi 
lutc_emoji <- lutc %>%
  select(textOriginal) %>% # burada, duygu analizi yapmak istediğiniz sütunu seçin
  unnest_tokens(word, textOriginal, drop = FALSE) %>%
  filter(word %in% emoji) %>%
  count(word, sort = TRUE) %>%
  head(20)

#Emoji kullanımını gösteren bar chart oluşturma  
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip()

#Emoji kullanımını gösteren pie chart oluşturma   
lutc_emoji_types %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = "", y = n, fill = word)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme(axis.text.x = element_blank())


# Perform sentiment analysis on the textOriginal column of the lutc dataset
lutc_sentiment <- lutc %>%
  select(textOriginal) %>%
  mutate(sentiment = sentimentr::sentiment(textOriginal))







