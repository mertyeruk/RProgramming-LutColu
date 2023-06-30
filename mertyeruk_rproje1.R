install.packages("tuber")   #youtube ofisiyle bağlantı kurmak için. R bağlantı kurmuş oldu.
library(tuber)

yt_oauth("265878295206-sl99n53qji82v4r7sefqrqtbqmct8f62.apps.googleusercontent.com", "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")
#Google cloud trafından verilen veri çekme kodu


lutc <- get_all_comments(video_id = "zV7HEsXnMrU")  #istediğimiz videyu çekmek için video linkinin son kodları buraya yapıştırılır

View(lutc)  


##  write.csv(lutcoluyoutube, file='lutcoluyoutube.csv')  #çekteiğmiz verileri belgeler de kaydettik.

write.csv(lutc, "C:\\Users\\merty\\Desktop\\veriMadenciliği\\lutcoluyoutube.csv")

