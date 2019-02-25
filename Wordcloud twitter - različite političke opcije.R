#Natural language processing in twitter

library(tm)
library(twitteR)
library(wordcloud)
library(RColorBrewer)
library(e1071)
library(class)

#Povezivanje na twitter


#Pretraživanje određene reči na tw - može da se bira broj tweetova i jezik

sns <- searchTwitter("sns", n=1000, lang = "sr")
savez <- searchTwitter("#1od5miliona", n=1000, lang = "sr")

head(sns)
#protestall <- searchTwitter("protest", n=1000)
#head(protestall)



#Uzimanje tekstualnih podataka iz tvitova

sns.tekst <- sapply(sns, function(x) x$getText())
savez.tekst <- sapply(savez, function(x) x$getText())

head(sns.tekst)
#Čišćenje podataka, brisanje emotikona, čuvanje samo u UTF formatu

#sns.tekst <- iconv(sns.tekst, "UTF-8", "latin1")
#savez.tekst <- iconv(savez.tekst, "UTF-8", "latin1")

head(sns.tekst)

#Kreiranje korpusa
sns.korpus <- Corpus(VectorSource(sns.tekst))
savez.korpus <- Corpus(VectorSource(savez.tekst))

#Dokument term matrica
term.doc.matrix1 <- TermDocumentMatrix(sns.korpus,
                                      control = list(removePunctuation = TRUE,
                                                     stopwords = c("je", "http","je", "ће", "коју","се", "још", "то", "није", "да", "за", "httpstconprvxivlb", "од", "би", "по", "су", "са", "је", "на", "што", "не", "био", "оног", "након", "све", "оно", "Није", "из"),
                                                     removeNumbers = TRUE,tolower = TRUE))

term.doc.matrix2 <- TermDocumentMatrix(savez.korpus,
                                      control = list(removePunctuation = TRUE,
                                                     stopwords = c("je", "http", "ће", "коју","се", "још", "то", "није", "да", "за", "httpstconprvxivlb", "од", "би", "по", "су", "са", "је", "на", "што", "не", "био", "оног", "након", "све", "оно", "Није", "из"),
                                                     removeNumbers = TRUE,tolower = TRUE))

term.doc.matrix1 <- as.matrix(term.doc.matrix1)
term.doc.matrix2 <- as.matrix(term.doc.matrix2)
head(term.doc.matrix)

#Brojimo reči
word.freq1 <- sort(rowSums(term.doc.matrix1), decreasing = TRUE)
dm1 <- data.frame(word=names(word.freq1), freq = word.freq1)

word.freq2 <- sort(rowSums(term.doc.matrix2), decreasing = TRUE)
dm2 <- data.frame(word=names(word.freq2), freq = word.freq2)

#Create wordcloud
#Za sns
wordcloud(dm1$word, dm1$freq, random.order=FALSE)

wordcloud(dm1$word, dm1$freq, max.words =50, min.freq=2, scale=c(1, 1), random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#za savez
wordcloud(dm2$word, dm2$freq, random.order=FALSE)

wordcloud(dm2$word, dm2$freq, max.words = 50, random.order = FALSE)

