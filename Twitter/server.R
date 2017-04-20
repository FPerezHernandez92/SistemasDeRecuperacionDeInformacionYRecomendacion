
# Instalar paquetes si no se pueden cargar
EnsurePackage<-function(x){
  x <- as.character(x)
  if (!require(x,character.only=TRUE)){
      #install.packages(pkgs=x,repos="http://cran.r-project.org")
      require(x,character.only=TRUE)
    }
}

library(wordcloud)

#Cargar paquetes
PrepareTwitter<-function(){
  EnsurePackage("twitteR")
  EnsurePackage("stringr")
  EnsurePackage("ROAuth")
  EnsurePackage("RCurl")
  EnsurePackage("ggplot2")
  EnsurePackage("reshape")
  EnsurePackage("tm")
  EnsurePackage("RJSONIO")
  EnsurePackage("wordcloud")
  EnsurePackage("gridExtra")
  #EnsurePackage("gplots") Not required... ggplot2 is used
  EnsurePackage("plyr")
}
PrepareTwitter()

#Authenticate Access to Twitter
source('credenciales.R')

#Ejecutamos el servidor
shinyServer(function(input, output) {
  #TABLE
  #Search tweets and create a data frame -Stanton (2013)
  # Clean the tweets
  library(sentiment)
  library(ggplot2)
  library(dplyr)
  library(purrr)
  library(twitteR)
  library(rsconnect)
  rsconnect::setAccountInfo(name='masterdatcom2017',
                            token='19DC6E57271120AA7B6345A293C97AD3',
                            secret='JlyRO5yazn1t/ckLcoE0pNLY46yJpmz2qzfiUbI1')
  TweetFrame<-function(twtList){
    df<- do.call("rbind",lapply(twtList,as.data.frame))
    #removes emoticons
    df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text)
    cat("tweets sacados\n")
    return (df$text)
  }
  #ADD
  TweetFrameLimpio<-function(twtList){
    df<- do.call("rbind",lapply(twtList,as.data.frame))
    #removes emoticons
    df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    df$text = df$text
    df$text = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', df$text)
    df$text = gsub('@\\w+', '', df$text)
    df$text = gsub('[[:punct:]]', '', df$text)
    df$text = gsub('[[:digit:]]', '', df$text)
    df$text = gsub('http\\w+', '', df$text)
    df$text = gsub('[ \t]{2,}', '', df$text)
    df$text = gsub('^\\s+|\\s+$', '', df$text)
    
    #Función para eliminar posibles errores al pasar a minúscula
    try.error = function(x){
      # creamos un missing value
      y = NA
      # tryCatch error
      try_error = tryCatch(tolower(x), error=function(e) e)
      # if not un error
      if (!inherits(try_error, 'error'))
        y = tolower(x)
      return(y)
    }
    df$text = sapply(df$text, try.error)
    df$text = df$text[!is.na(df$text)]
    names(df$text) = NULL
    cat("tweets sacados y limpios\n")
    return (df$text)
  }
  
  # Function to create a data frame from tweets, Stanton 2013
  pos.words=scan('positive-words.txt', what='character',comment.char=';')
  neg.words=scan('negative-words.txt', what='character',comment.char=';')
  
  wordDatabase<-function()
  {
    pos.words<<-c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader')
    neg.words<<-c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')
  }
  
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    list=lapply(sentences, function(sentence, pos.words, neg.words)
    {
      sentence = gsub('[[:punct:]]',' ',sentence)
      sentence = gsub('[[:cntrl:]]','',sentence)
      sentence = gsub('\\d+','',sentence)
      sentence = gsub('\n','',sentence)
      
      sentence = tolower(sentence)
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      pp=sum(pos.matches)
      nn = sum(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      list1=c(score, pp, nn)
      return (list1)
    }, pos.words, neg.words)
    score_new=lapply(list, `[[`, 1)
    pp1=score=lapply(list, `[[`, 2)
    nn1=score=lapply(list, `[[`, 3)
    
    scores.df = data.frame(score=score_new, text=sentences)
    positive.df = data.frame(Positive=pp1, text=sentences)
    negative.df = data.frame(Negative=nn1, text=sentences)
    
    list_df=list(scores.df, positive.df, negative.df)
    return(list_df)
  }
  
  library(reshape)
  sentimentAnalyser<-function(result)
  {
    #Creating a copy of result data frame
    test1=result[[1]]
    test2=result[[2]]
    test3=result[[3]]
    
    #Creating three different data frames for Score, Positive and Negative
    #Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    #Storing the first row(Containing the sentiment scores) in variable q
    q1=test1[1,]
    q2=test2[1,]
    q3=test3[1,]
    qq1=melt(q1, ,var='Score')
    qq2=melt(q2, ,var='Positive')
    qq3=melt(q3, ,var='Negative') 
    qq1['Score'] = NULL
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    #Creating data frame
    table1 = data.frame(Text=result[[1]]$text, Score=qq1)
    table2 = data.frame(Text=result[[2]]$text, Score=qq2)
    table3 = data.frame(Text=result[[3]]$text, Score=qq3)
    
    #Merging three data frames into one
    table_final=data.frame(Text=table1$Text, Positive=table2$value, Negative=table3$value, Score=table1$value)
    return(table_final)
  }
  
  percentage<-function(table_final)
  {
    #Positive Percentage
    
    #Renaming
    posSc=table_final$Positive
    negSc=table_final$Negative
    
    #Adding column
    table_final$PosPercent = posSc/ (posSc+negSc)
    
    #Replacing Nan with zero
    pp = table_final$PosPercent
    pp[is.nan(pp)] <- 0
    table_final$PosPercent = pp*100
    
    #Negative Percentage
    
    #Adding column
    table_final$NegPercent = negSc/ (posSc+negSc)
    
    #Replacing Nan with zero
    nn = table_final$NegPercent
    nn[is.nan(nn)] <- 0
    table_final$NegPercent = nn*100
    
    return(table_final)
  }
  
  wordDatabase()
  
  twtList<-reactive({twtList<-searchTwitter(input$searchTerm, n=input$maxTweets, lang="en") })
  tweets<-reactive({tweets<-TweetFrame(twtList() )})
  #ADD
  tweets_limpios <- reactive({tweets_limpios<-TweetFrameLimpio(twtList() )})
  
  result<-reactive({result<-score.sentiment(tweets(), pos.words, neg.words, .progress='none')})
  
  table_final<-reactive({table_final<-sentimentAnalyser(  result() )})
  table_final_percentage<-reactive({table_final_percentage<-percentage(  table_final() )})
  
  output$tabledata<-renderTable(table_final_percentage())
  
  #TOP TRENDING TWEETS
  
  toptrends<-function(place)
  {
    a_trends = availableTrendLocations()
    woeid = a_trends[which(a_trends$name==place),3]
    trend = getTrends(woeid)
    trends = trend[1:2]
    
    #To clean data and remove Non English words: (not required)
    dat <- cbind(trends$name)
    dat2 <- unlist(strsplit(dat, split=", "))
    dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
    dat4 <- dat2[-dat3]
    #dat5 <- trends[,which(trends$name==dat4)]
    return(dat4)
  }
  
  trend_table<-reactive({trend_table<-toptrends(  input$trendingTable ) })
  output$trendtable<-renderTable(trend_table() )
  
  #TOP TRENDING TWEETS
  
  toptrends<-function(place)
  {
    a_trends = availableTrendLocations()
    woeid = a_trends[which(a_trends$name==place),3]
    trend = getTrends(woeid)
    trends = trend[1:2]
    
    #To clean data and remove Non English words: (not required)
    dat <- cbind(trends$name)
    dat2 <- unlist(strsplit(dat, split=", "))
    dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
    if(dat3==0)
      return(dat2)
    dat4 <- dat2[-dat3]
    
    return(dat4)
  }
  
  trend_table<-reactive({trend_table<-toptrends(  input$trendingTable ) })
  output$trendtable<-renderTable(trend_table() )	
  
  #WORDCLOUD
  

  
  
  wordclouds <- function(texto_tweets){
    clasificacion_emociones = classify_emotion(texto_tweets, algorithm='bayes', prior=1.0)
    emociones = clasificacion_emociones[,7]
    # sustituimos NA's por 'unknown'
    emociones[is.na(emociones)] = 'unknown'
    clasificacion_popularidad = classify_polarity(texto_tweets, algorithm='bayes')
    popularidad = clasificacion_popularidad[,4]
    data = data.frame(text=texto_tweets, emotion=emociones, polarity=popularidad, stringsAsFactors=FALSE)
    data = within(data, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
    emociones_data = levels(factor(data$emotion))
    tama_emociones_data = length(emociones_data)
    documento_emociones = rep('', tama_emociones_data)
    for (i in 1:tama_emociones_data)
    {
      tmp = texto_tweets[emociones == emociones_data[i]]
      documento_emociones[i] = paste(tmp, collapse=' ')
    }
    
    # eliminamos palabras vacias como or,as,off...
    documento_emociones = removeWords(documento_emociones, stopwords('english'))
    # Creamos el corpus
    corpus = Corpus(VectorSource(documento_emociones))
    termdocumentmatrix = TermDocumentMatrix(corpus)
    termdocumentmatrix = as.matrix(termdocumentmatrix)
    colnames(termdocumentmatrix) = emociones_data
    
    # comparison word cloud
    
    png("./images/nube.png", width=568, height=606)
    comparison.cloud(termdocumentmatrix, colors = brewer.pal(tama_emociones_data, 'Dark2'),
                     scale = c(3,.5), random.order = FALSE, title.size = 1.5)
    dev.off()
  }
  sacar_nube <- reactive({sacar_nube<-wordclouds(tweets_limpios() )})
  output$word <- renderImage({
    wordclouds(tweets_limpios())
    filename <- normalizePath(file.path('./images',
                                        paste('nube', '.png', sep='')))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  #HISTOGRAM
  
  output$histPos<- renderPlot({hist(table_final()$Positive, col=rainbow(10), main = "Histogram of Positive Sentiment", xlab = "Positive Score") })
  output$histNeg<- renderPlot({hist(table_final()$Negative, col=rainbow(10), main = "Histogram of Negative Sentiment", xlab = "Negative Score") })
  output$histScore<- renderPlot({hist(table_final()$Score, col=rainbow(10), main = "Histogram of Score", xlab = "Overall Score")})
  
  #ANALISIS DE SENTIMIENTOS EMOCIONES (ADD)
  AnalisisSentimientos <- function(texto_tweets, i){
    clasificacion_emociones = classify_emotion(texto_tweets, algorithm='bayes', prior=1.0)
    emociones = clasificacion_emociones[,7]
    # sustituimos NA's por 'unknown'
    emociones[is.na(emociones)] = 'unknown'
    clasificacion_popularidad = classify_polarity(texto_tweets, algorithm='bayes')
    popularidad = clasificacion_popularidad[,4]
    data = data.frame(text=texto_tweets, emotion=emociones, polarity=popularidad, stringsAsFactors=FALSE)
    data = within(data, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
    if (i==0){
      ggplot(data, aes(x=emotion)) +
        geom_bar(aes(y=..count.., fill=emotion)) +
        scale_fill_brewer(palette='Dark2') +
        labs(x='Emotion categories', y='Number of tweets') +
        ggtitle('Sentiment Analysis of Tweets about Brexit\n(classification by emotion)') +
        theme(plot.title = element_text(size=12, face='bold'))
      ggsave("./images/emociones.png", dpi=80)
    }
    else {
      ggplot(data, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
        scale_fill_brewer(palette='RdGy') +
        labs(x='Polarity categories', y='Number of tweets') +
        ggtitle('Sentiment Analysis of Tweets about Brexit\n(classification by popularity)') +
        theme(plot.title = element_text(size=12, face='bold'))
      ggsave("./images/popularidad.png", dpi=80)
    }
  }
  sacar_sentimientos <- reactive({sacar_sentimientos<-AnalisisSentimientos(tweets_limpios() )})
  output$emociones <- renderImage({
    AnalisisSentimientos(tweets_limpios() ,0)
    filename <- normalizePath(file.path('./images',
                                        paste('emociones', '.png', sep='')))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  output$popularidad <- renderImage({
    AnalisisSentimientos(tweets_limpios() ,1)
    filename <- normalizePath(file.path('./images',
                                        paste('popularidad', '.png', sep='')))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  #PIE CHART
  
  slices <- reactive({c(sum(table_final()$Positive), sum(table_final()$Negative)) })
  labels <- c("Positive", "Negative")
  library(plotrix)
  #pie(slices(), labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
  output$piechart<-renderPlot({pie3D(slices(), labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis") })
  
  #TOP TWEETERS
  
  # Top tweeters for a particular hashtag (Barplot)
  toptweeters<-function(tweetlist)
  {
    tweets <- twListToDF(tweetlist)
    tweets <- unique(tweets)
    # Make a table of the number of tweets per user
    d <- as.data.frame(table(tweets$screenName)) 
    d <- d[order(d$Freq, decreasing=T), ] #descending order of tweeters according to frequency of tweets
    names(d) <- c("User","Tweets")
    return (d)
  }
  
  # Plot the table above for the top 20
  
  d<-reactive({d<-toptweeters(  twtList() ) })
  output$tweetersplot<-renderPlot ( barplot(head(d()$Tweets, 20),  names=head(d()$User, 20), las=2, horiz=F, main="Top 20: Tweets per User", col=1) )
  output$tweeterstable<-renderTable(head(d(),20))
  
  #Top 10 TWEETERS
  
  tw1 <- reactive({ tw1 = userTimeline(input$user, n = 3200) })
  tw <- reactive({ tw = twListToDF(tw1()) })
  vec1<-reactive ({ vec1 = tw()$text })
  
  extract.hashes = function(vec){
    
    hash.pattern = "#[[:alpha:]]+"
    have.hash = grep(x = vec, pattern = hash.pattern)
    
    hash.matches = gregexpr(pattern = hash.pattern,
                            text = vec[have.hash])
    extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
    
    df = data.frame(table(tolower(unlist(extracted.hash))))
    colnames(df) = c("tag","freq")
    df = df[order(df$freq,decreasing = TRUE),]
    return(df)
  }
  
  dat<-reactive({ dat = head(extract.hashes(vec1()),50) })
  dat2<- reactive ({ dat2 = transform(dat(),tag = reorder(tag,freq)) })
  
  p<- reactive ({ p = ggplot(dat2(), aes(x = tag, y = freq)) + geom_bar(stat="identity", fill = "blue")
  p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the tweeter") })
  output$tophashtagsplot <- renderPlot ({ p() })	
  
  #COMPARATIVA ANDROID IPHONE Y IPAD
  #POR PALABRAS
  por_palabras <- function(tweets){
    tweets_df <- tbl_df(map_df(tweets, as.data.frame))
    library(tidyr)
    tweets_iph_an_ipa <- tweets_df %>%
      select(id, statusSource, text, created) %>%
      extract(statusSource, "source", "Twitter for (.*?)<") %>%
      filter(source %in% c("iPhone", "Android", "iPad"))
    num_iphone <- nrow(tweets_df %>%
                         select(id, statusSource, text, created) %>%
                         extract(statusSource, "source", "Twitter for (.*?)<") %>%
                         filter(source %in% "iPhone"))
    num_android <- nrow(tweets_df %>%
                          select(id, statusSource, text, created) %>%
                          extract(statusSource, "source", "Twitter for (.*?)<") %>%
                          filter(source %in% "Android"))
    num_ipad <- nrow(tweets_iph_an_ipa) - num_iphone - num_android
    num_iphone
    num_android
    num_ipad
    library(stringr)
    tweet_picture_counts <- tweets_iph_an_ipa %>%
      filter(!str_detect(text, '^"')) %>%
      count(source,
            picture = ifelse(str_detect(text, "t.co"),
                             "Picture/link", "No picture/link"))
    
    ggplot(tweet_picture_counts, aes(source, n, fill = picture)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "", y = "Number of tweets", fill = "")
    ggsave("./images/palabras1.png", dpi=80)
   library(tidytext)
    reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
    tweet_words <- tweets_iph_an_ipa %>%
      filter(!str_detect(text, '^"')) %>%
      mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
      unnest_tokens(word, text, token = "regex", pattern = reg) %>%
      filter(!word %in% stop_words$word,
             str_detect(word, "[a-z]"))
    tweet_words
    tweet_words %>%
      count(word, sort = TRUE) %>%
      head(20) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_bar(stat = "identity") +
      ylab("Occurrences") +
      coord_flip()
    ggsave("./images/palabras2.png", dpi=80)
    
    t_words_android <- tweet_words[which(tweet_words$source=="Android"),]
    t_words_iphone <- tweet_words[which(tweet_words$source=="iPhone"),]
    t_words_ipad <- tweet_words[which(tweet_words$source=="iPad"),]
    
    t_words_android %>%
      count(word, sort = TRUE) %>%
      head(20) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_bar(stat = "identity") +
      ylab("Occurrences Android") +
      coord_flip()
    
    ggsave("./images/palabras3.png", dpi=80)
    
    t_words_iphone %>%
      count(word, sort = TRUE) %>%
      head(20) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_bar(stat = "identity") +
      ylab("Occurrences iPhone") +
      coord_flip()
    
    ggsave("./images/palabras4.png", dpi=80)
    
    t_words_ipad %>%
      count(word, sort = TRUE) %>%
      head(20) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_bar(stat = "identity") +
      ylab("Occurrences iPad") +
      coord_flip()
    
    ggsave("./images/palabras5.png", dpi=80)
  }
  por_palabras1 <- reactive({por_palabras1<-por_palabras(twtList() )})
  output$palabras1 <- renderImage({
    por_palabras(twtList())
    filename <- normalizePath(file.path('./images',
                                        paste('palabras1', '.png', sep='')))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  output$palabras2 <- renderImage({
    por_palabras(twtList())
    filename <- normalizePath(file.path('./images',
                                        paste('palabras2', '.png', sep='')))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$palabras3 <- renderImage({
    por_palabras(twtList())
    filename <- normalizePath(file.path('./images',
                                        paste('palabras3', '.png', sep='')))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$palabras4 <- renderImage({
    por_palabras(twtList())
    filename <- normalizePath(file.path('./images',
                                        paste('palabras4', '.png', sep='')))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$palabras5 <- renderImage({
    por_palabras(twtList())
    filename <- normalizePath(file.path('./images',
                                        paste('palabras5', '.png', sep='')))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  
}) #shiny server