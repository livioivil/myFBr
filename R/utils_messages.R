get_group_threads <- function(mess){
  nomi_threads=strsplit(levels(mess$thread),",")
  gruppo = mess$thread
  thread_gruppo=sapply(nomi_threads,length)>2
  levels(gruppo)=thread_gruppo
  gruppo
}

get_owner_from_threads <- function(mess){
  nomi_threads=strsplit(levels(mess$thread),",")
  nome_utente=names(which.max(table(unlist(nomi_threads[mess$gruppo=="FALSE"]))))
  nome_utente=gsub("^ ","",nome_utente)
  nome_utente=gsub(" $","",nome_utente)
  nome_utente
}


escludiCaratteriFinaliReplicatiPiuVolte <- function(testo,
                                                    listaCatatteri=c("a","e","i","o","u")){
  for(char in listaCatatteri){
    testo=gsub(paste(char,"+ ",sep=""),paste(char," ",sep=""),testo)
    testo=gsub(paste(char,"+$",sep=""),char,testo)
  }
  testo
}

char_emoticons=c(EMOTEGOOD="\\:\\)+|\\:\\-\\)+|\\:\\]+|\\:\\-\\]+|\\=\\)+|\\=\\]+|\\=\\>|\\:\\>|\\^\\^|\\^\\_+\\^|\\^\\-\\^|\\^o\\^|\\:[[:blank:]]\\)+|[[:blank:]]\\([[:blank:]]?\\:|\\:\\'D+",
                 EMOTELOVE="\\<3+|<U+2764>|<U+2665>|\\:\\*+",
                 EMOTEBAD="\\:\\(+|\\:\\-\\(+|\\:\\[+|\\:\\-\\[+|\\=\\[+|\\=\\(+|\\:[[:blank:]]\\(|[[:blank:]]\\([[:blank:]]?\\:|\\:\\'+\\(+|\\:\\'\\[|D\\:|\\:\\-\\[|\\:\\|\\:/+|\\=/+|\\:x|\\#\\_+\\#|X\\_+X|x\\_+x|X\\.X|x\\.x|>\\.<|>\\_+<|>\\_+>|>\\.>",
                 EMOTEWINK="\\;\\)+|\\;\\-\\)+|\\;\\]|\\;\\-\\]|\\;\\>|;d+|;D+|;o", 
                 EMOTESHOCK="O\\.o|o\\.o|O\\.O|o\\.O|O\\_+o|o\\_+o|O\\_+O|o\\_+O|\\:OO+|\\=O+|\\-\\.\\-|u\\.u|u\\.ù|ù\\.u|u\\_+u|çç|ç_+ç|t_+t|ù\\_+ù|ù\\.ù|\\:oo+|0\\_+0|\\=\\_+\\=|\\.\\_+\\.|òò|ò\\_+ò|\\*u+\\*|\\-\\_+\\-|ùù|\\-\\,\\-|\\-\\-\\'|\\.\\-\\.|\\'\\-\\'", 
                 EMOTEAMAZE="\\:P+[^e]|\\:p+[^e]|\\=P+|\\=p+|XD+|xD+|xd+|[[:blank:]]d\\:|\\:P+[^e]|\\:p+[^e]|\\=P+|\\=p+|XD+|xD+|xd+|[[:blank:]]d\\:")

char_non_word=c(domanda="\\?", esclamativo="\\!", virgole_punti="(,|;|\\.)")

char_wow=c(EMOTEZZZ 	="(#?zz+|#?u+ff[aif]+?|#?r+o+n+f+|#uff|ronf)",
           EMOTESIII 	="(#?sii+|#si+|#?yes+|#?s\uc38c\uc38c+)",
           EMOTENOOO 	="(#?noo+|#no+|#?nuu+)",
           EMOTEAHHH 	="(#?ahh+)",
           EMOTEEHHH 	="(#?ehh+)",
           EMOTEOHOH 	="(#?o?h?o+h[oh]+)",
           EMOTEIHIH 	="(#?i?h?i+h[ih]+)",
           EMOTEUHUH 	="(#?i?h?i+h[ih]+)",
           EMOTEAHAH 	="(#?a?h?ah[^h][ah]+)",
           EMOTEEHEH 	="(#?e?h?eh[^h][eh]+)",
           EMOTEAZZ 	="(#?azz+)",
           EMOTEDAIII 	="(#?[dv]aii+|#[dv]ai|forzaa+)",
           cazzo 	="(#?cazz[oi]+)",
           cazzata 	="(#?cazzat[a]+)",
           merda 	="(#?merd[a]+)",
           cazzo	="ca\\*\\*o|c\\*\\*\\*+o",
           EMOTEAAA 	="(#?aaa+)",
           EMOTEOOO 	="(#?ooo+)",
           EMOTEEEE 	="(#?eee+)",
           EMOTELOL 	="(#?l+o+l+|#?r+o+f+t+l+)",
           EMOTESOS 	="(#?aiutoo+|#?sos|#?help+)",
           EMOTEBASTA 	="(#ba+sta+|ba+staa+)",
           EMOTEWOW 	="#?([uw]+[ao]+[uw]+)")


summary.txts <- function(txts,nMostFrequentWords=Inf){
  #   txts=posts
  nchar.mess=sapply(txts,nchar)
  # hist(nchar.mess,xlim=c(1,100),10000)
  statistics=c(messaggi.totali=length(txts),  caratteri.totali=sum(nchar.mess), 
               caratteri.per.messaggio=summary(nchar.mess))
  #   print(summary)
  #   quantiles.nchar=quantile(nchar.mess,c(.1,.25,.5,.75))
  
  posts.norm=normalizzaTesti(txts,contaStringhe = c("\\?","\\!"),normalizzaslang = TRUE,normalizzahtml = TRUE)
  freqDomEsc=colSums(attributes(posts.norm)$counts)

  stringhe=c(char_emoticons,char_wow,char_non_word)  
  #library(tm)
  corpus <- tm::Corpus(VectorSource(paste(posts.norm,collapse = " ")))
  #[Nell'ambito dei motori di ricerca, l'espressione - scritta anche stopwords - indica quelle parole che, per la loro alta frequenza in una lingua, sono di solito ritenute poco significative dai motori, che le ignorano]
  freq_emoticons <- as.matrix(DocumentTermMatrix(corpus
                                                 , control = list( stemming = FALSE, 
                                                                   dictionary=stringhe) )
  )
  colnames(freq_emoticons)=names(freq_emoticons)
  freq_emoticons=data.frame(freq_emoticons)
  posts.norm=removeStopwords(posts.norm, c(names(stringhe),stopwords("en"),
                                           "the","just",
                                           "ok","va","non",
                                           stopwords("es"),itastopwords,
                                           "est","et"))
  posts.norm=escludiCaratteriFinaliReplicatiPiuVolte(posts.norm)
  posts.words=unlist(strsplit(posts.norm," "))
  posts.words=posts.words[sapply(posts.words,function(x)x!="")]
  tabParole=table(posts.words)
  freq_links=tabParole["wwwurlwww"]
  tabParole=tabParole[setdiff(names(tabParole),"wwwurlwww")]
  most.word=sort(tabParole,decreasing = TRUE)
  
  list(freq_parole=most.word[1:min(length(most.word),nMostFrequentWords)],
       statistics=statistics,
       freq_emoticons=freq_emoticons,
       freq_links =freq_links)
}


getMessages_summary_string_counts <- function(mess,stringhe=NULL){
  
  if(is.null(stringhe)) stringhe=c(myFBr:::char_emoticons,myFBr:::char_non_word,myFBr:::char_wow)
  
  mess$gruppo <- myFBr:::get_group_threads(mess)
  # table(mess$gruppo)
  nome_utente = myFBr:::get_owner_from_threads(mess)
  
  fun_temp <- function(dati){
    conteggi = plyr::llply(stringhe, function(stringa) stringr::str_count(mess_utente$text, 
                                                                   stringa))
    colSums(as.data.frame(conteggi))
  }
  list(user_grp=fun_temp(mess[(mess$user==nome_utente)&(mess$gruppo),]),
       user_personal=fun_temp(mess[(mess$user==nome_utente)&(!mess$gruppo),]),
       others_grp=fun_temp(mess[(mess$user!=nome_utente)&(mess$gruppo),]),
  others_personal=fun_temp(mess[(mess$user!=nome_utente)&(!mess$gruppo),]))
}