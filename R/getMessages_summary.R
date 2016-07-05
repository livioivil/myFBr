#' @names getMessages_summary
#' @title getMessages_summary
#' @param mess result of a call of \code{getMessages}.
#' @param percorso stringa che indica il percorso della cartella dei dati del profilo
#' @return a list with few summary statistics
#' @export getMessages_summary
#' @aliases getMessages_summary
#'
#' @author Caterina Ciampanelli, Livio Finos

getMessages_summary=function(mess){
  # testi=mess$text
  n_mess=length(mess$text)
  # user=mess$user
  tab_user= sort(table(mess$user),decreasing = TRUE)
  n_user=length(tab_user)
  mess_medi=n_mess/n_user
  freq_user=tab_user[1:3]
  
  freq_ore=table(format(mess$time, "%H"))
  # barplot(freq_ore,las=3)
  
  freq_data=table(as.Date(mess$time))
  # barplot(freq_data,las=3)
  
  freq_giorni=table(weekdays(mess$time))
  freq_giorni=freq_giorni[c("lunedì","martedì","mercoledì","giovedì","venerdì","sabato","domenica")]
  # barplot(freq_giorni[1:7])
  
  a=summary.txts(mess$text)
  
  Gini=Gini(mess$user)
  
  #alto-->tutte le modalità hanno numerosità simili
  #basso-->la frequenza totale è concentrata in poche modalità
  data(vocabolarioNomiPropri)
  genere=TextWiller::classificaUtenti(c(names(table(mess$user))),vocabolario=vocabolarioNomiPropri)
  indice1=which(genere=="masc")
  indice2=which(genere=="femm")
  maschi=length(indice1)
  femmine=length(indice2)
  
  
  v=list(n_mess=n_mess,n_user=n_user,Gini=Gini,maschi=maschi,femmine=femmine,
         char_info=a$statistics,freq_user=freq_user,
         freq_ore=freq_ore,freq_giorni=freq_giorni,
         freq_emoticons=a$freq_emoticons,most_words=sort(a$freq_parole,decreasing = TRUE)[1:10])
  v=as.vector(v,mode="any")
  return(v)
}