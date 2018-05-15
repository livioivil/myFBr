#' @name getMessages_summary
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
  mess_medi_friend=n_mess/n_user
  freq_user=tab_user[1:3]/n_mess
  names(freq_user)=c("freqFriend1","freqFriend2","freqFriend3")
  
  summary_freq_evets=summary_freq_events_week_hour(mess)
  a=summary.txts(mess$text)
  
  Gini=Gini(mess$user)
  
  #alto-->tutte le modalità hanno numerosità simili
  #basso-->la frequenza totale è concentrata in poche modalità
  require(TextWiller)
  data(vocabolarioNomiPropri)
  genere=TextWiller::classificaUtenti(c(names(table(mess$user))),vocabolario=vocabolarioNomiPropri)
  indice1=which(genere=="masc")
  indice2=which(genere=="femm")
  maschi=length(indice1)
  femmine=length(indice2)
  
  
  v=list(n_user=n_user,
         mess_medi_friend=mess_medi_friend,
         nchar_stats=a,
         Gini_freq_user=Gini,
         MFprops=c(maschi=maschi/(maschi+femmine),
                   femmine=femmine/(maschi+femmine)),
         freq_user=freq_user,
         freq_ore=summary_freq_evets$freq_ore,
         freq_giorni=summary_freq_evets$freq_giorni,
         n_days_active=summary_freq_evets$n_days_active)
  v=as.vector(v,mode="any")
  return(v)
}