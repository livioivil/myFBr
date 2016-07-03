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

char_emoticons=c(EMOTEGOOD="\\:\\)+|\\:\\-\\)+|\\:\\]+|\\:\\-\\]+|\\=\\)+|\\=\\]+|\\=\\>|\\:\\>|\\^\\^|\\^\\_+\\^|\\^\\-\\^|\\^o\\^|\\:[[:blank:]]\\)+|[[:blank:]]\\([[:blank:]]?\\:|\\:\\'D+",
                 EMOTELOVE="\\<3+|<U+2764>|<U+2665>|\\:\\*+",
                 EMOTEBAD="\\:\\(+|\\:\\-\\(+|\\:\\[+|\\:\\-\\[+|\\=\\[+|\\=\\(+|\\:[[:blank:]]\\(|[[:blank:]]\\([[:blank:]]?\\:|\\:\\'+\\(+|\\:\\'\\[|D\\:|\\:\\-\\[|\\:\\|\\:/+|\\=/+|\\:x|\\#\\_+\\#|X\\_+X|x\\_+x|X\\.X|x\\.x|>\\.<|>\\_+<|>\\_+>|>\\.>",
                 EMOTEWINK="\\;\\)+|\\;\\-\\)+|\\;\\]|\\;\\-\\]|\\;\\>|;d+|;D+|;o", 
                 EMOTESHOCK="O\\.o|o\\.o|O\\.O|o\\.O|O\\_+o|o\\_+o|O\\_+O|o\\_+O|\\:OO+|\\=O+|\\-\\.\\-|u\\.u|u\\.ù|ù\\.u|u\\_+u|çç|ç_+ç|t_+t|ù\\_+ù|ù\\.ù|\\:oo+|0\\_+0|\\=\\_+\\=|\\.\\_+\\.|òò|ò\\_+ò|\\*u+\\*|\\-\\_+\\-|ùù|\\-\\,\\-|\\-\\-\\'|\\.\\-\\.|\\'\\-\\'", 
                 EMOTEAMAZE="\\:P+[^e]|\\:p+[^e]|\\=P+|\\=p+|XD+|xD+|xd+|[[:blank:]]d\\:|\\:P+[^e]|\\:p+[^e]|\\=P+|\\=p+|XD+|xD+|xd+|[[:blank:]]d\\:")
