
#### Diagnostic section for accented characters ####
ces19web$cps19_prov_id
#Note value labels are cut-off at accented characters in Quebec. 
#I know this occupation has messed up characters
ces19web %>% 
  filter(str_detect(pes19_occ_text,"assembleur-m")) %>% 
  select(cps19_ResponseId, pes19_occ_text)
#Check the encodings of the occupation titles and store in a variable encoding
ces19web$encoding<-Encoding(ces19web$pes19_occ_text)
#Check encoding of problematic characters
ces19web %>% 
  filter(str_detect(pes19_occ_text,"assembleur-m")) %>% 
  select(cps19_ResponseId, pes19_occ_text, encoding) 


#Try to fix
#Run this function from Stack overflow
#source("fix_encodings.R")
for (n in names(ces19web)) {
  v <- ces19web[[n]]
  if (is.character(v)) {
    v <- iconv(v, from = "UTF-8", to = "latin1") 
    Encoding(v) <- "UTF-8"
  }
  ces19web[[n]] <- v
}

#Examine
ces19web %>% 
  filter(str_detect(pes19_occ_text,"assembleur-m")) %>% 
  select(cps19_ResponseId, pes19_occ_text, encoding) 

#Check
ces19web %>% 
  filter(str_detect(pes19_occ_text,"Ã|©")) %>% 
  select(cps19_ResponseId, pes19_occ_text, encoding) %>% 
head()

