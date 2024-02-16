library(readxl)
#read in the data file
read_excel(path=here("data-raw/2021_occupations_coded_government.xlsx"),
           col_types=c("text", "numeric", "numeric")) %>%
  #Set 0s to be missing in the two columns containing the NOC code
  mutate(across(2:3, function(x) car::Recode(x, "0=NA", as.factor=F)))->
  # Store as object noc
  noc


#| label: ces21-lower-merge-with-noc
ces21 %>%
  mutate(pes21_occ_text_lower=str_to_lower(pes21_occ_text)) %>%
  left_join(., noc, by=c("pes21_occ_text_lower"="title"))->ces21

