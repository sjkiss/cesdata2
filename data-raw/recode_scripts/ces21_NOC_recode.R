library(readxl)
#read in the data file
read_excel(path=here("data-raw/2021_occupations_coded_government.xlsx"),
           col_types=c("text", "numeric", "numeric")
           ) %>%
  #Set 0s to be missing in the two columns containing the NOC code
  mutate(across(2:3, function(x) car::Recode(x, "0=NA", as.factor=T)))->
  # Store as object noc
  noc


#| label: ces21-lower-merge-with-noc
ces21$pes21_occ_text
ces21 %>%
  mutate(pes21_occ_text_lower=str_to_lower(pes21_occ_text)) %>%
  left_join(., noc, by=c("pes21_occ_text_lower"="title"))->ces21

# Get job titles
library(openxlsx)
noc_2021_4_titles<-read.xlsx(xlsxFile = "data-raw/NOC_2021_4_job_titles.xlsx")
noc_2021_5_titles<-read.xlsx(xlsxFile = "data-raw/NOC_2021_5_job_titles.xlsx")


ces21 %>%
  left_join(., noc_2021_5_titles, by=c("NOC21_5")) %>%
  left_join(., noc_2021_4_titles) %>%
  # select(contains("NOC21")) %>%
  select(-contains('Description')) ->ces21

