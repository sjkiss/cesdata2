#### Recode script for ces25 most important problem
#### REVISED to align dictionaries with the 2021 coding scheme (codes 1-20)
#### 2021 scheme: Other=0, Environment=1, Crime=2, Ethics=3, Education=4,
#### Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
#### Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14,
#### Social_Programs=15, Brokerage=16, Free_Trade=17, Inflation=18,
#### Housing=19, COVID19=20

# load dataset
data("ces25")

## loading packages
library(tidyverse)
library(haven)
library(magrittr)
library(tidytext)
library(quanteda)
library(tm)
library(SnowballC)
library(crayon)
library(progressr)
library(progress)
library(lubridate)
library(quanteda.textstats)
library(tictoc)
library(gtsummary)


## Function (unchanged)
runDictionary <- function(dataA, word, dictionaryA) {
  tictoc::tic()
  dataA <- dataA %>%
    mutate(word = {{word}})
  corpusA <- tokens(dataA$word)
  dfmA <- dfm(tokens_lookup(corpusA,
                            dictionaryA,
                            nested_scope = "dictionary"))
  pb <- progress_bar$new(
    format = yellow(" downloading [:bar] :percent in :elapsed"),
    total = 100, clear = FALSE, width = 60)
  purrr::walk(1:100, ~{pb$tick(); Sys.sleep(0.01)})
  message(green("100% expressions/words found"))
  tictoc::toc()
  dataB <- convert(dfmA, to = "data.frame")
  return(dataB)
}

# Text cleaning (unchanged): lowercase, strip punctuation
ces25$cps25_imp_iss <- stringr::str_replace_all(ces25$cps25_imp_iss, "[A-Z]",
                                                ~tolower(.x))
ces25$cps25_imp_iss <- sub("ˆ(\\w+)\\s+(\\w+)$", "\\2 \\1", ces25$cps25_imp_iss)
ces25$cps25_imp_iss <- gsub("[[:punct:]]", "", ces25$cps25_imp_iss)


## =====================================================================
## 1. ENVIRONMENT  (oil/gas/pipeline removed -> Energy)
## =====================================================================
dict_enviro <- dictionary(list(enviro = c(
  "environ*", "climat*", "écolog*", "ecolog*", "envir*", "pollut*",
  "environnement", "environmental", "environment", "l'environnement",
  "climate", "climate change", "warming", "climatiques", "rechauffement",
  "enviroment", "enviromental", "ecology", "co2", "polluer", "pollute",
  "pollution", "planet", "planète", "earth", "green", "greener",
  "sustainability", "water", "renewable*", "ges", "environnemental",
  "lenvironnement", "enviournment", "envioroment", "enviornment",
  "enironment", "environement", "environmentalism")))
ces25.enviro <- runDictionary(ces25, cps25_imp_iss, dict_enviro)
ces25$enviro <- ces25.enviro$enviro
ces25 <- ces25 %>% mutate(enviro.dum = ifelse(enviro >= 1, 1, 0))


## =====================================================================
## 2. CRIME  (security/military moved to Foreign_Affairs; safety/guns stay)
## =====================================================================
dict_crime <- dictionary(list(crime = c(
  "crime", "crimes", "crimin*", "criminal", "criminals", "gang", "gangs",
  "safe*", "safety", "gun", "guns", "firearm", "firearms", "violence",
  "illegal", "cop", "law", "terroris*", "catch and release", "protests")))
ces25.crime <- runDictionary(ces25, cps25_imp_iss, dict_crime)
ces25$crime <- ces25.crime$crime
ces25 <- ces25 %>% mutate(crime.dum = ifelse(crime >= 1, 1, 0))


## =====================================================================
## 3. ETHICS  ("rights" moved to Socio_Cultural)
## =====================================================================
dict_ethics <- dictionary(list(ethics = c(
  "corrupt*", "corupt*", "honest*", "honnet*", "honntet", "ethic*",
  "transparen*", "accountab*", "responsib*", "truth", "vérité*", "lies",
  "lying", "lie*", "liar", "dishonesty", "integr*", "intégri*", "moral*",
  "trust", "trustworthy", "credibility", "greed", "promesses", "promise*",
  "fair*", "justice", "governance", "integrity")))
ces25.ethics <- runDictionary(ces25, cps25_imp_iss, dict_ethics)
ces25$ethics <- ces25.ethics$ethics
ces25 <- ces25 %>% mutate(ethics.dum = ifelse(ethics >= 1, 1, 0))


## =====================================================================
## 4. EDUCATION
## =====================================================================
dict_education <- dictionary(list(education = c(
  "educat*", "éducat*", "educ", "ducation", "l'ducation", "l'education",
  "deducation", "school", "schools", "schooling", "university", "tuition",
  "tuitition", "student", "students", "student loan*")))
ces25.education <- runDictionary(ces25, cps25_imp_iss, dict_education)
ces25$education <- ces25.education$education
ces25 <- ces25 %>% mutate(education.dum = ifelse(education >= 1, 1, 0))


## =====================================================================
## 5. ENERGY  (NEW - pulled from Katie's Environment dictionary)
## =====================================================================
dict_energy <- dictionary(list(energy = c(
  "pipeline*", "oil", "gas", "carbon", "carbone", "fossil", "fossiles",
  "energy", "energie", "énergie", "énergétiques", "nergtiques",
  "nuclear", "nucléaire")))
ces25.energy <- runDictionary(ces25, cps25_imp_iss, dict_energy)
ces25$energy <- ces25.energy$energy
ces25 <- ces25 %>% mutate(energy.dum = ifelse(energy >= 1, 1, 0))


## =====================================================================
## 6. JOBS  (pulled from Katie's Economy dictionary)
## =====================================================================
dict_jobs <- dictionary(list(jobs = c(
  "job*", "employ*", "emploi", "emploie", "work", "career", "income",
  "salar*", "wage", "wages", "unempl*", "unemployed", "unemployment",
  "d'oeuvre", "personnel")))
ces25.jobs <- runDictionary(ces25, cps25_imp_iss, dict_jobs)
ces25$jobs <- ces25.jobs$jobs
ces25 <- ces25 %>% mutate(jobs.dum = ifelse(jobs >= 1, 1, 0))


## =====================================================================
## 7. ECONOMY  (narrowed: taxes/debt/inflation/jobs/trade removed)
## =====================================================================
dict_economy <- dictionary(list(economy = c(
  "econom*", "écon*", "economie", "économie", "economics", "economist",
  "économiste", "recession", "growth", "market", "industr*", "commerciale",
  "business*", "finance", "finances", "financ*", "financière", "money",
  "argent", "largent", "dollars", "invest*", "investissements", "wealth",
  "recovery", "interest rate*", "middle class", "small business*",
  "infrastructure*", "evonomy", "enconomie", "ecomomie", "econamy",
  "ecomomy", "econics", "ecomony", "leconomie", "léconomie", "l'economie")))
ces25.economy <- runDictionary(ces25, cps25_imp_iss, dict_economy)
ces25$economy <- ces25.economy$economy
ces25 <- ces25 %>% mutate(economy.dum = ifelse(economy >= 1, 1, 0))


## =====================================================================
## 8. HEALTH  (renamed from healthcare; disability moved to Social_Programs)
## =====================================================================
dict_health <- dictionary(list(health = c(
  "health*", "heath*", "healthcare", "health-care", "care", "sant*",
  "santé", "sante", "soins", "soin", "mental", "pharma*", "pharmacare",
  "drug*", "medic*", "medical", "medicine", "doctor*", "docteur",
  "prescript*", "hospitals", "wellbeing", "bien être", "life",
  "medicade", "medicaid")))
ces25.health <- runDictionary(ces25, cps25_imp_iss, dict_health)
ces25$health <- ces25.health$health
ces25 <- ces25 %>% mutate(health.dum = ifelse(health >= 1, 1, 0))


## =====================================================================
## 9. TAXES  (pulled from Katie's Economy dictionary)
## =====================================================================
dict_taxes <- dictionary(list(taxes = c(
  "tax", "tax*", "taxe", "taxes", "taxed", "taxing", "taxation", "taxs",
  "impot", "impots", "impôt*", "impts", "d'impot", "d'impo", "d'impt",
  "dimpôts", "tqxes")))
ces25.taxes <- runDictionary(ces25, cps25_imp_iss, dict_taxes)
ces25$taxes <- ces25.taxes$taxes
ces25 <- ces25 %>% mutate(taxes.dum = ifelse(taxes >= 1, 1, 0))


## =====================================================================
## 10. DEFICIT_DEBT  (pulled from Katie's Economy dictionary)
## =====================================================================
dict_debt <- dictionary(list(debt = c(
  "debt", "dept", "debit", "dette*", "deficit", "deficits", "déficit",
  "defic*", "dficit", "dficits", "budget*", "budgè*", "budgétaire",
  "budgtaire", "buget", "fiscal*", "spend*", "spending", "depenses*",
  "dépenses", "dpense", "austerity", "l'endettement", "government waste")))
ces25.debt <- runDictionary(ces25, cps25_imp_iss, dict_debt)
ces25$debt <- ces25.debt$debt
ces25 <- ces25 %>% mutate(debt.dum = ifelse(debt >= 1, 1, 0))


## =====================================================================
## 11. DEMOCRACY  (Katie's "election" renamed; "citizen*" added per Simon)
## =====================================================================
dict_democracy <- dictionary(list(democracy = c(
  "democr*", "démocr*", "elect*", "élect*", "vot*", "voter", "voting",
  "vote", "ballot*", "scrutin", "representation", "proportional",
  "first past the post", "constitution", "federal", "citizen*", "citoyen*")))
ces25.democracy <- runDictionary(ces25, cps25_imp_iss, dict_democracy)
ces25$democracy <- ces25.democracy$democracy
ces25 <- ces25 %>% mutate(democracy.dum = ifelse(democracy >= 1, 1, 0))


## =====================================================================
## 12. FOREIGN_AFFAIRS  (Katie's security + Trump + borders, per Matt's rule:
##     Trump alone / USA relations / sovereignty -> here)
## =====================================================================
dict_foreign <- dictionary(list(foreign = c(
  # Trump / USA relations / sovereignty
  "trump", "trumps", "turmp", "dtrump", "usatrump", "bufoontrump", "donald",
  "president", "président", "us", "usa", "states", "the states", "america*",
  "american", "americans", "americain*", "américain*", "états-unis",
  "etats unis", "les état unis", "états unis", "sovereignty", "sovreignty",
  "souverain*", "annex", "annexation", "annexion", "51st", "independ*",
  "indépend*", "canadian identity", "keeping canada", "neighbours",
  "lintégrité territoriale", "frontière*", "border",
  # Security / defence / international
  "security", "segurity", "scurit", "sécurité", "defense", "defence",
  "défense", "military", "armes", "weapons", "war", "wars", "china",
  "israel", "palestin*", "gaza", "international", "relations", "global",
  "interference", "ingérence", "géopolitiques", "foreign", "foreign policy")))
ces25.foreign <- runDictionary(ces25, cps25_imp_iss, dict_foreign)
ces25$foreign <- ces25.foreign$foreign
ces25 <- ces25 %>% mutate(foreign.dum = ifelse(foreign >= 1, 1, 0))


## =====================================================================
## 13. IMMIGRATION  (minority/discrimination moved to Socio_Cultural;
##     "foreign" removed -> Foreign_Affairs)
## =====================================================================
dict_immigration <- dictionary(list(immigration = c(
  "immigr*", "imigr*", "imagrat*", "immagr*", "immegr*", "immgr*", "inmigr*",
  "limmigration", "l'imigration", "l'immigration", "d'immigrant",
  "emigration", "émigrat*", "illgale", "illégale", "refugee*", "réfugiés",
  "visa*")))
ces25.immigration <- runDictionary(ces25, cps25_imp_iss, dict_immigration)
ces25$immigration <- ces25.immigration$immigration
ces25 <- ces25 %>% mutate(immigration.dum = ifelse(immigration >= 1, 1, 0))


## =====================================================================
## 14. SOCIO_CULTURAL  (Katie's women + race + indigenous, plus lgbtq,
##     rights, minority, identity, values per Simon)
## =====================================================================
dict_socio <- dictionary(list(socio_cultural = c(
  # women / abortion / gender
  "women*", "women's", "woman", "woman's", "abort*", "unborn",
  "reproductive", "femme", "femmes", "gender", "maternity",
  # race
  "race", "racis*", "black", "white", "antisemitism", "islamophob*",
  # indigenous / reconciliation
  "indigenous", "indig*", "indeginous", "aboriginal", "autochtone*",
  "reconciliation", "reconcil*", "first nation*", "native", "treaty", "trc",
  # identity / rights / values / lgbtq
  "lgbt*", "trans", "identity", "values", "equal*", "equity", "rights",
  "droit*", "freedom", "libert*", "minorit*", "discrimination", "divers*",
  "inclusiv*", "woke", "cultur*")))
ces25.socio <- runDictionary(ces25, cps25_imp_iss, dict_socio)
ces25$socio_cultural <- ces25.socio$socio_cultural
ces25 <- ces25 %>% mutate(socio_cultural.dum = ifelse(socio_cultural >= 1, 1, 0))


## =====================================================================
## 15. SOCIAL_PROGRAMS  (Katie's welfare + seniors; "social*" added;
##     disability moved here from health)
## =====================================================================
dict_social <- dictionary(list(social = c(
  # social programs / welfare
  "social*", "social services", "social program*", "social assistance",
  "aide sociale", "social aid", "welfare", "childcare", "daycare", "dental",
  "child*", "enfant*", "family", "families", "famille*", "familiale",
  "parental", "basic income", "redistribution", "poverty", "pauvr*", "poor",
  "homeless*", "disab*", "service*", "public", "benefits", "funding", "funds",
  # seniors
  "senior*", "senoir*", "pension*", "aines", "ainé*", "aine", "ainees",
  "elderly", "elder*", "oas", "cpp", "aging", "vieillesse", "vieillissement",
  "retirement", "retraite", "retir*", "retirees", "old people", "65",
  "veteran*", "ei", "odsp")))
ces25.social <- runDictionary(ces25, cps25_imp_iss, dict_social)
ces25$social <- ces25.social$social
ces25 <- ces25 %>% mutate(social.dum = ifelse(social >= 1, 1, 0))


## =====================================================================
## 16. BROKERAGE  (Katie's Quebec + federalism + "unity" per Matt)
## =====================================================================
dict_brokerage <- dictionary(list(brokerage = c(
  "quebec", "qubec", "qiebec", "québec", "québécois", "francophone",
  "franco*", "francais", "français", "lacit", "laicit", "laicite",
  "loi 21", "bill 96", "federalis*", "fédéral*", "provinc*", "autonomie",
  "unity", "unité", "bloc", "langue")))
ces25.brokerage <- runDictionary(ces25, cps25_imp_iss, dict_brokerage)
ces25$brokerage <- ces25.brokerage$brokerage
ces25 <- ces25 %>% mutate(brokerage.dum = ifelse(brokerage >= 1, 1, 0))


## =====================================================================
## 17. FREE_TRADE  (Katie's tariff dict, per Matt: tariffs/trade/threat)
##     "economist/économiste" removed -> Economy
## =====================================================================
dict_free_trade <- dictionary(list(free_trade = c(
  "tariff*", "tarif*", "tarrif*", "tarriff*", "terrifs", "thariff",
  "taxestariffs", "economytariffs", "trade", "free trade", "free-trade",
  "trade war", "big beautiful bill")))
ces25.free_trade <- runDictionary(ces25, cps25_imp_iss, dict_free_trade)
ces25$free_trade <- ces25.free_trade$free_trade
ces25 <- ces25 %>% mutate(free_trade.dum = ifelse(free_trade >= 1, 1, 0))


## =====================================================================
## 18. INFLATION  (pulled from Katie's Economy dictionary)
## =====================================================================
dict_inflation <- dictionary(list(inflation = c(
  "inflation", "cost of living", "cost", "coût", "cout", "lecout", "living",
  "prices", "price", "prix", "expensive", "afford*", "afforability",
  "rising cost", "food")))
ces25.inflation <- runDictionary(ces25, cps25_imp_iss, dict_inflation)
ces25$inflation <- ces25.inflation$inflation
ces25 <- ces25 %>% mutate(inflation.dum = ifelse(inflation >= 1, 1, 0))


## =====================================================================
## 19. HOUSING
## =====================================================================
dict_housing <- dictionary(list(housing = c(
  "housing", "house*", "home", "homes", "dwelling", "affordable",
  "unaffordable", "rent", "rents", "rental", "renting", "loyer", "maison*",
  "logement*", "sans-abris", "itinérance", "propri*")))
ces25.housing <- runDictionary(ces25, cps25_imp_iss, dict_housing)
ces25$housing <- ces25.housing$housing
ces25 <- ces25 %>% mutate(housing.dum = ifelse(housing >= 1, 1, 0))


## =====================================================================
## 20. COVID19
## =====================================================================
dict_covid <- dictionary(list(covid = c(
  "covid", "co-vid", "co vid", "pandem*", "pandém*", "pandè*", "vaccin*",
  "vax*", "virus", "corona*", "lockdown*", "epidemic", "épidémie")))
ces25.covid <- runDictionary(ces25, cps25_imp_iss, dict_covid)
ces25$covid <- ces25.covid$covid
ces25 <- ces25 %>% mutate(covid.dum = ifelse(covid >= 1, 1, 0))


## =====================================================================
## Non-answers (idk)
## =====================================================================
dict_idk <- dictionary(list(idk = c(
  "99", "unsure", "don't know", "dont know", "je ne sais pas", "je sais pas",
  "jw sais pas", "je ne saos pas", "je ne connais", "je bsais pas",
  "jai pas de reponse", "idk", "prefer not", "nothing", "no")))
ces25.idk <- runDictionary(ces25, cps25_imp_iss, dict_idk)
ces25$idk <- ces25.idk$idk
ces25 <- ces25 %>% mutate(idk.dum = ifelse(idk >= 1, 1, 0))


## =====================================================================
## Summary of results (aligned to 2021 categories)
## =====================================================================
ces25 %>%
  tbl_summary(
    include = c(enviro.dum, crime.dum, ethics.dum, education.dum, energy.dum,
                jobs.dum, economy.dum, health.dum, taxes.dum, debt.dum,
                democracy.dum, foreign.dum, immigration.dum,
                socio_cultural.dum, social.dum, brokerage.dum, free_trade.dum,
                inflation.dum, housing.dum, covid.dum, idk.dum),
    label = list(
      enviro.dum         ~ "Environment",
      crime.dum          ~ "Crime",
      ethics.dum         ~ "Ethics",
      education.dum      ~ "Education",
      energy.dum         ~ "Energy",
      jobs.dum           ~ "Jobs",
      economy.dum        ~ "Economy",
      health.dum         ~ "Health",
      taxes.dum          ~ "Taxes",
      debt.dum           ~ "Deficit / Debt",
      democracy.dum      ~ "Democracy",
      foreign.dum        ~ "Foreign Affairs",
      immigration.dum    ~ "Immigration",
      socio_cultural.dum ~ "Socio-Cultural",
      social.dum         ~ "Social Programs",
      brokerage.dum      ~ "Brokerage",
      free_trade.dum     ~ "Free Trade",
      inflation.dum      ~ "Inflation",
      housing.dum        ~ "Housing",
      covid.dum          ~ "COVID-19",
      idk.dum            ~ "Don't know / did not answer"))
