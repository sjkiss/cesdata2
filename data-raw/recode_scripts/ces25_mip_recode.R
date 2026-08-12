## =====================================================================
## CES 2025 - Most Important Problem (cps25_imp_iss) dictionary coding
## Cleaned version: multi-word entries hashed out, bugs fixed.
## Every change is marked with ## FIX or ## HASHED
## =====================================================================

## ---- packages --------------------------------------------------------
## FIX: packages now load BEFORE data() is called
library(tidyverse)
library(haven)
library(magrittr)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(tm)
library(SnowballC)
library(crayon)
library(lubridate)
library(tictoc)
library(gtsummary)
library(stringi)
## FIX: dropped library(progressr) / library(progress) - the progress bar

data("ces25")


## ---- shared text cleaning -------------------------------------------
## Phrases that must survive tokenization as ONE token.
## FIX: this now runs BEFORE punctuation is stripped, so keys with
##      apostrophes ("don't know") actually match. In the old script the
##      punctuation was removed first, so that replacement was dead code.
## NOTE: longer patterns must come before shorter ones they contain.
phrase_map <- c(
  "prime minister"          = "primeminister",
  "national security"       = "nationalsecurity",
  "old people"              = "oldpeople",
  "basic income"            = "basicincome",
  "low income"              = "lowincome",
  "middle class"            = "middleclass",
  "cost of living"          = "costofliving",
  "first past the post"     = "firstpastthepost",
  "first nations"           = "firstnations",
  "first nation"            = "firstnation",
  "responsible government"  = "responsiblegovernment",
  "aide sociale"            = "aidesociale",
  "social aid"              = "socialaid",
  "programs sociale"        = "programssociale",
  "bien etre"               = "bienetre",
  "big beautiful bill"      = "bigbeautifulbill",
  "les etats unis"          = "etatsunis",
  "etats unis"              = "etatsunis",
  "canadian identity"       = "canadianidentity",
  "keeping canada"          = "keepingcanada",
  "lintegrite territoriale" = "lintegriteterritoriale",
  "prefer not"              = "prefernot",
  "don't know"              = "dontknow",
  "dont know"               = "dontknow",
  "je ne sais pas"          = "jenesaispas",
  "je sais pas"             = "jesaispas",
  "j'ai pas de reponse"     = "jaipasdereponse",
  "jai pas de reponse"      = "jaipasdereponse"
)

## FIX: one cleaning function, applied to BOTH the responses and the
##      dictionary terms. Previously ~30 dictionary entries could never
##      match anything because they contained punctuation or accents that
##      had already been stripped out of the responses
##      (e.g. "l'environnement", "u.s.", "women's", "health-care",
##      "o'toole", "sans-abris", "états-unis", "d'impot").
cleanText <- function(x) {
  x %>%
    as.character() %>%
    tidyr::replace_na("") %>%                              ## FIX: NA -> ""
    stringi::stri_trans_general("Latin-ASCII") %>%         ## FIX: é -> e, ô -> o
    stringr::str_to_lower() %>%                            ## FIX: replaces the
    ## old str_replace_all(x, "[A-Z]", tolower), which missed accented capitals
    stringr::str_replace_all(phrase_map) %>%
    stringr::str_replace_all("[[:punct:]]", "") %>%
    stringr::str_squish()
}

## helper: clean + de-duplicate dictionary terms
cleanTerms <- function(terms) unique(cleanText(terms))


## ---- clean the responses --------------------------------------------
ces25$cps25_imp_iss <- cleanText(ces25$cps25_imp_iss)

## FIX (important): removed
##   sub("^(\\w+)\\s+(\\w+)$", "\\2 \\1", ces25$cps25_imp_iss)
## That line silently REVERSED the word order of every response that was
## exactly two words long ("climate change" -> "change climate",
## "prime minister" -> "minister prime"). It was corrupting the data.


## ---- dictionary runner ----------------------------------------------
runDictionary <- function(
    dataA,        # input data
    word,         # column to be searched
    dictionaryA) {# dictionary of terms to search for

  tictoc::tic()

  key  <- names(dictionaryA)[1]
  ## FIX: pull() instead of mutate(word = {{word}}) - the old version
  ##      overwrote any existing column called "word", and choked on
  ##      haven_labelled columns
  txt  <- dataA %>% dplyr::pull({{ word }}) %>% as.character() %>%
    tidyr::replace_na("")

  toks <- quanteda::tokens(txt)
  dfmA <- quanteda::dfm(
    quanteda::tokens_lookup(toks, dictionaryA, nested_scope = "dictionary"))

  ## FIX: guarantees the key column exists even when nothing matches.
  ##      Previously a zero-match dictionary returned a data.frame with no
  ##      such column, so dataB$key was NULL and the assignment failed.
  dfmA <- quanteda::dfm_match(dfmA, features = key)

  dataB <- quanteda::convert(dfmA, to = "data.frame")

  ## FIX: real feedback instead of the progress bar
  message(crayon::green(sprintf(
    "%s: %d of %d responses matched", key, sum(dataB[[key]] >= 1), length(txt))))
  tictoc::toc()

  return(dataB)
}


## ---- Environment ------------------------------------------------------
dictionaryenviro <- dictionary(list(enviro = cleanTerms(c(
  "climate", "change", "envi", "pipelines", "oil", "carbon",
  "pipeline", "environnement", "environmental", "environment",
  #"climate change",                                        ## HASHED
  "warming",
  "l'environnement", "climatiques", "lenvironement", "ges",
  "rechauffement", "gas", "enviroment", "water",
  "sustainability", "enviromental", "environnement",
  "écologie", "l'envéronnement", "l'ecologie",
  "l'environnemen", "l'environnemenr", "l'environnent",
  "emvironnement", "environnemen5", "ecology", "co2", "polluer",
  "pollute", "pollution", "planet", "nergtiques", "energy",
  "carbone", "greener", "green", "climatique", "environnment",
  "enviournment", "climat", "envioroment", "earth", "cologie",
  "environnementaux", "ecologie", "enviornment", "enviro",
  "enviormental", "enironment", "fossiles", "fossil",
  "environement", "environmentalism", "l'cologie",
  "l'environement", "pipe", "lenvironnement", "lenrironnement",
  "environnemental"))))

ces25.enviro <- runDictionary(ces25, cps25_imp_iss, dictionaryenviro)
ces25$enviro <- ces25.enviro$enviro
ces25 <- ces25 %>% mutate(enviro.dum = ifelse(enviro >= 1, 1, 0))


## ---- Security / Defence and International Relations ------------------
dictionnarysecurity <- dictionary(list(security = cleanTerms(c(
  "security", "defense", "international", "china", "defence",
  "war", "wars", "relations", "global", "israel",
  #"u.s.",                     ## HASHED: strips to "us", which collides with
  ## the ordinary English pronoun; it is already covered in the borders dict
  "segurity", "scurit", "military", "palestin", "palestine",
  "interference", "ingérence", "défense",
  "terrorism", "sécurité", "géopolitiques", "armes",
  "weapons", "gaza",
  "nationalsecurity"))))       ## FIX: the pre-processing collapses
## "national security" -> "nationalsecurity", but that token was in no dictionary

ces25.security <- runDictionary(ces25, cps25_imp_iss, dictionnarysecurity)
ces25$security <- ces25.security$security
ces25 <- ces25 %>% mutate(security.dum = ifelse(security >= 1, 1, 0))


## ---- Ethics ----------------------------------------------------------
dictionnaryethics <- dictionary(list(ethics = cleanTerms(c(
  "gouvernement", "corruption", "honesty", "ethics",
  "transparency", "accountability", "responsibility", "truth",
  "lies", "lying", "ethical", "transparent", "integretity",
  "corrupt", "trustworthy", "dishonesty", "liar",
  "transparence", "moral", "integrity", "honest", "trust",
  "corruptions", "coruption", "credibility", "greed",
  "promesses", "honestly", "honnetete", "honntet", "morality",
  "morals", "accountable", "accountibility",
  "rights", "represented", "fairness", "governance",
  #"responsible government",                                ## HASHED
  "responsiblegovernment"))))

ces25.ethics <- runDictionary(ces25, cps25_imp_iss, dictionnaryethics)
ces25$ethics <- ces25.ethics$ethics
ces25 <- ces25 %>% mutate(ethics.dum = ifelse(ethics >= 1, 1, 0))


## ---- Education -------------------------------------------------------
dictionnaryeducation <- dictionary(list(education = cleanTerms(c(
  "education", "ducation", "school", "schools",
  "educational", "university", "tuition", "student",
  "students", "schooling", "l'ducation", "l'education",
  "deducation", "Éducation"))))

ces25.education <- runDictionary(ces25, cps25_imp_iss, dictionnaryeducation)
ces25$education <- ces25.education$education
ces25 <- ces25 %>% mutate(education.dum = ifelse(education >= 1, 1, 0))


## ---- Economy ---------------------------------------------------------
dictionaryeco <- dictionary(list(economy = cleanTerms(c(
  "economy", "jobs", "employment", "tax", "taxs", "taxes",
  "job", "conomie", "con", "l'conomie", "economie",
  "conomique", "dette", "debt", "deficit", "impts", "finances",
  "finance", "impot", "dficits", "budget", "conomiques",
  "economics", "balanced", "dollars", "deficits", "evonomy",
  #"low income", "middle class", "cost of", "rising cost",  ## HASHED
  "lowincome", "middleclass", "costofliving",               ## FIX: collapsed forms
  "spending", "trade", "depenses", "déficit",
  "dpense", "taxing", "wage", "wages",
  "economic", "budgets", "taxation",
  "fiscal", "market", "recession", "growth", "loans",
  "budgétaire", "leconomie", "argent",
  "l'endettement", "living", "cost", "money",
  "inequality", "prices", "inflation", "poor",
  "enconomie", "ecomomie", "econamy", "emploi", "ecomomy",
  "econics", "unemployment", "impots", "affordability",
  "d'impot", "d'impo", "d'impt", "emploie", "economique",
  "ecomony", "work", "unemployed", "taxe", "taxed", "dficit",
  "financial", "budgtaire", "l'economie", "economist",
  "économiste", "coût", "prix",
  "léconomie", "cout", "expensive", "industry", "industrie",
  "commerciale", "Économie", "afforability",
  "afford", "financière", "largent",
  "léconomique", "impôts", "dimpôts",
  "investments", "investment", "investissements", "économique",
  "income", "lecout", "trading"))))

ces25.econ <- runDictionary(ces25, cps25_imp_iss, dictionaryeco)
ces25$economy <- ces25.econ$economy
ces25 <- ces25 %>% mutate(economy.dum = ifelse(economy >= 1, 1, 0))


## ---- Healthcare ------------------------------------------------------
dictionnaryhealthcare <- dictionary(list(healthcare = cleanTerms(c(
  "health", "health-care", "care", "sant", "soins", "life",
  "mental", "disability", "pharmacare", "disabled", "drugs",
  "drug", "medicare", "santé", "medical", "heath",
  "prescriptions", "doctors", "sante", "soin",
  "santè", "docteur", "healthcare", "healtcare",
  "heathcare", "hospitals", "medicine",
  #"bien être",                                             ## HASHED
  "bienetre", "wellbeing", "hralthcare", "medicade", "medicaid"))))

ces25.healthcare <- runDictionary(ces25, cps25_imp_iss, dictionnaryhealthcare)
ces25$healthcare <- ces25.healthcare$healthcare
ces25 <- ces25 %>% mutate(healthcare.dum = ifelse(healthcare >= 1, 1, 0))


## ---- Electoral Reform ------------------------------------------------
dictionnaryelection <- dictionary(list(election = cleanTerms(c(
  "election", "electoral", "voting", "voter",
  "representation", "democracy",
  #"first past the post",                                   ## HASHED
  "firstpastthepost", "proportional", "vote"))))

ces25.election <- runDictionary(ces25, cps25_imp_iss, dictionnaryelection)
ces25$election <- ces25.election$election
ces25 <- ces25 %>% mutate(election.dum = ifelse(election >= 1, 1, 0))


## ---- Crime -----------------------------------------------------------
## FIX: crime.dum was used in tbl_summary() but no crime dictionary
##      existed anywhere in the script, so the summary errored out.
##      Terms below are a starting point - tune before you report.
dictionnarycrime <- dictionary(list(crime = cleanTerms(c(
  "crime", "crimes", "criminal", "criminals", "criminality",
  "criminalité", "criminalite", "police", "policing", "gun", "guns",
  "violence", "violent", "safety", "theft", "thefts", "murder",
  "gang", "gangs", "justice", "prison", "prisons", "jail",
  "délinquance", "delinquance", "sécuritaire", "securitaire",
  "vandalism", "trafficking", "fentanyl", "overdose"))))

ces25.crime <- runDictionary(ces25, cps25_imp_iss, dictionnarycrime)
ces25$crime <- ces25.crime$crime
ces25 <- ces25 %>% mutate(crime.dum = ifelse(crime >= 1, 1, 0))


## ---- Trump -----------------------------------------------------------
dictionnarytrump <- dictionary(list(trump = cleanTerms(c(
  "trump", "turmp", "donald", "president", "président",
  "trumps", "dtrump", "bufoontrump", "usatrump"))))

ces25.trump <- runDictionary(ces25, cps25_imp_iss, dictionnarytrump)
ces25$trump <- ces25.trump$trump
ces25 <- ces25 %>% mutate(trump.dum = ifelse(trump >= 1, 1, 0))


## ---- Tariffs ---------------------------------------------------------
dictionnarytariff <- dictionary(list(tariff = cleanTerms(c(
  "tariff", "tariffs", "tarif", "tarifs",
  #"economist", "économiste",   ## HASHED: these are economy words that had
  ## been pasted into the tariff dictionary and were inflating the count
  #"big beautiful bill",                                    ## HASHED
  "bigbeautifulbill",
  "tarriffs", "terrifs", "economytariffs", "tarrifs",
  "taxestariffs", "thariff"))))

ces25.tariff <- runDictionary(ces25, cps25_imp_iss, dictionnarytariff)
ces25$tariff <- ces25.tariff$tariff
ces25 <- ces25 %>% mutate(tariff.dum = ifelse(tariff >= 1, 1, 0))


## ---- Borders / US relations ------------------------------------------
dictionnaryborders <- dictionary(list(borders = cleanTerms(c(
  "soveriegnty", "border", "souveraineté", "frontière",
  "annex", "annexation", "annexion", "trump", "sovereignty",
  "us",   ## NOTE: this also catches the English pronoun "us". Consider
  ## hashing it - "usa"/"america"/"american" already cover the concept.
  "usa", "states",
  #"the states", "les état unis", "États unis", "etats unis",  ## HASHED
  #"lintégrité territoriale", "canadian identity", "keeping canada", ## HASHED
  "etatsunis", "etasunis", "lintegriteterritoriale",
  "canadianidentity", "keepingcanada",
  "america", "americans", "états-unis", "américain", "americain",
  "américains", "americains", "frontières", "indépendance",
  "independence", "sovreignty", "independent", "indépendant",
  "autonomie", "autonomy", "51st", "américaines", "american",
  "souverainté", "neighbours"))))

ces25.borders <- runDictionary(ces25, cps25_imp_iss, dictionnaryborders)
ces25$borders <- ces25.borders$borders
ces25 <- ces25 %>% mutate(borders.dum = ifelse(borders >= 1, 1, 0))


## ---- Trump + Tariffs + Borders combined ------------------------------
## FIX: built as the union of the three component dictionaries instead of
##      a fourth hand-typed copy. The old copy had already drifted
##      (e.g. it contained "Étasunis"/"Étatsunis", which borders did not).
dictionnarycombined <- dictionary(list(combined = unique(c(
  as.list(dictionnarytrump)$trump,
  as.list(dictionnarytariff)$tariff,
  as.list(dictionnaryborders)$borders))))

ces25.combined <- runDictionary(ces25, cps25_imp_iss, dictionnarycombined)
ces25$combined <- ces25.combined$combined
ces25 <- ces25 %>% mutate(combined.dum = ifelse(combined >= 1, 1, 0))


## ---- Immigration -----------------------------------------------------
dictionaryimmigration <- dictionary(list(immigration = cleanTerms(c(
  "immigration", "illgale", "illégale", "minority",
  "discrimination", "immigrants", "immigrant", "langue",
  "l'imigration", "d'immigrant", "foreign", "immigrations",
  "imagration", "imigration", "immegrants",
  "l'immigration", "emigration", "refugee", "refugees",
  "immagration", "immgration", "imigrant", "limmigration"))))

ces25.immigration <- runDictionary(ces25, cps25_imp_iss, dictionaryimmigration)
ces25$immigration <- ces25.immigration$immigration
ces25 <- ces25 %>% mutate(immigration.dum = ifelse(immigration >= 1, 1, 0))


## ---- Women's Issues and Abortion -------------------------------------
dictionnarywomen <- dictionary(list(women = cleanTerms(c(
  "women", "women's", "abortion", "abortions", "woman",
  "woman's", "unborn", "reproductive", "femme", "femmes",
  "gender", "maternity", "womens", "anatiabortion"))))

ces25.women <- runDictionary(ces25, cps25_imp_iss, dictionnarywomen)
ces25$women <- ces25.women$women
ces25 <- ces25 %>% mutate(women.dum = ifelse(women >= 1, 1, 0))


## ---- Race ------------------------------------------------------------
dictionnaryrace <- dictionary(list(race = cleanTerms(c(
  "race", "racism", "racist", "black", "white", "antisemitism",
  "islamophoby", "islamaphobia", "islamophobia"))))

ces25.race <- runDictionary(ces25, cps25_imp_iss, dictionnaryrace)
ces25$race <- ces25.race$race
ces25 <- ces25 %>% mutate(race.dum = ifelse(race >= 1, 1, 0))


## ---- Indigenous ------------------------------------------------------
dictionnaryindigenous <- dictionary(list(indigenous = cleanTerms(c(
  "indigenous", "aboriginal", "reconciliation",
  #"first nations", "first nation",                         ## HASHED
  "firstnations", "firstnation",
  "indeginous", "native"))))

ces25.indigenous <- runDictionary(ces25, cps25_imp_iss, dictionnaryindigenous)
ces25$indigenous <- ces25.indigenous$indigenous
ces25 <- ces25 %>% mutate(indigenous.dum = ifelse(indigenous >= 1, 1, 0))


## ---- Other Welfare ---------------------------------------------------
dictionnarywelfare <- dictionary(list(welfare = cleanTerms(c(
  "childcare", "children", "daycare", "dental", "welfare",
  "social", "family", "families", "famille", "child",
  "children's", "basicincome", "familiale", "familles",
  "poverty", "assistance", "public", "pauvret",
  "service", "services", "parental", "redistribution",
  #"aide sociale", "social aid",                            ## HASHED
  "aidesociale", "socialaid", "programssociale"))))

ces25.welfare <- runDictionary(ces25, cps25_imp_iss, dictionnarywelfare)
ces25$welfare <- ces25.welfare$welfare
ces25 <- ces25 %>% mutate(welfare.dum = ifelse(welfare >= 1, 1, 0))


## ---- Seniors ---------------------------------------------------------
dictionnaryseniors <- dictionary(list(seniors = cleanTerms(c(
  "pension", "pensions", "seniors", "senior", "aines", "ages",
  "cpp", "elderly", "oas", "aging", "senior's", "retirement",
  "âgées", "ainés", "65", "vieillisse", "viellesse",
  "vielliesse", "vieux", "ainees", "aine", "vieillissement",
  "veillesse", "pesion", "age",
  #"old people",                                            ## HASHED
  "oldpeople", "retirees", "retraite", "retired", "senoir"))))

ces25.seniors <- runDictionary(ces25, cps25_imp_iss, dictionnaryseniors)
ces25$seniors <- ces25.seniors$seniors
ces25 <- ces25 %>% mutate(seniors.dum = ifelse(seniors >= 1, 1, 0))


## ---- Leaders ---------------------------------------------------------
dictionnaryleaders <- dictionary(list(leaders = cleanTerms(c(
  "carney", "mark", "libéral", "libral", "liberals",
  "leadership", "leader", "justin", "conservatives", "parties",
  "leaders", "pm", "andrew", "sheer", "singh", "blanchet", "ndp",
  "bloc", "green", "paul", "may", "otoole", "trudeau",
  "toole", "bernier", "politician", "trudeau's", "o'toole",
  "libéraux", "ford", "scheer",
  #"prime minister",                                        ## HASHED
  "primeminister",
  "candidate", "candidates", "liberal", "pierre", "poilievre",
  "conservateurs", "conservateur", "carnay", "marc",
  "conservatrices", "poliviere", "conservative"))))

ces25.leaders <- runDictionary(ces25, cps25_imp_iss, dictionnaryleaders)
ces25$leaders <- ces25.leaders$leaders
ces25 <- ces25 %>% mutate(leaders.dum = ifelse(leaders >= 1, 1, 0))


## ---- Quebec ----------------------------------------------------------
dictionnaryquebec <- dictionary(list(quebec = cleanTerms(c(
  "quebec", "21", "qubec", "francophone", "lacit", "laicit",
  "laicite", "québec", "québécois"))))

ces25.quebec <- runDictionary(ces25, cps25_imp_iss, dictionnaryquebec)
ces25$quebec <- ces25.quebec$quebec
ces25 <- ces25 %>% mutate(quebec.dum = ifelse(quebec >= 1, 1, 0))


## ---- Housing ---------------------------------------------------------
dictionnaryhousing <- dictionary(list(housing = cleanTerms(c(
  "housing", "affordable", "rent", "homeless", "rental",
  "unaffordable", "renting", "home", "homes", "dwelling",
  "loyer", "maisons", "sans-abris", "logement", "logements",
  "rents", "homelessness", "housingaffordability",
  "itinérance", "inflationhousingrenting"))))

ces25.housing <- runDictionary(ces25, cps25_imp_iss, dictionnaryhousing)
ces25$housing <- ces25.housing$housing
ces25 <- ces25 %>% mutate(housing.dum = ifelse(housing >= 1, 1, 0))


## ---- Non-answers -----------------------------------------------------
dictionnaryidk <- dictionary(list(idk = cleanTerms(c(
  "99", "unsure", "dontknow", "jenesaispas", "idk",
  #"prefer not",                                            ## HASHED
  "prefernot",
  #"no",   ## HASHED: "no" matched any response containing the word
  ## ("no jobs", "no housing"), badly over-counting non-answers
  "nothing", "none", "na", "jesaispas", "jaipasdereponse"))))

ces25.idk <- runDictionary(ces25, cps25_imp_iss, dictionnaryidk)
ces25$idk <- ces25.idk$idk
ces25 <- ces25 %>% mutate(idk.dum = ifelse(idk >= 1, 1, 0))


## ---- Summary of the results ------------------------------------------
ces25 %>%
  tbl_summary(
    include = c(economy.dum, enviro.dum, immigration.dum,
                healthcare.dum, housing.dum, seniors.dum, leaders.dum,
                ethics.dum, education.dum, crime.dum, indigenous.dum,
                welfare.dum, election.dum, women.dum, security.dum,
                idk.dum, quebec.dum, race.dum, combined.dum,
                tariff.dum, trump.dum, borders.dum),
    label = list(
      economy.dum     ~ "The Economy",
      enviro.dum      ~ "The Environment",
      immigration.dum ~ "Immigration",   ## FIX: was "=" instead of "~"
      healthcare.dum  ~ "Healthcare",
      housing.dum     ~ "Housing",
      seniors.dum     ~ "Seniors",
      leaders.dum     ~ "Party Leaders",
      ethics.dum      ~ "Ethical Concerns",
      education.dum   ~ "Education",
      crime.dum       ~ "Crime",
      indigenous.dum  ~ "Indigenous Issues and Reconciliation",
      welfare.dum     ~ "Welfare",
      election.dum    ~ "Electoral Reform",
      women.dum       ~ "Women's Issues",
      security.dum    ~ "Security and International Relations",
      idk.dum         ~ "Don't know / did not answer",
      quebec.dum      ~ "Quebec",
      race.dum        ~ "Race",
      combined.dum    ~ "US Relations, Trump and Tariffs",
      tariff.dum      ~ "Tariffs",
      trump.dum       ~ "Trump",
      borders.dum     ~ "US Relations"))










## =====================================================================
## CES 2025 - Most Important Problem (cps25_imp_iss)
## Dictionary coding ALIGNED TO THE 2021 CODING SCHEME
##
## Two problems are solved here.
##
## (1) MULTI-WORD PHRASES
##     You write "first nations" normally in the term list. The script
##     scans every term list, finds everything that is still multi-word
##     after normalisation, and auto-generates the collapsing rule
##     ("first nations" -> "firstnations"), then applies the identical
##     rule to the survey responses before tokenising. Nothing is hashed
##     out and there is no hand-maintained phrase_map to keep in sync.
##     It is now impossible to add a phrase to a dictionary and have it
##     silently fail to match.
##
## (2) 2021 ALIGNMENT
##     Category names, numeric codes and value labels are taken from the
##     2021 val_labels() block. The 2025 categories have been split and
##     merged to fit. The `issue_scheme` object below is the single place
##     where a category's name, code, label and terms live together, so
##     they cannot drift apart.
##
## CROSSWALK 2025 script -> 2021 scheme
## ---------------------------------------------------------------------
##  2021 code / label          <- 2025 source
##   1 Environment             <- enviro, minus the energy terms
##   2 Crime                   <- crime + domestic-security half of `security`
##   3 Ethics                  <- ethics
##   4 Education               <- education
##   5 Energy                  <- SPLIT OUT of enviro (pipeline/oil/gas/fossil)
##   6 Jobs                    <- SPLIT OUT of economy
##   7 Economy                 <- economy, after 6/9/10/18 are removed
##   8 Health                  <- healthcare
##   9 Taxes                   <- SPLIT OUT of economy
##  10 Deficit_Debt            <- SPLIT OUT of economy
##  11 Democracy               <- election
##  12 Foreign_Affairs         <- international half of `security`
##  13 Immigration             <- immigration
##  14 Socio_Cultural          <- women + race + indigenous
##  15 Social_Programs         <- welfare + seniors
##  16 Brokerage               <- quebec
##  17 Free_Trade              <- tariff   (2021 code 17 existed but was
##                                          never assigned; tariffs fit it)
##  18 Inflation               <- SPLIT OUT of economy (cost of living etc.)
##  19 Housing                 <- housing
##  20 COVID19                 <- kept for structural comparability; expect ~0
##  21 US_Relations  ** NEW ** <- borders
##  22 Trump         ** NEW ** <- trump
##  23 Party_Leaders ** NEW ** <- leaders (2021 had no such code; these
##                                         responses fell into Other)
##   0 Other                   <- nothing matched
##     mip_missing             <- idk (NOT a category; mirrors 2021)
##
## For a strict 2021-comparable series, pool 12 + 17 + 21 + 22 into
## Foreign_Affairs and fold 23 back into Other. `mip21` at the bottom
## does exactly that.
## =====================================================================

## ---- packages --------------------------------------------------------
library(tidyverse)
library(here)
library(haven)
library(labelled)
library(quanteda)
library(crayon)
library(tictoc)
library(gtsummary)
library(stringi)

data("ces25")

RESP <- "cps25_imp_iss"   ## the open-ended column being coded


## =====================================================================
## SECTION 1 - THE CODING SCHEME
##
## One entry per category: name -> code, label, terms.
##
## Terms may use:
##   * glob wildcards, exactly as in the 2021 str_detect() calls
##     ("environ*", "democr*"). quanteda's tokens_lookup() uses
##     valuetype = "glob" by default, so these work unchanged.
##   * multi-word phrases ("cost of living", "first nations"), which are
##     auto-collapsed - see Section 2.
##   * accents, apostrophes, hyphens - all normalised away.
##
## IMPORTANT DIFFERENCE FROM 2021: matching is now per-TOKEN, not
## substring. In 2021 str_detect(mip_lower, "ev") fired on "everything"
## and "development"; str_detect(mip_lower, "na") fired on hundreds of
## unrelated words; "old*" fired on "gold". Those are gone. If you rerun
## 2021 through this machinery the counts WILL move, and they will move
## in the right direction. Wildcards still give you the 2021 reach:
## "environ*" matches the token "environnement" but not "unenvironed".
## =====================================================================

issue_scheme <- list(

  ## ---- 1 Environment -------------------------------------------------
  enviro = list(code = 1, label = "Environment", terms = c(
    ## 2021 terms
    "environ*", "climat*", "changement*", "climatiques", "ecolog*",
    "écolog*", "pollution", "planet*", "planète", "drinking water",
    "carbon", "carbone", "envion*", "envrion*", "ev", "emviron*", "verte",
    "eniron*", "envirron*", "l'envér*", "l'air", "envir*", "clkmatique",
    "pesticide*", "gaz a effet de serre", "nuclear", "cl8mate change",
    "climet chang",
    ## 2025 additions
    "climate change", "warming", "rechauffement", "ges", "water",
    "sustainability", "enviroment*", "enviorment*", "enviournment",
    "enviornment", "enioroment", "envioroment", "co2", "polluer",
    "pollute", "earth", "ecology", "greener", "environmentalism",
    "lenvironnement", "lenrironnement", "lenvironement", "l'cologie",
    "l'ecologie", "l'environnement")),
  ## NOTE "green" is NOT here. In 2021 it sat in Environment; in 2025 it
  ## collides with the Green Party (category 23). Left in neither -
  ## decide which you want and put it in exactly one.

  ## ---- 2 Crime -------------------------------------------------------
  crime = list(code = 2, label = "Crime", terms = c(
    ## 2021 terms
    "crime*", "crimin*", "safe*", "law", "catch and release", "protests",
    "terroris*",
    ## 2021 put "security"/"securité" here; the international senses live
    ## in category 12. See the overlap audit in Section 5.
    "security", "securite", "securité", "sécuritaire", "securitaire",
    ## 2025 additions
    "criminalit*", "police", "policing", "gun", "guns", "violence",
    "violent", "theft", "thefts", "murder", "gang", "gangs", "prison",
    "prisons", "jail", "delinquance", "délinquance", "vandalism",
    "trafficking", "fentanyl", "overdose")),

  ## ---- 3 Ethics ------------------------------------------------------
  ethics = list(code = 3, label = "Ethics", terms = c(
    ## 2021 terms
    "honest*", "honnet*", "honnête*", "honnêteté", "integrity", "integre*",
    "intégri*", "ethic*", "éthique", "l'éthique", "corrupt*", "corupt*",
    "truth", "vérité*", "justice", "trust", "fair*", "lying", "lieing*",
    "liar*", "lie*", "crook*", "hypocrisy", "l'honn*", "l'intégr*",
    "ėquité", "équité", "moral*", "decency", "promise*", "promesses",
    "bad behavior", "in jail",
    ## 2025 additions
    "transparen*", "accountab*", "accountibility", "responsibility",
    "responsible government", "governance", "trustworthy", "dishonesty",
    "integretity", "credibility", "greed", "rights", "represented",
    "gouvernement")),
  ## NOTE "rights" also reads as Socio_Cultural (14) - see overlap audit.

  ## ---- 4 Education ---------------------------------------------------
  education = list(code = 4, label = "Education", terms = c(
    "educat*", "éducat*", "educ", "ducation", "l'educat*", "l'ducation",
    "deducation", "school", "schools", "schooling", "university",
    "tuition", "tuitition", "student", "students", "student loan*",
    "student funding", "student issue*", "loans and grants")),

  ## ---- 5 Energy ------------------------------------------------------
  ## SPLIT OUT of the 2025 enviro dictionary to restore the 2021 code 5.
  energy = list(code = 5, label = "Energy", terms = c(
    "pipeline*", "pipe", "energy", "energie", "énergie", "nergtiques",
    "énergétique*", "energetique*", "oil", "petrole", "pétrole", "gas",
    "fossil", "fossiles", "fuel", "electricity", "electricite",
    "électricité", "hydro")),

  ## ---- 6 Jobs --------------------------------------------------------
  ## SPLIT OUT of the 2025 economy dictionary to restore the 2021 code 6.
  jobs = list(code = 6, label = "Jobs", terms = c(
    "job*", "employ*", "emploi*", "emploie", "income", "lowincome",
    "low income", "d'oeuvre", "personnel", "work", "career", "salar*",
    "unempl*", "unemployed", "unemployment", "travail")),
  ## NOTE 2021 put "wage*" in Inflation (18), not Jobs. Kept there.

  ## ---- 7 Economy -----------------------------------------------------
  economy = list(code = 7, label = "Economy", terms = c(
    ## 2021 terms
    "econom*", "écon*", "ècon*", "ecnomic", "ecenomy", "ecomony",
    "éconmique", "recovery", "economic recovery", "financ*", "money",
    "l'argent", "argent", "largent", "wealth", "richesse*", "la richesse",
    "interest rate*", "industry", "industrie", "growth", "développemen*",
    "business concern*", "small business*", "commerces", "commerciale",
    "middle class", "classe moyenne", "the classes", "rich and the rest",
    "pay increase*", "raises", "fighting big tech", "infrastructure*",
    ## 2025 additions
    "enconomie", "ecomomie", "econamy", "ecomomy", "econics", "evonomy",
    "leconomie", "l'economie", "l'conomie", "conomi*", "con",
    "market", "recession", "trade", "trading", "loans", "dollars",
    "investment*", "investissements", "inequality", "poor")),
  ## NOTE "poor" is also Social_Programs in 2021. See overlap audit.

  ## ---- 8 Health ------------------------------------------------------
  health = list(code = 8, label = "Health", terms = c(
    ## 2021 terms
    "health*", "heath*", "helath", "heaalthcare", "heaith care",
    "heakth caew", "hralthcare", "healtcare", "sant*", "pharma*",
    "long term*", "longterm care", "medical*", "medicine", "medicare",
    "medicaid", "medicade", "opioid", "opiod", "overdose", "drug*",
    "doctor*", "docteur", "prescript*", "optometr*", "bill 124",
    ## 2025 additions
    "care", "soin", "soins", "life", "mental", "disability", "disabled",
    "hospitals", "bien etre", "bien être", "wellbeing")),

  ## ---- 9 Taxes -------------------------------------------------------
  ## SPLIT OUT of the 2025 economy dictionary to restore the 2021 code 9.
  taxes = list(code = 9, label = "Taxes", terms = c(
    "tax", "tax*", "taxe*", "taxs", "taxation", "tqxes", "impot*",
    "impôt*", "impts", "d'impot", "d'impôt*", "d'impo", "d'impt")),

  ## ---- 10 Deficit / Debt ---------------------------------------------
  ## SPLIT OUT of the 2025 economy dictionary to restore the 2021 code 10.
  debt = list(code = 10, label = "Deficit_Debt", terms = c(
    "debt", "dept", "debit", "dette*", "la dette", "l'endettement",
    "deficit*", "défic*", "defic*", "defec*", "dficit*", "budget*",
    "budjet*", "bugdet*", "budgè*", "buget", "budgtaire", "budgétaire",
    "fiscal*", "spend*", "government spend*", "government waste",
    "depense*", "dépense*", "dpense", "austerity", "balanced")),

  ## ---- 11 Democracy --------------------------------------------------
  democracy = list(code = 11, label = "Democracy", terms = c(
    "democr*", "démocr*", "demovracy", "demo0cr5atic", "elect*", "élect*",
    "vot*", "ballot*", "scrutin", "constitution", "representation",
    "first past the post", "proportional")),
  ## NOTE 2021 also had a bare "federal" here, which duplicated
  ## "fédéral*" in Brokerage. Dropped - it belongs in 16.

  ## ---- 12 Foreign Affairs --------------------------------------------
  foreign = list(code = 12, label = "Foreign_Affairs", terms = c(
    ## 2021 terms
    "peace", "war", "wars", "chin*", "foreign", "foreign policy",
    "defence", "defense", "défense", "armed force*", "israel", "jew*",
    "gaza", "travel restriction*",
    ## 2025 additions
    "international", "relations", "global", "military", "armes",
    "weapons", "palestin*", "interference", "ingerence", "ingérence",
    "geopolitique*", "géopolitique*", "national security", "nato", "otan",
    "ukraine", "russia", "russie")),
  ## NOTE 2021 put "border" and "usa" here. In 2025 those carry a
  ## different meaning (annexation / 51st state) and live in code 21.

  ## ---- 13 Immigration ------------------------------------------------
  immigration = list(code = 13, label = "Immigration", terms = c(
    "immigr*", "imigr*", "inmigr*", "imagrat*", "immagr*", "immegrants",
    "immgration", "l'immigr*", "l'imigr*", "limmigration", "d'immigrant",
    "emigration", "émigrat*", "refugee*", "refudgee", "réfugiés",
    "third world people", "visa*", "sponsor", "illegal*", "illégale",
    "illgale")),
  ## NOTE 2021 had "frontiere" here; in 2025 border language is code 21.

  ## ---- 14 Socio-Cultural ---------------------------------------------
  socio_cultural = list(code = 14, label = "Socio_Cultural", terms = c(
    ## 2021 terms
    "trans", "identity", "values", "equal*", "equity", "égalité", "human*",
    "indig*", "indigenous*", "indeginous", "aboriginal", "native",
    "autochtone*", "minorit*", "rights", "droit*", "gun*", "firearm*",
    "fire arm*", "weapons ownership", "abort*", "unborn", "freedom",
    "libert*", "liberté", "free speech", "accessibility", "anti-semitism",
    "antisemitism", "divers*", "lgbt*", "woke", "c-6", "c-71",
    "cancel-culture", "inclusiv*", "cultur*", "women*", "woman*",
    "feministe", "feminine", "islamophob*", "islamaphobia", "race",
    "racis*", "animal", "arts", "treaty", "trc", "reconcil*",
    "false information", "fake information", "fake news", "misinformation",
    "desinformation", "credible information", "media", "land agreement*",
    "family issue*", "youth issue*",
    ## 2025 additions
    "first nations", "first nation", "abortions", "reproductive",
    "maternity", "femme", "femmes", "gender", "discrimination", "black",
    "white", "langue")),
  ## NOTE 2021 had "racis*" in Social_Programs. That was a
  ## mis-assignment; moved here. 2021 also had "reconcil*" in BOTH
  ## Socio_Cultural and Brokerage - now here only.

  ## ---- 15 Social Programs --------------------------------------------
  social = list(code = 15, label = "Social_Programs", terms = c(
    ## 2021 terms
    "poverty", "pauvr*", "pauvreté", "impoverish*", "poor", "child*",
    "enfant*", "homeless*", "senior*", "senoir*", "seanor", "old*",
    "old people", "elder*", "age*", "agee*", "agée*", "âgé*", "âgées",
    "ainé*", "ainee*", "aine*", "aîné*", "vieill*", "viell*", "vieux",
    "retir*", "retrait*", "retraités", "retirees", "pension*", "pesion",
    "universal basic*", "basic income", "ubi", "day*", "daycare",
    "childcare", "social*", "disab*", "sick leave", "reserve*", "cerb",
    "pcre", "pcu", "ccb", "cpp", "oas", "odsp", "ltc", "ei", "wsib",
    "autis*", "veteran*", "65", "60 ans", "under-privileged", "benefits",
    "family support", "funding", "funds", "la population", "famille*",
    "soutien", "fonds", "garde*",
    ## 2025 additions
    "dental", "welfare", "family", "families", "assistance", "service",
    "services", "parental", "redistribution", "aide sociale",
    "social aid", "programs sociale", "programmes sociaux", "familiale",
    "vieillissement")),
  ## NOTE "ubi" was in Brokerage in 2021 - clearly a typo-level error,
  ## moved here. 2021's "ei" and "65" were substring-matched and fired
  ## constantly; as tokens they are now safe.

  ## ---- 16 Brokerage --------------------------------------------------
  brokerage = list(code = 16, label = "Brokerage", terms = c(
    "provinc*", "provincia*", "juridiction*", "jurisdiction",
    "province jurisdiction", "federalis*", "fédéral*", "federal",
    "quebec", "québec", "qubec", "qiebec", "québécois", "quebecois",
    "franc*", "français", "francais", "franco*", "francophone",
    "autonomie", "unity", "unité", "bill 96", "loi 21", "bill 21",
    "laicit*", "laïcit*", "lacit")),
  ## NOTE the 2025 script had a bare "21" here for loi 21. As a token
  ## that matches any stray "21"; replaced with the phrases above.

  ## ---- 17 Free Trade / Tariffs ---------------------------------------
  ## Code 17 was defined in the 2021 val_labels but never assigned.
  ## Reused rather than inventing a new number.
  tariff = list(code = 17, label = "Free_Trade", terms = c(
    "tariff*", "tarriff*", "tarrif*", "tarif*", "terrifs", "thariff",
    "economytariffs", "taxestariffs", "trade war", "big beautiful bill",
    "protectionis*", "usmca", "cusma", "nafta", "aceum")),
  ## NOTE "economist"/"économiste" were in the 2025 tariff dictionary.
  ## They are economy words and were inflating this count. Removed.

  ## ---- 18 Inflation --------------------------------------------------
  ## SPLIT OUT of the 2025 economy dictionary to restore the 2021 code 18.
  inflation = list(code = 18, label = "Inflation", terms = c(
    "inflation", "price*", "prix", "cost", "costs", "cout", "coût",
    "lecout", "cost of living", "costofliving", "rising cost",
    "afford*", "affordability", "afforability", "unaffordable",
    "expensive", "living", "wage*", "base rate", "food", "grocer*",
    "epicerie", "épicerie", "phone plan*")),

  ## ---- 19 Housing ----------------------------------------------------
  housing = list(code = 19, label = "Housing", terms = c(
    "housing", "housingaffordability", "inflationhousingrenting",
    "logement*", "rent", "rents", "rental", "renting", "loyer", "home",
    "homes", "hpmes", "house*", "dwelling", "maison*", "propri*",
    "homeless*", "sans-abris", "sans abris", "itinerance", "itinérance")),

  ## ---- 20 COVID-19 ---------------------------------------------------
  ## Kept so the 2021 and 2025 tables have the same rows. Expect ~0 in
  ## 2025. Note "19" alone is useless post-normalisation because
  ## "covid-19" collapses to the single token "covid19".
  covid = list(code = 20, label = "COVID19", terms = c(
    "covid", "covid19", "covid 19", "co-vid", "co vid", "convid", "copid",
    "covic", "cobid", "covit", "cvid", "civid", "covis", "pandem*",
    "pandém*", "pandè*", "pendé*", "pendemie", "pa ndemic", "pandémie",
    "epidemic", "épidémie", "vaccine*", "vax*", "corona*", "virus",
    "lockdown*", "return to normal", "retour normal", "normalité",
    "get a shot")),

  ## ---- 21 US Relations / Sovereignty  ** NEW ** -----------------------
  us_relations = list(code = 21, label = "US_Relations", terms = c(
    "border", "borders", "frontiere*", "frontière*", "annex*", "annexion",
    "sovereignty", "sovreignty", "soveriegnty", "souverain*",
    "souveraineté", "souverainté", "independen*", "indépend*",
    "independance", "indépendance", "autonomy", "51st",
    "us", "usa", "u.s.", "states", "the states", "etats unis",
    "les etats unis", "états-unis", "etats-unis", "etatsunis", "etasunis",
    "america", "american*", "americain*", "américain*", "neighbours",
    "canadian identity", "keeping canada",
    "l'intégrité territoriale", "lintegrite territoriale")),
  ## NOTE bare "us" also catches the English pronoun. It is kept because
  ## it was in the 2025 dictionary, but the audit in Section 5 will show
  ## you how much it is doing. Strongly consider deleting it.

  ## ---- 22 Trump  ** NEW ** -------------------------------------------
  trump = list(code = 22, label = "Trump", terms = c(
    "trump", "trumps", "trump's", "turmp", "dtrump", "bufoontrump",
    "usatrump", "donald", "president", "président", "maga")),

  ## ---- 23 Party Leaders  ** NEW ** -----------------------------------
  ## 2021 had no leaders code; these responses fell into Other.
  leaders = list(code = 23, label = "Party_Leaders", terms = c(
    "carney", "carnay", "mark", "marc", "poilievre", "poliviere", "pierre",
    "singh", "blanchet", "trudeau", "trudeau's", "justin", "scheer",
    "sheer", "o'toole", "otoole", "toole", "bernier", "ford", "may",
    "paul", "andrew", "leader*", "leadership", "pm", "prime minister",
    "candidate*", "politician*", "parties", "liberal*", "libéral*",
    "libéraux", "libral", "conservative*", "conservateur*",
    "conservatrices", "ndp", "npd", "bloc"))
)


## ---- Non-answers (NOT a category - mirrors 2021's mip_missing) -------
## `terms` are token/glob matches; `exact` are whole-response matches
## (2021 used mip_lower == "x" for these). After normalisation every
## punctuation-only response ("?", "...", "-") becomes "", so the single
## exact entry "" covers all of them.
missing_terms <- c(
  "nothing", "nothinh", "nothibg", "nth", "none", "nil", "nul", "rien",
  "aucun", "unsure", "unknown", "uninterested", "not sure",
  "don't know", "dont know", "i don't know", "i do not know",
  "don't knoe", "dnk", "d/k", "dunno", "idk", "sais pas", "ne sait pas",
  "je ne sais pas", "je sais pas", "j'ai pas de reponse",
  "no comment", "no issue", "no opinion", "no clue", "no interest",
  "no point", "no particular issue", "no strong feeling", "undecided",
  "neutral", "neutre", "netural", "not applicable", "n/a",
  "don't care", "dont care", "don't have", "dont have",
  "doesn't matter", "won't matter", "hard to pick", "not a citizen",
  "prefer not", "prefer not to say", "-99", "99")
## NOTE deliberately dropped from the 2021 list: bare "na", "pas",
## "non", "oui", "good", "no", "hm", "ei". Under substring matching
## they swallowed enormous numbers of real answers ("pas de logement",
## "no jobs"). Genuine one-word junk is caught by `missing_exact`.

missing_exact <- c(
  "", "no", "n", "yes", "y", "oui", "non", "na", "nope", "not", "the",
  "0", "1", "as", "f", "g", "j", "u", "x", "xxx", "ish", "oo", "td",
  "cul", "yup", "good", "hm", "very nice", "bye felicia", "fuck off",
  "i'm neutral", "je c po", "je men fou", "no ne", "no choice",
  "no matters", "not interested", "don't have one", "do not have one",
  "asdbf", "dssgdsdhsdg", "fghhjg", "gdfg", "gtghhgh", "gvfhyg", "hdfg",
  "hgg", "hiouoi", "hohgj", "ijhiojlkuty7", "jjdjr", "rhrhrh", "sdfvfx",
  "sfasegs", "sfdg", "tdfyfgj", "tyfytfyutiuyi", "t8a1z3", "ygggg",
  "yfghgkjj", "gosrimtne", "heqlyhcate", "ha")


## =====================================================================
## SECTION 2 - NORMALISATION + AUTOMATIC PHRASE COLLAPSING
## =====================================================================

## Character-level normalisation. `keep_glob = TRUE` preserves the "*"
## wildcards in dictionary terms; responses are cleaned without it.
##   é -> e | "don't" -> "dont" | "health-care" -> "healthcare"
##   "covid-19" -> "covid19" | "u.s." -> "us"
normalizeText <- function(x, keep_glob = FALSE) {
  punct <- if (keep_glob) "[^[:alnum:][:space:]*]" else "[^[:alnum:][:space:]]"
  x %>%
    as.character() %>%
    tidyr::replace_na("") %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(punct, "") %>%
    stringr::str_squish()
}

## Every term the script knows about, in one vector.
all_raw_terms <- c(
  unlist(lapply(issue_scheme, `[[`, "terms"), use.names = FALSE),
  missing_terms, missing_exact)

## The phrase set: anything still multi-word after normalisation.
## Stored WITHOUT globs, because that is the form the responses take.
phrase_set <- {
  n <- normalizeText(all_raw_terms, keep_glob = TRUE)
  p <- n[stringr::str_detect(n, "\\s")]
  p <- stringr::str_squish(stringr::str_remove_all(p, "\\*"))
  unique(p[nchar(p) > 0])
}

## Longest first, so "first past the post" is consumed before
## "first nation", and "les etats unis" before "etats unis".
phrase_set <- phrase_set[order(
  -stringr::str_count(phrase_set, "\\S+"), -nchar(phrase_set))]

## Replacement map applied to RESPONSES.
## Leading \\b only: omitting the trailing boundary lets "student loans"
## and "middle classes" collapse too, which is what you want given the
## glob terms on the dictionary side.
phrase_map <- stats::setNames(
  stringr::str_remove_all(phrase_set, "\\s"),
  paste0("\\b", phrase_set))

message(crayon::cyan(sprintf(
  "Auto-detected %d multi-word phrase(s) across %d categories.",
  length(phrase_map), length(issue_scheme))))
print(utils::head(data.frame(
  phrase = phrase_set,
  token  = unname(phrase_map),
  row.names = NULL), 60))

## Responses: normalise, then collapse phrases.
cleanText <- function(x) {
  out <- normalizeText(x, keep_glob = FALSE)
  if (length(phrase_map) > 0) out <- stringr::str_replace_all(out, phrase_map)
  stringr::str_squish(out)
}

## Dictionary terms: normalise keeping globs, then collapse the whole
## term to one token if (glob-stripped) it is a known phrase.
cleanTerms <- function(terms) {
  n    <- normalizeText(terms, keep_glob = TRUE)
  bare <- stringr::str_squish(stringr::str_remove_all(n, "\\*"))
  out  <- ifelse(bare %in% phrase_set, stringr::str_remove_all(n, "\\s"), n)
  unique(out[nchar(out) > 0])
}


## ---- clean the responses --------------------------------------------
## The old 2025 script contained
##   sub("^(\\w+)\\s+(\\w+)$", "\\2 \\1", ces25$cps25_imp_iss)
## which silently REVERSED the word order of every two-word response
## ("climate change" -> "change climate"). It is gone. Do not reinstate.
ces25$mip_lower <- cleanText(ces25[[RESP]])


## =====================================================================
## SECTION 3 - RUN THE DICTIONARIES
## =====================================================================

toks <- quanteda::tokens(ces25$mip_lower)

runDictionary <- function(toks, terms, key) {
  d <- quanteda::dictionary(stats::setNames(list(cleanTerms(terms)), key))
  m <- quanteda::dfm(quanteda::tokens_lookup(
    toks, d, valuetype = "glob", nested_scope = "dictionary"))
  m <- quanteda::dfm_match(m, features = key)
  as.integer(quanteda::convert(m, to = "data.frame")[[key]])
}

tictoc::tic("all dictionaries")
for (nm in names(issue_scheme)) {
  hits <- runDictionary(toks, issue_scheme[[nm]]$terms, nm)
  ces25[[paste0(nm, "_mip")]] <- as.integer(hits >= 1)
  message(crayon::green(sprintf(
    "%-15s (code %2d) %5d responses  %5.1f%%",
    issue_scheme[[nm]]$label, issue_scheme[[nm]]$code,
    sum(hits >= 1), 100 * mean(hits >= 1))))
}
tictoc::toc()

## Non-answers. Named mip_missing, and deliberately NOT ending in "_mip",
## so it stays out of the rowSums() below - same convention as 2021.
mm_hits <- runDictionary(toks, missing_terms, "missing")
ces25$mip_missing <- as.integer(
  mm_hits >= 1 | ces25$mip_lower %in% cleanText(missing_exact))
message(crayon::green(sprintf(
  "%-15s          %5d responses  %5.1f%%", "Non-answer",
  sum(ces25$mip_missing), 100 * mean(ces25$mip_missing))))

## Supplementary 2025-only composite: Trump + Tariffs + US Relations.
## Built as the union of its components rather than a fourth hand-typed
## copy that can drift. Named *_dum, NOT *_mip, so it cannot double-count
## in mip_total.
ces25$us_combined_dum <- as.integer(
  ces25$trump_mip + ces25$tariff_mip + ces25$us_relations_mip >= 1)


## =====================================================================
## SECTION 4 - THE 2021 mip PIPELINE
## Same logic as 2021: count how many issues each respondent triggered,
## keep the single-issue respondents, map them onto the numeric scheme.
## =====================================================================

ces25 <- ces25 %>%
  mutate(mip_total = rowSums(across(ends_with("_mip")), na.rm = TRUE))

## How many respondents triggered 0, 1, 2, ... categories
table(ces25$mip_total)

## 2021 defined a "single" respondent as mip_total == 1
ces25 <- ces25 %>% mutate(mip_single = as.integer(mip_total == 1))

## mip2: the category NAME, for single-issue respondents only.
mip_cols <- paste0(names(issue_scheme), "_mip")
M <- as.matrix(ces25[, mip_cols])
ces25$mip2 <- ifelse(
  rowSums(M, na.rm = TRUE) == 1,
  names(issue_scheme)[max.col(M, ties.method = "first")],
  NA_character_)

## mip: the numeric code. Built by lookup from issue_scheme, so the
## numbers cannot drift from the labels the way a hand-written
## case_when() chain does.
code_lookup  <- vapply(issue_scheme, `[[`, numeric(1), "code")
ces25$mip    <- unname(code_lookup[ces25$mip2])
## Everyone who answered something codeable but matched nothing -> Other
ces25$mip[is.na(ces25$mip) & ces25$mip_total == 0 & ces25$mip_missing == 0] <- 0

label_lookup <- vapply(issue_scheme, `[[`, character(1), "label")
val_labels(ces25$mip) <- stats::setNames(
  c(0, unname(code_lookup)), c("Other", unname(label_lookup)))

table(as_factor(ces25$mip), useNA = "ifany")

## ---- strict 2021-comparable collapse ---------------------------------
## Pools the three 2025-only US codes into Foreign_Affairs and folds
## Party_Leaders back into Other, so the series lines up with 2021.
ces25 <- ces25 %>%
  mutate(mip21 = case_when(
    mip %in% c(17, 21, 22) ~ 12,
    mip == 23              ~ 0,
    TRUE                   ~ mip))
val_labels(ces25$mip21) <- c(
  Other = 0, Environment = 1, Crime = 2, Ethics = 3, Education = 4,
  Energy = 5, Jobs = 6, Economy = 7, Health = 8, Taxes = 9,
  Deficit_Debt = 10, Democracy = 11, Foreign_Affairs = 12,
  Immigration = 13, Socio_Cultural = 14, Social_Programs = 15,
  Brokerage = 16, Free_Trade = 17, Inflation = 18, Housing = 19,
  COVID19 = 20)

table(as_factor(ces25$mip21), useNA = "ifany")


## =====================================================================
## SECTION 5 - AUDITS. Run these before you report anything.
## =====================================================================

## (a) OVERLAP. A term in two categories pushes respondents to
##     mip_total == 2, which sets mip to NA and DESTROYS the case.
##     This was doing real damage in 2021 ("reconcil*" in both
##     Socio_Cultural and Brokerage, "sécurité" in both Crime and
##     Foreign_Affairs, "poor" in both Economy and Social_Programs).
overlapTerms <- function(scheme) {
  long <- purrr::imap_dfr(scheme, ~ tibble::tibble(
    category = .y, term = cleanTerms(.x$terms)))
  long %>%
    group_by(term) %>%
    filter(n() > 1) %>%
    summarise(categories = paste(sort(category), collapse = " + "),
              .groups = "drop") %>%
    arrange(categories, term)
}
overlaps <- overlapTerms(issue_scheme)
message(crayon::yellow(sprintf(
  "%d term(s) appear in more than one category.", nrow(overlaps))))
print(overlaps, n = 200)

## (b) DEAD TERMS. Terms that match nothing in the data - usually
##     typos-of-typos carried over from a previous wave.
deadTerms <- function(scheme, cleaned_responses) {
  toks_seen <- unique(unlist(stringr::str_split(cleaned_responses, "\\s+")))
  toks_seen <- toks_seen[nchar(toks_seen) > 0]
  purrr::imap_dfr(scheme, function(x, nm) {
    cl   <- cleanTerms(x$terms)
    live <- vapply(cl, function(t)
      any(grepl(utils::glob2rx(t), toks_seen)), logical(1))
    if (all(live)) return(NULL)
    tibble::tibble(category = nm, term = cl[!live])
  })
}
dead <- deadTerms(issue_scheme, ces25$mip_lower)
message(crayon::yellow(sprintf(
  "%d dictionary term(s) match nothing in the data.", nrow(dead))))
print(dead, n = 300)

## (c) UNCODED. The 2021 workflow: look at what fell through, add terms,
##     rerun. Repeat until the tail is genuinely idiosyncratic.
ces25 %>%
  filter(mip_total == 0, mip_missing == 0) %>%
  count(mip_lower, sort = TRUE) %>%
  print(n = 200)

ces25 %>% filter(mip_total == 0, mip_missing == 0) %>% nrow()

## (d) How much work is bare "us" doing in US_Relations? If most of these
##     are the pronoun, delete "us" from that term list.
ces25 %>%
  filter(us_relations_mip == 1, stringr::str_detect(mip_lower, "\\bus\\b")) %>%
  count(mip_lower, sort = TRUE) %>%
  print(n = 50)


## =====================================================================
## SECTION 6 - SUMMARY TABLE
## =====================================================================

tbl_labels <- stats::setNames(
  as.list(unname(label_lookup)), paste0(names(issue_scheme), "_mip"))

ces25 %>%
  tbl_summary(
    include = c(all_of(mip_cols), mip_missing, us_combined_dum),
    label   = c(tbl_labels, list(
      mip_missing     ~ "Don't know / did not answer",
      us_combined_dum ~ "US Relations, Trump and Tariffs (composite)")))
