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
