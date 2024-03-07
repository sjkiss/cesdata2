library(tidyverse)
library(here)
data("ces21")
lookfor(ces21, "important issue")
#Convert cps21_imp_iss to lower
ces21 %>%
  mutate(mip_lower=str_trim(str_to_lower(cps21_imp_iss)))->ces21

#This shows the unique responses to most important issue with counts
ces21 %>%
  group_by(mip_lower) %>%
  count() %>%
  arrange(desc(n))

#This puts that out into a csv file for inspection in excel
ces21 %>%
  group_by(mip_lower) %>%
  count() %>%
  arrange(desc(n)) %>%
  write_csv(here("data-raw/2021_mip_unique.csv"))

#We need to code these responses as best we can
# I would like a series of dichotomous variables for each issue in this model in this list
#Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9,
                              #Deficit_Debt=10, Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16)
# At the start =here are 8949 responses
# In the space of about 45 minutes of work I have gotten that down to about 4500
# So it can go pretty quick.
# The Environment
ces21 %>%
  mutate(enviro_mip=case_when(
    str_detect(mip_lower, "environ*")~1,
    str_detect(mip_lower, "climat*")~1,
    str_detect(mip_lower, "environnement")~1,
    str_detect(mip_lower, "l'environnement")~1,
    str_detect(mip_lower, "climate")~1,
    str_detect(mip_lower, "changement*")~1,
    str_detect(mip_lower, "climatiques")~1,
    str_detect(mip_lower, "écolog*")~1,
    str_detect(mip_lower, "ecolog*")~1,
    str_detect(mip_lower, "pollution")~1,
    str_detect(mip_lower, "planète")~1,
    str_detect(mip_lower, "drinking water")~1,
    str_detect(mip_lower, "carbon")~1,
    str_detect(mip_lower, "green")~1,
    str_detect(mip_lower, "envion*")~1,
    str_detect(mip_lower, "envrion*")~1,
    str_detect(mip_lower, "ev")~1,
    str_detect(mip_lower, "cl8mate change")~1,
    str_detect(mip_lower, "climet chang")~1,
    str_detect(mip_lower, "emviron*")~1,
    str_detect(mip_lower, "verte")~1,
    str_detect(mip_lower, "eniron*")~1,
    str_detect(mip_lower, "envirron*")~1,
    str_detect(mip_lower, "l'envér*")~1,
    str_detect(mip_lower, "planet*")~1,
    str_detect(mip_lower, "l'air")~1,
    str_detect(mip_lower, "envir*")~1,
    str_detect(mip_lower, "renewable*")~1,
    str_detect(mip_lower, "clkmatique")~1,
    str_detect(mip_lower, "pesticide*")~1,
    str_detect(mip_lower, "61")~1,
    str_detect(mip_lower, "gaz a effet de serre")~1,
    str_detect(mip_lower, "nuclear")~1,
  ))->ces21
# Crime
ces21 %>%
  mutate(crime_mip=case_when(
    str_detect(mip_lower, "crime|crimin")~1,
    str_detect(mip_lower, "safe*")~1,
    str_detect(mip_lower, "security")~1,
    str_detect(mip_lower, "securité")~1,
    str_detect(mip_lower, "terroris*")~1,
    str_detect(mip_lower, "law")~1,
  ))->ces21
#Ethics
ces21 %>%
  mutate(ethics_mip=case_when(
    str_detect(mip_lower, "honest*")~1,
    str_detect(mip_lower, "integrity")~1,
    str_detect(mip_lower, "ethic*")~1,
    str_detect(mip_lower, "corrupt*")~1,
    str_detect(mip_lower, "truth")~1,
    str_detect(mip_lower, "justice")~1,
    str_detect(mip_lower, "trust")~1,
    str_detect(mip_lower, "fair*")~1,
    str_detect(mip_lower, "honnêteté")~1,
    str_detect(mip_lower, "lying")~1,
    str_detect(mip_lower, "liar*")~1,
    str_detect(mip_lower, "crook*")~1,
    str_detect(mip_lower, "hypocrisy")~1,
    str_detect(mip_lower, "honnet*")~1,
    str_detect(mip_lower, "integre*")~1,
    str_detect(mip_lower, "intégri*")~1,
    str_detect(mip_lower, "l'honn*")~1,
    str_detect(mip_lower, "l'intégr*")~1,
    str_detect(mip_lower, "l'honn*")~1,
    str_detect(mip_lower, "lie*")~1,
    str_detect(mip_lower, "ėquité")~1,
    str_detect(mip_lower, "éthique")~1,
    str_detect(mip_lower, "équité")~1,
    str_detect(mip_lower, "l'éthique")~1,
    str_detect(mip_lower, "corupt*")~1,
    str_detect(mip_lower, "lieing*")~1,
    str_detect(mip_lower, "moral*")~1,
    str_detect(mip_lower, "vérité*")~1,
    str_detect(mip_lower, "decency")~1,
    str_detect(mip_lower, "honnête")~1,
  ))->ces21
# Education
ces21 %>%
  mutate(education_mip=case_when(
    str_detect(mip_lower, "educat*")~1,
    str_detect(mip_lower, "éducat*")~1,
    str_detect(mip_lower, "educ")~1,
  ))->ces21
# Energy
ces21 %>%
  mutate(energy_mip=case_when(
    str_detect(mip_lower, "pipeline*")~1,
    str_detect(mip_lower, "energy")~1,
    str_detect(mip_lower, "oil")~1,
  ))->ces21

# Jobs
ces21 %>%
  mutate(jobs_mip=case_when(
    str_detect(mip_lower, "job*")~1,
    str_detect(mip_lower, "employ*")~1,
    str_detect(mip_lower, "income")~1,
    str_detect(mip_lower, "d'oeuvre")~1,
    str_detect(mip_lower, "personnel")~1,
    str_detect(mip_lower, "work")~1,
    str_detect(mip_lower, "career")~1,
    str_detect(mip_lower, "salar*")~1,
    str_detect(mip_lower, "unempl*")~1,
  ))->ces21
# Economy
ces21 %>%
  mutate(economy_mip=case_when(
    str_detect(mip_lower, "econom*")~1,
    str_detect(mip_lower, "économie")~1,
    str_detect(mip_lower, "economie")~1,
    str_detect(mip_lower, "economic recovery")~1,
    str_detect(mip_lower, "financ*")~1,
    str_detect(mip_lower, "money*")~1,
    str_detect(mip_lower, "économique")~1,
    str_detect(mip_lower, "l'argent")~1,
    str_detect(mip_lower, "argent")~1,
    str_detect(mip_lower, "ecomony")~1,
    str_detect(mip_lower, "recovery")~1,
    str_detect(mip_lower, "wealth")~1,
    str_detect(mip_lower, "interest rate*")~1,
    str_detect(mip_lower, "industry")~1,
    str_detect(mip_lower, "growth")~1,
    str_detect(mip_lower, "econ*")~1,
    str_detect(mip_lower, "business concern*")~1,
    str_detect(mip_lower, "développemen")~1,
    str_detect(mip_lower, "ecnomic")~1,
    str_detect(mip_lower, "ècon*")~1,
    str_detect(mip_lower, "ecenomy")~1,
    str_detect(mip_lower, "éconmique")~1,
    mip_lower=="$" ~1,
      ))->ces21
# Health and Health Care
ces21 %>%
  mutate(health_mip=case_when(
    str_detect(mip_lower, "health*")~1,
    str_detect(mip_lower, "sant*")~1,
    str_detect(mip_lower, "pharma*")~1,
    str_detect(mip_lower, "long term*")~1,
    str_detect(mip_lower, "medical*")~1,
    str_detect(mip_lower, "heath*")~1,
    str_detect(mip_lower, "opioid")~1,
    str_detect(mip_lower, "drug*")~1,
    str_detect(mip_lower, "doctor*")~1,
    str_detect(mip_lower, "heaalthcare")~1,
    str_detect(mip_lower, "heaith care")~1,
    str_detect(mip_lower, "heakth caew")~1,
    str_detect(mip_lower, "helath")~1,
    str_detect(mip_lower, "prescript*")~1,
    str_detect(mip_lower, "optometr*")~1,
    str_detect(mip_lower, "overdose")~1,
    str_detect(mip_lower, "opiod")~1,
  ))->ces21
# Socio-Cultural
ces21 %>%
  mutate(socio_cultural_mip=case_when(
    str_detect(mip_lower, "trans")~1,
    str_detect(mip_lower, "identity")~1,
    str_detect(mip_lower, "values")~1,
    str_detect(mip_lower, "equal*")~1,
    str_detect(mip_lower, "human*")~1,
    str_detect(mip_lower, "indigenous*")~1,
    str_detect(mip_lower, "minority")~1,
    str_detect(mip_lower, "rights")~1,
    str_detect(mip_lower, "gun*")~1,
    str_detect(mip_lower, "firearm*")~1,
    str_detect(mip_lower, "fire arm*")~1,
    str_detect(mip_lower, "abort*")~1,
    str_detect(mip_lower, "freedom")~1,
    str_detect(mip_lower, "liberté")~1,
    str_detect(mip_lower, "equity")~1,
    str_detect(mip_lower, "libert*")~1,
    str_detect(mip_lower, "free speech")~1,
    str_detect(mip_lower, "droit")~1,
    str_detect(mip_lower, "égalité")~1,
    str_detect(mip_lower, "accessibility")~1,
    str_detect(mip_lower, "anti-semitism")~1,
    str_detect(mip_lower, "armes")~1,
    str_detect(mip_lower, "autochtones")~1,
    str_detect(mip_lower, "divers*")~1,
    str_detect(mip_lower, "lgbt*")~1,
    str_detect(mip_lower, "woke")~1,
    str_detect(mip_lower, "c-6")~1,
    str_detect(mip_lower, "c-71")~1,
    str_detect(mip_lower, "cancel-culture")~1,
    str_detect(mip_lower, "inclusiv*")~1,
    str_detect(mip_lower, "indig*")~1,
    str_detect(mip_lower, "cultur*")~1,
    str_detect(mip_lower, "loi 21")~1,
    str_detect(mip_lower, "c21")~1,
    str_detect(mip_lower, "cultur*")~1,
    str_detect(mip_lower, "women*")~1,
    str_detect(mip_lower, "treaty")~1,
    str_detect(mip_lower, "trc")~1,
    str_detect(mip_lower, "reconciliation")~1,
    str_detect(mip_lower, "minorit*")~1,
    str_detect(mip_lower, "autochtone")~1,
    str_detect(mip_lower, "weapons ownership")~1,
    str_detect(mip_lower, "feministe")~1,
    str_detect(mip_lower, "feminine")~1,
    str_detect(mip_lower, "islamophobia")~1,
    str_detect(mip_lower, "race")~1,
  ))->ces21
# Social Programs
ces21 %>%
  mutate(social_mip=case_when(
    str_detect(mip_lower, "poverty")~1,
    str_detect(mip_lower, "child*")~1,
    str_detect(mip_lower, "enfants")~1,
    str_detect(mip_lower, "homeless*")~1,
    str_detect(mip_lower, "senior*")~1,
    str_detect(mip_lower, "racis*")~1,
    str_detect(mip_lower, "universal basic*")~1,
    str_detect(mip_lower, "basic income")~1,
    str_detect(mip_lower, "day*")~1,
    str_detect(mip_lower, "social*")~1,
    str_detect(mip_lower, "disability")~1,
    str_detect(mip_lower, "disabl*")~1,
    str_detect(mip_lower, "âgées")~1,
    str_detect(mip_lower, "pauvreté")~1,
    str_detect(mip_lower, "ainés")~1,
    str_detect(mip_lower, "ainé*")~1,
    str_detect(mip_lower, "aîné*")~1,
    str_detect(mip_lower, "retraités")~1,
    str_detect(mip_lower, "sick leave")~1,
    str_detect(mip_lower, "old*")~1,
    str_detect(mip_lower, "elder*")~1,
    str_detect(mip_lower, "agee*")~1,
    str_detect(mip_lower, "reserve*")~1,
    str_detect(mip_lower, "pension*")~1,
    str_detect(mip_lower, "cerb")~1,
    str_detect(mip_lower, "pcre")~1,
    str_detect(mip_lower, "autis*")~1,
    str_detect(mip_lower, "enfant")~1,
    str_detect(mip_lower, "retir*")~1,
    str_detect(mip_lower, "agée*")~1,
    str_detect(mip_lower, "longterm care")~1,
    str_detect(mip_lower, "veteran*")~1,
    str_detect(mip_lower, "pauvr*")~1,
    str_detect(mip_lower, "65")~1,
    str_detect(mip_lower, "poor")~1,
    str_detect(mip_lower, "vieillesse")~1,
    str_detect(mip_lower, "impoverish*")~1,
    str_detect(mip_lower, "senoir*")~1,
    str_detect(mip_lower, "seanor")~1,
    str_detect(mip_lower, "odsp")~1,
    str_detect(mip_lower, "60 ans")~1,
    str_detect(mip_lower, "ltc")~1,
    str_detect(mip_lower, "oas")~1,
    str_detect(mip_lower, "under-privileged")~1,
    str_detect(mip_lower, "ei")~1,
    str_detect(mip_lower, "garde*")~1,
    str_detect(mip_lower, "pcu")~1,
    str_detect(mip_lower, "ccb")~1,
    str_detect(mip_lower, "cpp")~1,
    str_detect(mip_lower, "âgé*")~1,
    str_detect(mip_lower, "wsib")~1,
  ))->ces21
# Brokerage (Canada, Quebec Fed- Prov)
ces21 %>%
  mutate(brokerage_mip=case_when(
    str_detect(mip_lower, "reconcil*")~1,
    str_detect(mip_lower, "provinciales")~1,
    str_detect(mip_lower, "provinces")~1,
    str_detect(mip_lower, "québec")~1,
    str_detect(mip_lower, "français")~1,
    str_detect(mip_lower, "provinciaux")~1,
    str_detect(mip_lower, "autonomie")~1,
    str_detect(mip_lower, "quebec")~1,
    str_detect(mip_lower, "federalis*")~1,
    str_detect(mip_lower, "fédéral*")~1,
    str_detect(mip_lower, "québécois")~1,
    str_detect(mip_lower, "fédérale")~1,
    str_detect(mip_lower, "fédéral*")~1,
    str_detect(mip_lower, "province jurisdiction")~1,
    str_detect(mip_lower, "fédéral*")~1,
    str_detect(mip_lower, "juridictions")~1,
    str_detect(mip_lower, "provinciale")~1,
    str_detect(mip_lower, "provincial")~1,
  ))->ces21
# Inflation
ces21 %>%
  mutate(inflation_mip=case_when(
    str_detect(mip_lower, "inflation")~1,
    str_detect(mip_lower, "prices")~1,
    str_detect(mip_lower, "cost of living")~1,
    str_detect(mip_lower, "coût")~1,
    str_detect(mip_lower, "afford*")~1,
    str_detect(mip_lower, "living")~1,
    str_detect(mip_lower, "wage*")~1,
    str_detect(mip_lower, "dépenses")~1,
    str_detect(mip_lower, "cout")~1,
    str_detect(mip_lower, "cost")~1,
    str_detect(mip_lower, "price of food")~1,
  ))->ces21
# Housing, rent
ces21 %>%
  mutate(housing_mip=case_when(
    str_detect(mip_lower, "logement")~1,
    str_detect(mip_lower, "housing")~1,
    str_detect(mip_lower, "rent")~1,
    str_detect(mip_lower, "home")~1,
    str_detect(mip_lower, "house*")~1,
    str_detect(mip_lower, "maison")~1,
    str_detect(mip_lower, "hpmes")~1,
    str_detect(mip_lower, "propri*")~1,
  ))->ces21
# COVID
ces21 %>%
  mutate(covid_mip=case_when(
    str_detect(mip_lower, "covid")~1,
    str_detect(mip_lower, "pandémie")~1,
    str_detect(mip_lower, "pandem*")~1,
    str_detect(mip_lower, "vaccine*")~1,
    str_detect(mip_lower, "corona*")~1,
    str_detect(mip_lower, "virus")~1,
    str_detect(mip_lower, "co-vid")~1,
    str_detect(mip_lower, "pendemie")~1,
    str_detect(mip_lower, "lockdown*")~1,
    str_detect(mip_lower, "vax*")~1,
    str_detect(mip_lower, "19")~1,
    str_detect(mip_lower, "get a shot")~1,
    str_detect(mip_lower, "co vid")~1,
    str_detect(mip_lower, "convid")~1,
    str_detect(mip_lower, "copid")~1,
    str_detect(mip_lower, "covic")~1,
    str_detect(mip_lower, "cobid")~1,
    str_detect(mip_lower, "a la normal*")~1,
    str_detect(mip_lower, "covit")~1,
    str_detect(mip_lower, "cvid")~1,
    str_detect(mip_lower, "pendé*")~1,
    str_detect(mip_lower, "pandè*")~1,
    str_detect(mip_lower, "civid")~1,
    str_detect(mip_lower, "covis")~1,
    str_detect(mip_lower, "pandém*")~1,
    str_detect(mip_lower, "la normal")~1,
    str_detect(mip_lower, "retour normal")~1,
    str_detect(mip_lower, "pa ndemic")~1,
    str_detect(mip_lower, "normalité")~1,
    str_detect(mip_lower, "epidemic")~1,
    str_detect(mip_lower, "normale")~1,
    str_detect(mip_lower, "vrai normal")~1,
    str_detect(mip_lower, "à normal")~1,
  ))->ces21
# Taxes
ces21 %>%
  mutate(taxes_mip=case_when(
    str_detect(mip_lower, "tax|tax*")~1,
    str_detect(mip_lower, "impot")~1,
    str_detect(mip_lower, "impôt*")~1,
    str_detect(mip_lower, "tqxes")~1,
  ))->ces21
# Debt and Deficit
ces21 %>%
  mutate(debt_mip=case_when(
    str_detect(mip_lower, "debt|deficit")~1,
    str_detect(mip_lower, "budget")~1,
    str_detect(mip_lower, "la dette")~1,
    str_detect(mip_lower, "dette*")~1,
    str_detect(mip_lower, "fiscal*")~1,
    str_detect(mip_lower, "government spend*")~1,
    str_detect(mip_lower, "spend*")~1,
    str_detect(mip_lower, "déficit")~1,
    str_detect(mip_lower, "debit")~1,
    str_detect(mip_lower, "austerity")~1,
    str_detect(mip_lower, "defic*")~1,
    str_detect(mip_lower, "defec*")~1,
    str_detect(mip_lower, "depenses*")~1,
    str_detect(mip_lower, "budjet*")~1,
    str_detect(mip_lower, "bugdet*")~1,
    str_detect(mip_lower, "budgè*")~1,
    str_detect(mip_lower, "buget")~1,
  ))->ces21
# Democracy
ces21 %>%
  mutate(democracy_mip=case_when(
    str_detect(mip_lower, "democr*")~1,
    str_detect(mip_lower, "elect*")~1,
    str_detect(mip_lower, "vot*")~1,
    str_detect(mip_lower, "demo0cr5atic")~1,
    str_detect(mip_lower, "démocr*")~1,
    str_detect(mip_lower, "élect*")~1,
    str_detect(mip_lower, "ballot*")~1,
    str_detect(mip_lower, "demovracy")~1,
    str_detect(mip_lower, "scrutin")~1,
  ))->ces21
# Foreign Affairs
ces21 %>%
  mutate(foreign_mip=case_when(
    str_detect(mip_lower, "peace|war")~1,
    str_detect(mip_lower, "chin*")~1,
    str_detect(mip_lower, "foreign policy")~1,
    str_detect(mip_lower, "border")~1,
    str_detect(mip_lower, "defence")~1,
    str_detect(mip_lower, "defense")~1,
    str_detect(mip_lower, "foreign")~1,
    str_detect(mip_lower, "gaza")~1,
    str_detect(mip_lower, "défense")~1,
    str_detect(mip_lower, "armed force*")~1,
    str_detect(mip_lower, "sécurité")~1,
    str_detect(mip_lower, "défense")~1,
    str_detect(mip_lower, "israel")~1,
    str_detect(mip_lower, "jew*")~1,
    str_detect(mip_lower, "usa")~1,
  ))->ces21
# Immigration
ces21 %>%
  mutate(immigration_mip=case_when(
    str_detect(mip_lower, "immigr*")~1,
    str_detect(mip_lower, "inmigr*")~1,
    str_detect(mip_lower, "refugee*")~1,
    str_detect(mip_lower, "réfugiés")~1,
    str_detect(mip_lower, "imagrat*")~1,
    str_detect(mip_lower, "immagr*")~1,
    str_detect(mip_lower, "third world people")~1,
    str_detect(mip_lower, "frontiere")~1,
    str_detect(mip_lower, "imigrat*")~1,
    str_detect(mip_lower, "émigrat*")~1,
    str_detect(mip_lower, "visa*")~1,
    str_detect(mip_lower, "sponsor")~1,
    str_detect(mip_lower, "refudgee")~1,
  ))->ces21

#coding indecisive as missing
ces21 %>%
  mutate(mip_missing=case_when(
    str_detect(mip_lower, "nothing")~1,
    str_detect(mip_lower, "-99")~1,
    str_detect(mip_lower, "not sure")~1,
    str_detect(mip_lower, "none")~1,
    str_detect(mip_lower, "sais pas")~1,
    str_detect(mip_lower, "aucun")~1,
    str_detect(mip_lower, "n/a")~1,
    str_detect(mip_lower, "na")~1,
    str_detect(mip_lower, "don’t know")~1,
    str_detect(mip_lower, "i don't know")~1,
    str_detect(mip_lower, "unsure")~1,
    str_detect(mip_lower, "idk")~1,
    str_detect(mip_lower, "rien")~1,
    str_detect(mip_lower, "no comment")~1,
    str_detect(mip_lower, "no issue")~1,
    str_detect(mip_lower, "good")~1,
    str_detect(mip_lower, "no issue")~1,
    str_detect(mip_lower, "dont know")~1,
    str_detect(mip_lower, "ne sait pas")~1,
    str_detect(mip_lower, "no opinion")~1,
    str_detect(mip_lower, "undecided")~1,
    str_detect(mip_lower, "dunno")~1,
    str_detect(mip_lower, "dont know")~1,
    str_detect(mip_lower, "i do not know")~1,
    str_detect(mip_lower, "pas")~1,
    str_detect(mip_lower, "don't care")~1,
    str_detect(mip_lower, "nil")~1,
    str_detect(mip_lower, "non")~1,
    str_detect(mip_lower, "not applicable")~1,
    str_detect(mip_lower, "don't have")~1,
    str_detect(mip_lower, "dont have")~1,
    str_detect(mip_lower, "doesn't matter")~1,
    str_detect(mip_lower, "haven’t decided")~1,
    str_detect(mip_lower, "hm")~1,
    str_detect(mip_lower, "prefer not")~1,
    str_detect(mip_lower, "dont see the point")~1,
    str_detect(mip_lower, "does it really matter")~1,
    str_detect(mip_lower, "don't knoe")~1,
    str_detect(mip_lower, "i don’t care")~1,
    str_detect(mip_lower, "i don’t have*")~1,
    str_detect(mip_lower, "neutre")~1,
    str_detect(mip_lower, "neutral")~1,
    str_detect(mip_lower, "hard to pick")~1,
    str_detect(mip_lower, "not a citizen")~1,
    str_detect(mip_lower, "none")~1,
    str_detect(mip_lower, "won’t matter")~1,
    str_detect(mip_lower, "no point")~1,
    str_detect(mip_lower, "dont care")~1,
    str_detect(mip_lower, "oui")~1,
    str_detect(mip_lower, "don’t mnow")~1,
    mip_lower=="no" ~1,
    mip_lower=="?" ~1,
    mip_lower=="??" ~1,
    mip_lower=="???" ~1,
    mip_lower=="." ~1,
    mip_lower=="..." ~1,
    mip_lower=="....." ~1,
    mip_lower=="-" ~1,
    mip_lower=="don't care" ~1,
    mip_lower=="don't know" ~1,
    mip_lower=="0" ~1,
    mip_lower=="1" ~1,
    mip_lower=="" ~1,
    mip_lower=="as" ~1,
    mip_lower=="asdbf" ~1,
    mip_lower=="bye felicia" ~1,
    mip_lower=="d/k" ~1,
    mip_lower=="dnk" ~1,
    mip_lower=="don't have one" ~1,
    mip_lower=="don't  know" ~1,
    mip_lower=="dssgdsdhsdg" ~1,
    mip_lower=="djdjdjdbenebe ebejbdbdjd jdbdbd" ~1,
    mip_lower=="f" ~1,
    mip_lower=="fghhjg" ~1,
    mip_lower=="fuck off" ~1,
    mip_lower=="g" ~1,
    mip_lower=="gdfg" ~1,
    mip_lower=="gtghhgh" ~1,
    mip_lower=="gvfhyg" ~1,
    mip_lower=="ha!" ~1,
    mip_lower=="hdfg" ~1,
    mip_lower=="hgg" ~1,
    mip_lower=="hiouoi" ~1,
    mip_lower=="hohgj" ~1,
    mip_lower=="ijhiojlkuty7" ~1,
    mip_lower=="ish" ~1,
    mip_lower=="j" ~1,
    mip_lower=="jjdjr" ~1,
    mip_lower=="f" ~1,
    mip_lower=="heqlyhcate" ~1,
    mip_lower=="i do not have one" ~1,
    mip_lower=="i don’t care" ~1,
    mip_lower=="i’m neutral" ~1,
    mip_lower=="je c po" ~1,
    mip_lower=="je men fou" ~1,
    mip_lower=="gosrimtne" ~1,
    mip_lower=="ygggg" ~1,
    mip_lower=="yfghgkjj" ~1,
    mip_lower=="y" ~1,
    mip_lower=="xxx" ~1,
    mip_lower=="x" ~1,
    mip_lower=="…." ~1,
    mip_lower=="unknown" ~1,
    mip_lower=="uninterested" ~1,
    mip_lower=="u" ~1,
    mip_lower=="tyfytfyutiuyi" ~1,
    mip_lower=="the" ~1,
    mip_lower=="tdfyfgj" ~1,
    mip_lower=="td" ~1,
    mip_lower=="t8a1z3" ~1,
    mip_lower=="sfasegs" ~1,
    mip_lower=="sfdg" ~1,
    mip_lower=="oo" ~1,
    mip_lower=="n" ~1,
    mip_lower=="sdfvfx" ~1,
    mip_lower=="yup" ~1,
    mip_lower=="very nice" ~1,
    mip_lower=="rhrhrh" ~1,
    mip_lower=="nul" ~1,
    mip_lower=="nth" ~1,
    mip_lower=="no ne" ~1,
    mip_lower=="nothinh" ~1,
    mip_lower=="not interested" ~1,
    mip_lower=="not" ~1,
    mip_lower=="nope" ~1,
    mip_lower=="no strong feeling" ~1,
    mip_lower=="no matters" ~1,
    mip_lower=="no clue" ~1,
    mip_lower=="no choice" ~1,
    mip_lower=="netural" ~1,
    mip_lower=="netural" ~1,
    mip_lower=="cul" ~1,
    mip_lower=="yes" ~1,
    mip_lower=="don’t care" ~1,
    mip_lower=="nothibg" ~1,
    mip_lower=="don’t have one" ~1,
    mip_lower=="don’t knoe" ~1,
    mip_lower=="no interest" ~1,
    mip_lower=="no particular issue" ~1,
    TRUE ~ 0
  ))->ces21

tail(names(ces21))
#This code calculates the sum of issues that have been coded
ces21 %>%
  mutate(mip_total=rowSums(across(ends_with("_mip")), na.rm=T))->ces21
### This code will show the responses that have no mip codes that have been assigned
# This should be run repeatedly to iteratively capture as many topics as possible


ces21 %>%
  filter(mip_total<1) %>%
  filter(mip_missing<1)%>%
  group_by(mip_lower) %>%
  count() %>%
  arrange(desc(n))%>%
  print(n=1000)
### This will go quicker than you think.
### 1) For example, adding str_detect(mip_lower, "santé") combine multiple responses.
### As will housing.
### 2) str_detect() takes what is called a regular expression. See here https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
### So for example, * is a wildcard. "democr*" will capture democracy and democratic
### immigr* will capture immigrants and immigration
### 3) French here will obviously be a particular challenge, but basically just google stuff as it comes up.
summary(ces21$mip_total)
ces21 %>%
  select(ends_with("_mip")) %>%
  View()
