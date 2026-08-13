## =====================================================================
## CES 2025 - Most Important Problem (cps25_imp_iss)
## Dictionary coding, re-cut onto the CES 2021 category frame
## =====================================================================
##
## WHAT THIS IS
## ------------
## The CES21 script (ces21_recode.R) codes the MIP question with
## str_detect() patterns; this script codes CES25 with quanteda
## dictionaries. The two now use the SAME categories and the SAME
## vocabulary, so the waves can be compared directly.
##
## CATEGORY FRAME (numbers match the CES21 val_labels)
##    1 Environment            11 Democracy
##    2 Crime                  12 Foreign Affairs   <- was "security"
##    3 Ethics and Government  13 Immigration
##    4 Education              14 Socio-Cultural    <- women/abortion/race/
##    5 Energy                                         indigenous/rights/LGBTQ
##    6 Jobs                   15 Social Programs   <- welfare + seniors
##    7 Economy                16 Brokerage         <- Quebec, fed-prov
##    8 Health                (17 Free Trade - not coded, as in CES21)
##    9 Taxes                  18 Inflation         <- cost of living
##   10 Debt and Deficit       19 Housing
##                            (20 COVID - DROPPED for 2025)
##   CES25-only additions:
##   21 Trump  22 Tariffs  23 US Relations / Borders  24 Leaders
##
## CHANGES FROM THE PREVIOUS CES25 SCRIPT
## --------------------------------------
##  * "security" -> "foreign" (Foreign Affairs), and it now carries the
##    CES21 foreign-affairs vocabulary (peace/war, china, defence, gaza,
##    israel, foreign policy, armed forces...).
##  * "ethics" -> Ethics AND Government (government, gouvernement,
##    governance, bureaucracy added; "rights" moved OUT to socio-cultural).
##  * The old catch-all "economy" dictionary is split the way CES21 splits
##    it: economy / jobs / taxes / debt (incl. government spending) /
##    inflation (cost of living).
##  * "election" -> "democracy" (elections, voting, ballots, referendum,
##    constitution, parliament).
##  * "women" + "race" + "indigenous" -> one socio_cultural category, and
##    it now includes rights, gay rights, LGBTQ, trans, abortion, freedom,
##    values, culture, guns, misinformation.
##  * "welfare" + "seniors" -> one social (Social Programs) category.
##  * "quebec" -> "brokerage" (Quebec, provinces, federalism, language,
##    Bill 21 / Bill 96, national unity, western alienation).
##  * "energy" split out of the environment dictionary, as in CES21.
##  * "healthcare" -> "health" (CES21 name), CES21 vocabulary added.
##  * Trump / tariff / borders / combined / leaders kept as they were.
##  * NO covid category.
##  * Multi-word patterns ("cost of living", "basic income", "free speech",
##    "first past the post", ...) are handled automatically - see the
##    phrase-map section below.
##
## HOW TO EDIT: every category's vocabulary lives in ONE place, the
## mip_terms list in part 2. Add a word there and everything downstream
## (phrase map, dictionary, dummy, summary table, mip code) follows.
## ---------------------------------------------------------------------

## ---- packages --------------------------------------------------------
library(tidyverse)
library(haven)
library(magrittr)
library(quanteda)
library(labelled)     ## for val_labels() on the single-issue mip variable
library(crayon)
library(tictoc)
library(gtsummary)
library(stringi)

data("ces25")

## =====================================================================
## 1. TEXT PIPELINE
## =====================================================================
## Responses and dictionary terms go through the SAME cleaning, so a term
## can never be written in a form the responses can no longer contain.
##
## prepText()  accents -> ASCII, lower case, drop punctuation, squish
##             (keep_glob = TRUE keeps the * wildcards used in dictionary
##              terms; "[[:punct:]]" would otherwise delete them)

## FIX: punctuation is no longer all treated the same way. Apostrophes and
##      periods CLOSE UP, so "don't" -> dont, "l'environnement" ->
##      lenvironnement and "u.s." -> us. Every other separator becomes a
##      SPACE. Deleting a slash outright fused responses together:
##      "cost of living/taxes" became the single token costoflivingtaxes,
##      which no dictionary could match - it shows up three times in the
##      uncoded list.
prepText <- function(x, keep_glob = FALSE) {
  sep <- if (keep_glob) "[^[:alnum:][:space:]*]" else "[^[:alnum:][:space:]]"
  x %>%
    as.character() %>%
    tidyr::replace_na("") %>%
    stringi::stri_trans_general("Latin-ASCII") %>%   ## e -> e, o -> o
    stringr::str_to_lower() %>%
    stringr::str_remove_all("[’'`.]") %>%       ## elisions and u.s.
    stringr::str_replace_all(sep, " ") %>%
    stringr::str_squish()
}

## ---- multi-word phrases ----------------------------------------------
## quanteda tokenises on whitespace, so "cost of living" is three tokens
## and a three-word dictionary entry has to be matched as a sequence.
## Rather than trust that, we glue every multi-word phrase into a single
## token on BOTH sides: "cost of living" -> "costofliving" in the
## responses, and the identical rewrite is applied to the dictionary
## terms. Nothing can drift out of sync because the map is built FROM the
## dictionaries themselves.
##
## buildPhraseMap() takes every term that contains a space, strips the
## wildcards, and returns a named vector for str_replace_all().
## Longer phrases are applied first so that "first nations" is consumed
## before "first nation" can touch it.

buildPhraseMap <- function(term_list, extra = character()) {
  p <- unique(prepText(c(unlist(term_list, use.names = FALSE), extra)))
  p <- p[stringr::str_detect(p, " ")]
  p <- p[order(-stringr::str_count(p, " "), -nchar(p))]
  stats::setNames(stringr::str_remove_all(p, " "), p)
}

## FIX: the phrases are applied in ONE pass, as a single ordered
##      alternation, not as 300 successive str_replace_all() calls. Applied
##      one after another they cascade: "le cout de la vie" was being cut
##      to "lecout de la vie" by an earlier pattern before the longer
##      "cout de la vie" could match it. The trailing \\w* lets a phrase
##      absorb its own plural, so "student loans" -> "studentloans".
collapsePhrases <- function(x, map) {
  if (length(map) == 0) return(x)
  ## FIX: a phrase absorbs its own plural ("student loans" -> studentloans)
  ##      but ONLY -s / -es, and only up to a word boundary. It used to
  ##      absorb any trailing letters at all, which swallowed the next word
  ##      whenever punctuation had closed the gap: "US relationship" was
  ##      glued into usrelationship, which matched nothing.
  rx <- paste0("\\b(?:", paste(names(map), collapse = "|"), ")(?:es|s)?\\b")
  stringr::str_replace_all(x, rx, function(m) stringr::str_remove_all(m, " "))
}

## cleanText()  - for the survey responses
## cleanTerms() - for dictionary terms (keeps * ; warns about any phrase
##                that somehow did not make it into the phrase map)
## phrase_map is filled in properly in section 3; this empty default just
## means cleanText() still works (as a no-op on phrases) if it is called
## before then.
phrase_map <- character(0)

cleanText <- function(x) collapsePhrases(prepText(x), phrase_map)

cleanTerms <- function(terms) {
  out <- collapsePhrases(prepText(terms, keep_glob = TRUE), phrase_map)
  out <- unique(out[nzchar(out)])
  stray <- out[stringr::str_detect(out, " ")]
  if (length(stray)) {
    message(crayon::yellow(paste0(
      "  ! multi-word term(s) not in the phrase map: ",
      paste(stray, collapse = " | "))))
  }
  out
}

## ---- glob cheat sheet ------------------------------------------------
## quanteda matches dictionary terms against WHOLE TOKENS using globs:
##   "vote"      matches the token "vote" only
##   "vot*"      matches vote, voter, voting, votes
##   "*environ*" matches environment, lenvironnement, denvironnement
## The leading * matters in French: "l'environnement" cleans to
## "lenvironnement", one token, so "environ*" alone would miss it.
##
## Because matching is by token and not by substring, the short CES21
## patterns that were quietly matching inside other words are safe here:
## "ei", "65", "oas", "cpp", "ltc", "na" only fire when they are the whole
## token. The CES21 patterns that CANNOT be rescued ("ev" for EV, "19" for
## covid, "no" for a non-answer) are hashed out with a note where they
## would have gone.

## =====================================================================
## 2. THE VOCABULARY - one list, one place to edit
## =====================================================================
## Sources for every category: the CES25 dictionaries you already had,
## PLUS every pattern from the matching CES21 category in ces21_recode.R.
## Multi-word entries are fine anywhere - they are handled automatically.

mip_terms <- list(

  ## -- 1. Environment --------------------------------------------------
  enviro = c(
    ## stems. The leading * matters in French: "l'environnement" cleans to
    ## the single token "lenvironnement".
    "*envir*", "emviron*", "enviorn*", "enviourn*", "envion*", "envrion*",
    "eniron*", "environnemen*", "environnment", "enviroment*", "enviornment",
    ## climate
    "climat*", "*climatiq*", "climate change", "changement climatique",
    "changements climatiques", "changement*", "cl8mate change",
    "climet chang", "clkmatique", "warming", "global warming",
    "rechauffement*", "rechauffement climatique",
    ## ecology, pollution, planet
    "*ecolog*", "cologie", "lcologie", "ecology",
    "pollution", "polluer", "pollute", "polluting", "pollue*",
    "planet*", "planete*", "earth", "terre",
    ## emissions and carbon
    "carbon", "carbone", "carbon tax*", "co2", "ges", "gaz a effet de serre",
    "ev", "evs", "electric vehicle*", "vehicule electrique*",
    "greenhouse gas", "greenhouse gases", "emission*", "empreinte carbone",
    ## green, sustainability, nature
    "green", "greener", "verte", "vert", "sustainability", "sustainable",
    "developpement durable", "biodiversity", "biodiversite", "deforestation",
    "pesticide*", "recycling", "recyclage", "plastic*", "conservation",
    "nature", "wildlife", "oceans",
    ## water, air, weather
    "water", "drinking water", "eau potable", "lair", "air quality",
    "qualite de lair", "wildfire*", "feux de foret", "flood*", "inondation*",
    "secheresse"
    ## "ev" ## HASHED (CES21): as a substring it fired on every response
    ##      containing "ev" - even/never/level. Add "evs" if you want EVs.
    ## "61" ## HASHED (CES21): a stray code, not a word.
  ),

  ## -- 2. Crime --------------------------------------------------------
  crime = c(
    "crime*", "*crimin*", "criminalite", "delinquance", "*delinqu*",
    "police", "policing", "policier*", "rcmp", "grc",
    "gun*", "gun crime", "gun violence", "gun control", "firearm*", "fire arm*",
    "armes a feu", "arme a feu",
    "violence", "violent", "safe", "safety", "unsafe", "securitaire",
    "theft*", "car theft", "auto theft", "stealing", "robbery", "break ins",
    "murder*", "homicide*", "shooting*", "stabbing*",
    "gang*", "justice", "prison*", "jail", "*jail*", "incarceration",
    "bail", "bail reform", "catch and release", "sentencing", "repeat offenders",
    "protests", "vandalism", "vandalisme", "trafficking", "traficking",
    "fentanyl", "opioid*", "overdose*", "drug crime", "hard drugs",
    "law", "laws", "law and order", "victimes", "victims",
    ## CES21 files security / terrorism under crime as well as foreign
    "security", "securite", "securit*", "terroris*", "terrorisme"
  ),

  ## -- 3. Ethics and Government ----------------------------------------
  ethics = c(
    ## honesty, corruption, integrity
    "honest*", "honesty", "honnet*", "honntet*", "dishonest*", "dishonesty",
    "integrity", "integrite", "lintegrite", "integretity", "intgrit*",
    "corrupt*", "corupt*", "coruption", "ethic*", "ethical", "ethique*",
    "truth", "verite*", "lie", "lies", "lying", "lieing", "liar*",
    "menteur*", "mensonge*", "crook*", "hypocrisy", "hypocrite*",
    "moral*", "morality", "morals", "decency", "trust", "trustworthy",
    "mistrust", "distrust", "credibility", "credibilite", "greed",
    "scandal*", "scandale*", "promise*", "promesse*", "broken promises",
    "bad behavior", "in jail",
    ## fairness, accountability, representation
    "fair", "fairly", "fairness", "unfair", "equite", "accountability",
    "accountable", "accountibility", "responsibility", "responsible",
    "responsible government", "transparency", "transparence", "transparent",
    "represented",
    ## the "and Government" half
    "government", "governments", "*gouvernement*", "gouvernement*",
    "governance", "gouvernance", "bureaucracy", "bureaucratie", "red tape",
    "mismanagement", "incompetence", "incompetent", "waste of money"
    ## NOTE "rights" now lives in socio_cultural (CES21 files it there).
    ## NOTE "justice" is in crime. ces21_recode.R also lists it under
    ##      ethics - add it back here if you want the CES21 count exactly.
    ## NOTE "*integr*" is deliberately NOT used: it would fire on
    ##      "integration" (immigration) and on "integrite territoriale".
  ),

  ## -- 4. Education ----------------------------------------------------
  education = c(
    "educ", "educat*", "*education*", "educational", "ducation*", "leducation",
    "deducation", "school*", "ecole*", "schooling", "student*", "etudiant*",
    "*etudiant*", "university", "universities", "universite*", "college*",
    "cegep*", "tuition", "tuitition", "frais de scolarite", "scolarite",
    "student loan*", "student debt", "student funding", "student issue*",
    "loans and grants", "teacher*", "enseignant*", "literacy",
    "post secondary", "postsecondary", "curriculum", "class sizes"
  ),

  ## -- 5. Energy -------------------------------------------------------
  ## Split out of the environment dictionary so it matches CES21.
  energy = c(
    "energy", "*energi*", "energie*", "energetique*", "nergtiques",
    "pipeline*", "pipe", "oil", "oil and gas", "oil sands", "oilsands",
    "sables bitumineux", "petrol*", "petrole*", "gas", "gaz", "lng",
    "fossil*", "fossiles", "fuel*", "carburant*", "refinery", "refineries",
    "nuclear", "nucleaire", "hydro", "hydroelectric*", "electricity",
    "electricite", "power grid", "renewable*", "renouvelable*",
    "solar", "solaire", "wind power", "eolien*",
    "energy security", "energy prices", "keystone", "trans mountain"
  ),

  ## -- 6. Jobs ---------------------------------------------------------
  jobs = c(
    "job*", "*emplo*", "unemployment", "unemployed", "emploi*", "emploie*",
    "chomage", "*chomage*", "work", "works", "working", "worker*",
    "travail", "travailleur*", "career*", "carriere*",
    "salar*", "salaire*", "wage*", "minimum wage*", "salaire minimum",
    "income", "incomes", "low income", "revenu*", "labour", "labor",
    "labour shortage*", "main doeuvre", "doeuvre", "penurie de main doeuvre",
    "personnel", "staffing", "hiring", "layoffs", "job losses",
    "job security", "job creation", "creation demplois"
  ),

  ## -- 7. Economy ------------------------------------------------------
  ## Everything about jobs / taxes / debt / prices has been moved to its
  ## own category, the way CES21 splits them.
  economy = c(
    "*econom*", "*conomi*", "*conomiq*", "econ*", "ecomom*", "ecomon*",
    "econam*", "ecenom*", "ecnomic*", "econics", "evonomy", "enconomie",
    "exonom*",
    "financ*", "*financ*", "money", "argent", "*argent*",
    "wealth", "richesse*", "la richesse", "recovery", "economic recovery",
    "growth", "croissance", "industry", "industrie*", "manufacturing",
    "manufacturier", "business*", "small business*", "business concern*",
    "commerce*", "commerciale*", "entreprise*", "pme",
    "market*", "marche*", "stock market*", "recession", "*recession*",
    "productivity", "productivite", "gdp", "pib", "dollar*", "loonie",
    "interest rate*", "taux dinteret", "banks", "banques",
    "middle class", "classe moyenne", "the classes", "rich and the rest",
    "inequality", "inegalite*", "wealth gap",
    "infrastructure*", "developpement*", "investment*", "investissement*",
    "trade", "trading", "free trade", "libre echange", "supply chain*",
    "pay increase*", "raises", "big tech", "fighting big tech", "monopolies"
    ## "con" ## HASHED: it was a fragment of "economie", but on its own it
    ##       is a French insult and an abbreviation for "conservative".
    ## NOTE CES21 also codes a bare "$" here; punctuation is stripped, so a
    ##      response of "$" ends up empty and is counted as a non-answer.
  ),

  ## -- 8. Health and Health Care ---------------------------------------
  health = c(
    "*health*", "healthcare", "healtcare", "heathcare", "hralthcare",
    "heath*", "helath*", "heaalthcare", "heaith care", "heakth caew",
    "health care", "mental health", "sante mentale", "public health",
    "sant", "sant*", "sante*", "la sante", "soin*", "soins de sante",
    "medical*", "medecin*", "medicine", "medicament*", "medicare",
    "medicaid", "medicade", "pharma*", "pharmacare", "prescription*",
    "prescript*", "doctor*", "docteur*", "nurse*", "infirmier*",
    "hospital*", "hopital*", "hopitaux", "urgence*", "emergency room*",
    "wait times", "wait time", "waiting lists", "temps dattente",
    "long term care", "longterm care", "long term*", "soins de longue duree",
    "mental", "addiction*", "dependance", "opioid*", "opiod*", "overdose*",
    "drug*", "dental", "dentaire", "optometr*", "bill 124",
    "disability", "disabilities", "disabl*", "handicap*",
    "bien etre", "bienetre", "wellbeing", "well being",
    "quality of life", "qualite de vie", "palliative"
    ## "care" ## HASHED: on its own it also fires on "child care", "daycare",
    ##        "I don't care". The care phrases above are matched instead.
    ## "life" ## HASHED: fired on "life is expensive", "cost of life".
  ),

  ## -- 9. Taxes --------------------------------------------------------
  taxes = c(
    "tax", "taxes", "taxs", "taxe*", "taxed", "taxing", "taxation", "tqxes",
    "*impot*", "impts", "impt", "income tax*", "carbon tax*", "taxe carbone",
    "capital gains", "gst", "hst", "tps", "tvq", "sales tax*", "property tax*",
    "tax cuts", "tax breaks", "baisse dimpot", "fiscalite",
    "taxpayer*", "contribuable*", "overtaxed"
    ## NOTE "tax*" is not used because it also matches "taxi".
  ),

  ## -- 10. Debt and Deficit (incl. government spending) ----------------
  debt = c(
    "debt", "debts", "dept", "debit", "national debt", "public debt",
    "dette*", "*dette*", "la dette", "dette publique", "endettement",
    "lendettement", "deficit*", "defic*", "dficit*", "defec*",
    "budget*", "budgetaire*", "budgtaire", "budjet*", "bugdet*", "buget",
    "balanced budget*", "balanced", "surplus", "fiscal*",
    "fiscal responsibility", "austerity", "austerite",
    "spending", "spend*", "overspending", "government spending",
    "government waste", "wasteful spending", "gaspillage",
    "depense*", "dpense*", "depenses publiques", "cost cutting", "cuts"
  ),

  ## -- 11. Democracy (electoral reform, elections, institutions) -------
  democracy = c(
    "*democr*", "demovracy", "demo0cr5atic", "democratie*",
    "elect", "elected", "electing", "election*", "*election*", "electoral*",
    "electoral reform", "reforme electorale", "lection*",
    "vote", "vote*", "*vote*", "voting", "voter*", "ballot*", "scrutin",
    "referendum*", "constitution*", "charter", "charte",
    "parliament*", "parlement*", "senate", "senat", "house of commons",
    "proportional", "proportionnelle", "first past the post",
    "representation", "gerrymander*", "election interference",
    "voter turnout", "minority government*", "majority government*",
    "coalition", "term limits"
    ## "elect*" ## deliberately NOT used - it also matches electricity.
    ## "vot*"   ## deliberately NOT used - it also matches French "votre".
    ## "federal" ## CES21 files it here; in CES25 it is in brokerage.
  ),

  ## -- 12. Foreign Affairs (replaces the old "security" category) ------
  foreign = c(
    "war", "wars", "peace", "paix", "guerre*", "ukraine", "russia", "russie",
    "chin*", "chinese", "israel", "israeli", "gaza", "palestin*",
    "middle east", "moyen orient", "iran", "north korea",
    "foreign", "foreign policy", "foreign affairs", "affaires etrangeres",
    "etranger*", "international*", "relations", "relations internationales",
    "global", "geopolitic*", "geopolitiques", "diplomacy", "diplomatie",
    "defence", "defense", "defens*", "military", "militaire*",
    "armed forces", "armee*", "troops", "nato", "otan",
    "united nations", "onu", "weapons", "armes", "nuclear weapons",
    "security", "national security", "securite", "securite nationale",
    "segurity", "scurit", "terroris*", "terrorisme",
    "interference", "ingerence", "foreign interference", "espionage",
    "foreign aid", "aide internationale", "travel restriction*"
    ## NOTE CES21 files "jew*" here; antisemitism sits in socio_cultural.
    ## NOTE "border" and "usa" are CES21 foreign-affairs terms. In CES25
    ##      they belong to the borders category - see 23 below.
  ),

  ## -- 13. Immigration -------------------------------------------------
  immigration = c(
    "*immigr*", "*imigr*", "*migrat*", "*migrant*", "inmigr*", "imagrat*",
    "immagr*", "immegrant*", "immgration", "emigration",
    "refugee*", "refugie*", "refudgee", "asylum", "asile",
    "demandeurs dasile", "visa*", "sponsor", "newcomers",
    "nouveaux arrivants", "illegal immigration", "illegals", "illegale*",
    "illgale", "clandestin*", "third world people",
    "temporary foreign workers", "tfw", "international students",
    "immigration levels", "population growth", "overpopulation"
    ## NOTE "minority" and "discrimination" moved to socio_cultural,
    ##      "foreign" to foreign affairs, "frontiere" to borders.
  ),

  ## -- 14. Socio-Cultural ----------------------------------------------
  ## women's issues + abortion + race + indigenous + rights + LGBTQ,
  ## following the CES21 socio_cultural category.
  socio_cultural = c(
    ## rights and freedoms
    ## NOTE not "droit*" - that also matches "droite", the political right
    "right", "rights", "human rights", "charter rights", "droit", "droits",
    "droits humains", "freedom*", "libert*", "liberte dexpression",
    "free speech", "censorship", "censure",
    "equality", "equal*", "egalite*", "equity", "inclusiv*", "inclusion",
    "diversity", "divers*", "accessibility", "accommodation",
    ## gender, sexuality, abortion
    "gay", "gay rights", "lgbt*", "2slgbtq*", "queer", "homosexual*",
    "trans", "transgender*", "trans rights", "gender*", "gender identity",
    "identite de genre", "sexual orientation",
    "abort*", "avortement*", "pro life", "prolife", "pro choice", "unborn",
    "reproductive*", "anatiabortion",
    "women", "womens", "woman", "womans", "femme*", "feminis*", "feminine",
    "maternity", "maternite", "childbirth",
    ## race, religion, minorities
    "race", "races", "racis*", "raciste*", "black", "white",
    "minority", "minorit*", "visible minorities", "discrimination",
    "prejudice", "hate", "hate crime*", "haine",
    "antisemitism", "anti semitism", "antisemitisme", "jewish", "jew",
    "islamophobia", "islamophoby", "islamaphobia", "islamophobie", "muslim*",
    "religion", "religious freedom",
    ## indigenous
    "indigenous", "indig*", "*indigen*", "indeginous", "aboriginal", "native",
    "autochtone*", "first nations", "first nation", "reconciliation",
    "reconcil*", "treaty", "treaties", "trc", "residential school*",
    "land agreement*", "land claims", "clean drinking water",
    ## culture, values, information
    "culture", "cultur*", "values", "valeurs", "identity", "identite",
    "canadian identity", "woke", "wokeism", "cancel culture",
    "political correctness", "media", "medias", "social media",
    "fake news", "fake information", "false information", "misinformation",
    "desinformation", "disinformation", "credible information",
    "arts", "animal*", "youth issue*", "family issue*", "family values",
    ## firearms (CES21 files them here; they are also in crime)
    "gun*", "firearm*", "weapons ownership", "gun control", "gun violence",
    "arme a feu", "armes a feu",
    "c21", "c71", "c6"
  ),

  ## -- 15. Social Programs (welfare + seniors) -------------------------
  social = c(
    ## welfare and transfers
    "welfare", "aide sociale", "social aid", "programs sociale",
    "programmes sociaux", "social", "sociale*", "sociaux", "social programs",
    "social services", "services sociaux", "social assistance",
    "assistance", "assistance sociale",
    "safety net", "redistribution", "universal basic income", "basic income",
    "universal basic", "revenu de base", "ubi", "guaranteed income",
    "benefits", "prestations", "funding", "funds", "fonds", "soutien",
    "family support", "public services", "services publics",
    ## children and families
    "child", "children", "childrens", "child care", "childcare", "daycare",
    "day care", "garderie*", "garde denfants", "enfant*", "famille*",
    "familiale*", "family", "families", "parental", "parental leave",
    "conge parental", "ccb", "allocation familiale", "autis*", "youth",
    ## poverty
    "poverty", "poor", "pauvre*", "pauvret*", "impoverish*",
    "under privileged", "underprivileged", "food bank*",
    "banques alimentaires", "hunger", "food insecurity",
    ## seniors and retirement
    "senior*", "senoir*", "seanor", "aine*", "ainee*", "elderly", "elder*",
    "old", "older", "old people", "personnes agees", "agee*", "age", "ages",
    "aging", "aging population", "vieillisse*", "vieillissement",
    "vieillesse", "viellesse", "vielliesse", "veillesse", "vieux",
    "retirement", "retraite*", "retired", "retirees", "retir*",
    "pension*", "pesion", "cpp", "oas", "old age security", "rrq", "gis",
    "65", "60 ans", "long term care", "longterm care", "ltc", "chsld",
    "nursing homes", "care homes", "retirement homes",
    ## disability, veterans, other
    "disability", "disabl*", "handicap*", "odsp", "ei", "wsib",
    "veteran*", "anciens combattants", "sick leave", "reserve*",
    "la population"
    ## NOTE cerb / pcu / pcre are dropped with the covid category.
    ## NOTE these short tokens ("ei", "65", "oas", "ltc", "cpp") are safe
    ##      here because quanteda matches whole tokens, not substrings.
  ),

  ## -- 16. Brokerage (Quebec, federal-provincial) ----------------------
  brokerage = c(
    "*quebec*", "qubec", "qiebec", "quebecois*", "qc",
    "province*", "provincial*", "provinciale*", "provinciaux",
    "province jurisdiction", "jurisdiction*", "juridiction*",
    "federal*", "federalis*", "fed prov", "ottawa",
    "national unity", "unity", "unite", "unite nationale",
    "separatism", "separatiste*", "separation", "secession", "souverainisme",
    "quebec sovereignty", "souverainete du quebec", "quebec separation",
    "distinct society",
    "francophone*", "franco*", "francais*", "french", "langue*", "language*",
    "bilingual*", "bilinguisme",
    "loi 21", "bill 21", "loi 96", "bill 96", "laicite", "laicit", "lacit",
    "secularism", "religious symbols",
    "alberta", "western alienation", "equalization", "perequation",
    "interprovincial"
    ## "21"  ## HASHED: on its own it also matched ages, dates and codes.
    ##       "loi 21" / "bill 21" are matched as phrases instead.
    ## "ubi" ## moved to social programs (CES21 had it here by accident).
    ## NOTE "constitution" is in democracy, not here.
  ),

  ## -- 18. Inflation (cost of living) ----------------------------------
  inflation = c(
    "*inflation*", "cost of living", "cost", "costs", "cout*", "couts",
    "lecout", "cout de la vie", "prix", "price", "prices",
    "pricing", "afford*", "aford*", "affort*", "afgord*", "afforability",
    "unaffordable", "expensive",
    "cher", "trop cher", "living", "standard of living", "niveau de vie",
    "purchasing power", "pouvoir dachat", "grocer*", "groceries",
    "food", "food prices", "epicerie*", "panier depicerie", "nourriture",
    "gas prices", "prix de lessence", "essence", "base rate",
    "phone plan*", "utility bills", "bills", "wage*", "salaire*",
    "cost of everything"
    ## NOTE CES21 puts "wage*" in inflation AND in jobs; kept in both.
  ),

  ## -- 19. Housing -----------------------------------------------------
  housing = c(
    "*housing*", "housing crisis", "affordable housing",
    "*logement*", "crise du logement",
    "rent", "rents", "rental*", "renting", "renters", "loyer*", "rent control",
    "home", "homes", "house", "houses", "maison*", "hpmes", "dwelling*",
    "condo*", "mortgage*", "hypotheque*", "propriet*", "proprio*",
    "landlord*", "eviction*", "homeless*", "sans abri*", "sansabri*",
    "itinerance", "shelter*", "abri*", "hebergement", "first time buyers",
    "zoning", "inflationhousingrenting", "unaffordable", "affordable"
    ## NOTE "propri*" (CES21) also matched "propre"; narrowed to "propriet*".
  ),

  ## -- 21. Trump -------------------------------------------------------
  trump = c(
    "trump*", "turmp", "dtrump", "bufoontrump", "usatrump", "donald",
    "president*", "maga"
  ),

  ## -- 22. Tariffs -----------------------------------------------------
  tariff = c(
    "*tarif*", "tariff*", "tarrif*", "terrif*", "thariff", "taxestariffs",
    "economytariffs", "trade war*", "guerre commerciale", "trade dispute*",
    "big beautiful bill", "protectionism", "protectionnisme",
    "buy canadian", "counter tariffs", "droits de douane"
    ## "economist" / "economiste" ## HASHED: economy words that had been
    ## pasted into the tariff dictionary and were inflating the count.
  ),

  ## -- 23. Borders / US relations --------------------------------------
  borders = c(
    "border*", "frontiere*", "*frontier*", "annex*", "annexion",
    ## sovereignty is spelled a dozen different ways in the responses
    "sove*", "sover*", "sovr*", "sovre*", "souver*", "soveir*",
    "51st", "51st state", "51e etat",
    "us", "usa", "america", "american*", "americain*", "states",
    "the states", "etats unis", "etat unis", "etatsunis", "etatunis", "etasunis",
    "united states", "canada us relations", "us relations",
    "*independ*", "*independan*",
    "autonomie", "autonomy", "integrite territoriale",
    "lintegrite territoriale", "canadian identity", "keeping canada",
    "neighbours", "neighbors", "trump"
    ## NOTE "us" also matches the English pronoun. It is kept because in
    ## short MIP answers it is nearly always "U.S."; drop it if you would
    ## rather be conservative - "usa"/"america*"/"states" still fire.
  ),

  ## -- 24. Leaders -----------------------------------------------------
  leaders = c(
    "carney", "carnay", "mark", "marc", "poilievre", "poliviere", "pierre",
    "singh", "jagmeet", "blanchet", "trudeau*", "justin", "scheer", "sheer",
    "otoole", "toole", "bernier", "ford", "may", "paul", "andrew",
    "libera*", "libral", "liberaux",
    "conservative*", "conservateur*", "conservatrice*", "tory", "tories",
    "ndp", "npd", "bloc", "green party", "parti vert",
    "leader*", "leadership", "prime minister*", "premier ministre", "pm",
    "politician*", "politicien*", "parties", "party", "partis",
    "candidate*", "candidat*"
    ## NOTE "green" on its own stays in the environment dictionary;
    ##      "green party" is matched as a phrase here.
  ),

  ## -- Non-answers -----------------------------------------------------
  idk = c(
    "99", "unsure", "not sure", "dont know", "don t know", "do not know",
    "dunno", "dnk", "d k", "idk", "no idea", "no clue", "no comment",
    "no opinion", "no issue*", "no interest", "not interested",
    "undecided", "havent decided", "hard to pick", "not applicable",
    "dont care", "don t care", "dont have one", "dont have",
    "doesnt matter", "does it really matter", "no point", "wont matter",
    "dont see the point", "no strong feeling", "no choice", "no matters",
    "no particular issue", "i do not have one", "dont knoe", "dont mnow",
    "prefer not", "prefer not to say", "not a citizen", "neutral", "neutre",
    "nothing", "none", "nil", "na", "nan", "rien", "aucun", "aucune",
    "je ne sais pas", "je sais pas", "sais pas", "ne sait pas", "je c po",
    "jai pas de reponse", "pas de reponse", "je men fou", "sans opinion",
    "unknown", "uninterested"
    ## "no"  ## HASHED: it fired on "no jobs", "no housing", "no doctors".
    ## "pas" ## HASHED (CES21): it fired on every French negation.
    ## Junk strings ("xxx", "asdbf", keyboard mash) are handled by the
    ## exact-match list further down, not here.
  )
)

## =====================================================================
## 3. BUILD THE PHRASE MAP, CLEAN THE RESPONSES, BUILD THE DICTIONARIES
## =====================================================================

## Phrases that have to be glued together even though no category codes
## them. Without this, "far right" hands the loose token "right" to
## socio-cultural and a response about ideology gets counted as a rights
## response. CES21 leaves ideology uncoded (its "rights" pattern never
## matched "far right"), so these stay uncoded here too - move any of them
## into a category below if you would rather they counted somewhere.
ideology_phrases <- c("far right", "far left", "right wing", "left wing",
                      "alt right", "extreme right", "extreme left",
                      "right leaning", "left leaning",
                      "la droite", "la gauche", "extreme droite",
                      "extreme gauche")

## Every multi-word entry above, plus these, becomes a single token.
phrase_map <- buildPhraseMap(mip_terms, extra = ideology_phrases)
message(crayon::silver(sprintf("phrase map: %d multi-word patterns collapsed",
                               length(phrase_map))))

## The raw response column is left alone. CES21 calls the cleaned column
## mip_lower, so CES25 does too.
## FIX: the old script overwrote ces25$cps25_imp_iss in place, so re-running
##      any part of it cleaned already-cleaned text.
ces25$mip_lower <- cleanText(ces25$cps25_imp_iss)

## FIX (kept from the previous clean-up): the old script had a line
##   sub("^(\\w+)\\s+(\\w+)$", "\\2 \\1", ...)
## that reversed the word order of every two-word response
## ("climate change" -> "change climate"). It is gone for good.

## one quanteda dictionary per category
mip_dicts <- lapply(names(mip_terms), function(k) {
  quanteda::dictionary(stats::setNames(list(cleanTerms(mip_terms[[k]])), k))
})
names(mip_dicts) <- names(mip_terms)

## Trump + tariffs + borders, built as the union of its parts so it can
## never drift away from them.
mip_dicts$combined <- quanteda::dictionary(list(combined = unique(c(
  cleanTerms(mip_terms$trump),
  cleanTerms(mip_terms$tariff),
  cleanTerms(mip_terms$borders)))))

## the CES21 numbering, so the two waves line up.
## 17 (Free Trade) and 20 (COVID) are deliberately left empty.
mip_codes <- c(enviro = 1, crime = 2, ethics = 3, education = 4, energy = 5,
               jobs = 6, economy = 7, health = 8, taxes = 9, debt = 10,
               democracy = 11, foreign = 12, immigration = 13,
               socio_cultural = 14, social = 15, brokerage = 16,
               inflation = 18, housing = 19,
               trump = 21, tariff = 22, borders = 23, leaders = 24)

issue_cols <- names(mip_codes)   ## the substantive categories

## =====================================================================
## 4. THE DICTIONARY RUNNER
## =====================================================================

## the lookup itself: text in, one count per response out
lookupCounts <- function(txt, dictionaryA) {
  key  <- names(dictionaryA)[1]
  txt  <- tidyr::replace_na(as.character(txt), "")
  toks <- quanteda::tokens(txt)
  dfmA <- quanteda::dfm(
    quanteda::tokens_lookup(toks, dictionaryA,
                            valuetype = "glob",     ## FIX: stated, not assumed
                            nested_scope = "dictionary"))
  ## FIX: guarantees the key column exists even when nothing matches.
  ##      Previously a zero-match dictionary returned a data.frame with no
  ##      such column, so dataB$key was NULL and the assignment failed.
  dfmA <- quanteda::dfm_match(dfmA, features = key)
  as.integer(quanteda::convert(dfmA, to = "data.frame")[[key]])
}

## same call signature as the old runDictionary(): data, column, dictionary
runDictionary <- function(
    dataA,          # input data
    word,           # column to be searched
    dictionaryA) {  # dictionary of terms to search for
  tictoc::tic()
  key <- names(dictionaryA)[1]
  ## FIX: pull() instead of mutate(word = {{word}}) - the old version
  ##      overwrote any existing column called "word" and choked on
  ##      haven_labelled columns
  hits <- lookupCounts(dataA %>% dplyr::pull({{ word }}), dictionaryA)
  message(crayon::green(sprintf(
    "%-15s %5d of %d responses matched (%.1f%%)",
    key, sum(hits >= 1), length(hits), 100 * mean(hits >= 1))))
  tictoc::toc()
  stats::setNames(data.frame(seq_along(hits), hits), c("doc_id", key))
}

## adds both the count (enviro) and the dummy (enviro.dum).
## `word` here is the column NAME as a string, so nothing has to be
## forwarded through tidy evaluation inside the loop below.
codeCategory <- function(dataA, word, dictionaryA) {
  key  <- names(dictionaryA)[1]
  hits <- lookupCounts(dataA[[word]], dictionaryA)
  dataA[[key]]                  <- hits
  dataA[[paste0(key, ".dum")]]  <- as.integer(hits >= 1)
  message(crayon::green(sprintf(
    "%-15s %5d of %d responses matched (%.1f%%)",
    key, sum(hits >= 1), length(hits), 100 * mean(hits >= 1))))
  dataA
}

## =====================================================================
## 5. CODE EVERY CATEGORY
## =====================================================================
for (k in names(mip_dicts)) {
  ces25 <- codeCategory(ces25, "mip_lower", mip_dicts[[k]])
}

## ---- how many issues did each respondent get coded into? -------------
## combined is a union of trump/tariff/borders and idk is not an issue,
## so neither counts towards the total.
ces25$mip_total <- rowSums(as.data.frame(ces25[paste0(issue_cols, ".dum")]))

## ---- non-answers -----------------------------------------------------
## The CES21 script also lists dozens of exact junk strings; the ones that
## survive cleaning are here, plus anything that cleans to nothing at all
## ("?", "...", "$", "-") and any one- or two-character response that no
## dictionary caught.
mip_junk <- cleanText(c(
  "", "?", "??", "???", ".", "...", ".....", "-", "$", "0", "1", "no", "non",
  "oui", "yes", "yup", "n", "x", "xxx", "y", "u", "f", "g", "j", "as", "the",
  "hm", "ish", "nul", "nth", "no ne", "nope", "nothinh", "nothibg", "cul",
  "dnk", "d/k", "n/a", "na", "unknown", "netural", "very nice", "ha!",
  "good", "bad", "blah", "click", "clicks", "asdasd", "all", "everything",
  "bye felicia", "fuck off", "i'm neutral", "je c po", "je men fou"))

ces25 <- ces25 %>%
  mutate(idk.dum = as.integer(
    idk >= 1 |
      mip_lower %in% mip_junk |
      (mip_total == 0 & nchar(mip_lower) <= 2)))

message(crayon::green(sprintf(
  "non-answers: %d of %d (%.1f%%)",
  sum(ces25$idk.dum), nrow(ces25), 100 * mean(ces25$idk.dum))))

## =====================================================================
## 6. WHAT IS STILL UNCODED
## =====================================================================
## Run this, look at the top of the list, add the words you see to the
## right category in mip_terms, re-run. This is the CES21 workflow.
ces25 %>%
  filter(mip_total < 1, idk.dum < 1) %>%
  count(mip_lower, sort = TRUE) %>%
  print(n = 200)

ces25 %>%
  filter(mip_total < 1, idk.dum < 1) %>%
  count(mip_lower, sort = TRUE) %>%
  write_csv("2025_mip_uncoded.csv")

## =====================================================================
## 7. SUMMARY OF THE RESULTS
## =====================================================================
## Percentages add to well over 100: a response can be coded into more
## than one category, exactly as in CES21.
ces25 %>%
  tbl_summary(
    include = c(economy.dum, inflation.dum, housing.dum, health.dum,
                jobs.dum, taxes.dum, debt.dum, enviro.dum, energy.dum,
                immigration.dum, crime.dum, ethics.dum, education.dum,
                democracy.dum, foreign.dum, socio_cultural.dum, social.dum,
                brokerage.dum, leaders.dum, combined.dum, trump.dum,
                tariff.dum, borders.dum, idk.dum),
    label = list(
      economy.dum        ~ "The Economy",
      inflation.dum      ~ "Inflation and Cost of Living",
      housing.dum        ~ "Housing",
      health.dum         ~ "Health and Health Care",
      jobs.dum           ~ "Jobs",
      taxes.dum          ~ "Taxes",
      debt.dum           ~ "Debt and Deficit",
      enviro.dum         ~ "The Environment",
      energy.dum         ~ "Energy",
      immigration.dum    ~ "Immigration",
      crime.dum          ~ "Crime",
      ethics.dum         ~ "Ethics and Government",
      education.dum      ~ "Education",
      democracy.dum      ~ "Democracy and Electoral Reform",
      foreign.dum        ~ "Foreign Affairs",
      socio_cultural.dum ~ "Socio-Cultural Issues",
      social.dum         ~ "Social Programs",
      brokerage.dum      ~ "Brokerage (Quebec, Fed-Prov)",
      leaders.dum        ~ "Party Leaders",
      combined.dum       ~ "US Relations, Trump and Tariffs",
      trump.dum          ~ "Trump",
      tariff.dum         ~ "Tariffs",
      borders.dum        ~ "US Relations",
      idk.dum            ~ "Don't know / did not answer"))

## =====================================================================
## 8. SINGLE-ISSUE VARIABLE (the CES21 mip / mip2 construction)
## =====================================================================
## Respondents coded into exactly one category get that category; anyone
## coded into two or more is NA, as in ces21_recode.R.
dum_mat <- as.matrix(as.data.frame(ces25[paste0(issue_cols, ".dum")]))

ces25$mip_single <- as.integer(ces25$mip_total == 1)

which_one <- apply(dum_mat, 1, function(r) {
  w <- which(r == 1)
  if (length(w) == 1) w else NA_integer_
})

ces25$mip2 <- issue_cols[which_one]
ces25$mip  <- unname(mip_codes[ces25$mip2])

## coded into nothing and not a refusal -> Other
ces25$mip[ces25$mip_total == 0 & ces25$idk.dum == 0] <- 0

## the CES21 label set, minus COVID, plus the 2025 additions
val_labels(ces25$mip) <- c(
  Other = 0, Environment = 1, Crime = 2, Ethics = 3, Education = 4,
  Energy = 5, Jobs = 6, Economy = 7, Health = 8, Taxes = 9,
  Deficit_Debt = 10, Democracy = 11, Foreign_Affairs = 12, Immigration = 13,
  Socio_Cultural = 14, Social_Programs = 15, Brokerage = 16, Free_Trade = 17,
  Inflation = 18, Housing = 19, COVID19 = 20,
  Trump = 21, Tariffs = 22, US_Relations = 23, Leaders = 24)

table(ces25$mip2)
table(as_factor(ces25$mip))
table(ces25$mip_total)

## =====================================================================
## 9. SELF TEST - confirms the pipeline still does what it says
## =====================================================================
## Run mip_selftest() after editing mip_terms. Every probe is a response
## that must land in the category named beside it; the multi-word probes
## are the ones that check the phrase map.
mip_selftest <- function() {
  probes <- c(
    "cost of living"        = "inflation",
    "Le coût de la vie" = "inflation",
    "climate change"        = "enviro",
    "l'environnement"       = "enviro",
    "pipelines and oil"     = "energy",
    "not enough jobs"       = "jobs",
    "the economy"           = "economy",
    "l'économie"       = "economy",
    "health care"           = "health",
    "santé"            = "health",
    "taxes are too high"    = "taxes",
    "government spending"   = "debt",
    "electoral reform"      = "democracy",
    "first past the post"   = "democracy",
    "the war in Ukraine"    = "foreign",
    "national security"     = "foreign",
    "immigration"           = "immigration",
    "gay rights"            = "socio_cultural",
    "abortion"              = "socio_cultural",
    "First Nations"         = "socio_cultural",
    "women's issues"        = "socio_cultural",
    "basic income"          = "social",
    "old people"            = "social",
    "Québec"           = "brokerage",
    "loi 21"                = "brokerage",
    "affordable housing"    = "housing",
    "Trump"                 = "trump",
    "tariffs"               = "tariff",
    "the 51st state"        = "borders",
    "Carney"                = "leaders",
    "je ne sais pas"        = "idk",
    "don't know"            = "idk",
    ## regression probes - each of these was a real bug at some point
    "freedom"               = "socio_cultural",
    "la liberte"            = "socio_cultural",
    "cost of living/taxes"  = "inflation",
    "U.S. tariffs"          = "tariff",
    "Canada-US relationship" = "borders",
    "canadian sovereignity" = "borders")

  txt <- cleanText(names(probes))
  hit <- vapply(seq_along(probes), function(i) {
    lookupCounts(txt[i], mip_dicts[[unname(probes[i])]]) >= 1
  }, logical(1))

  out <- data.frame(response = names(probes), expected = unname(probes),
                    matched = hit, row.names = NULL)
  print(out, row.names = FALSE)
  if (all(hit)) {
    message(crayon::green(sprintf("all %d probes matched", length(hit))))
  } else {
    message(crayon::red(paste("FAILED:",
                              paste(names(probes)[!hit], collapse = " | "))))
  }
  invisible(out)
}

mip_selftest()
