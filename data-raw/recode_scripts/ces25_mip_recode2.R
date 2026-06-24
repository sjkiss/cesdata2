################################################################################
## CES 2025 — "Most Important Problem" recode (CORRECTED)
##
## 1. TEXT CLEANING REWRITTEN.
##    - The old `sub("ˆ(\\w+)\\s+(\\w+)$", ...)` used a MODIFIER CIRCUMFLEX (ˆ),
##      not the regex anchor ^, so it never fired. Removed.
##    - The old blanket punctuation strip turned every apostrophe term into junk:
##      "don't know", "l'économie", "d'impot" could NEVER match because the text
##      had its apostrophes removed but the dictionaries still contained them.
##      French + contraction terms were silently dead. Fixed by cleaning the TEXT
##      and the DICTIONARY TERMS with the same function (clean_str).
##    - U.S./U.S.A. (caps) are normalized to the token "usa" before lowercasing.
##
## 2. US / "us" DISAMBIGUATION — DECIDED BY VALIDATION, NOT ASSUMPTION.
##    The original counted bare "us" as the country. First instinct was to drop
##    it as the English pronoun ("hurting us"). But pulling the 323 flagged
##    strings showed ~90% are GENUINE country references ("us relations", "us
##    trade", "the us", "canada us relations", French "les us"). So "us" is KEPT
##    as a country token, and clean_str() instead strips only the clear
##    personal-pronoun verb contexts (protect us, lie to us, benefit us, ...).
##    Country-topical "relationship with us / trade with us" are NOT stripped.
##    Residual uncaught pronouns (~3-5%) are a documented limitation.
##
##    OTHER FALSE-POSITIVE TERMS PRUNED:
##    - leaders: "may","mark"/"marc","green","ford","paul" (verb/names/colour/
##      premier). Costs Elizabeth May & Ford mentions — noted tradeoff.
##    - enviro: bare "change" ("we need change").
##    - healthcare: "care","life" ("I don't care","quality of life").
##    - economy & tariff: "economist"/"économiste" (duplicated, not a tariff term).
##    - idk: bare "no","nothing" (inflated non-response).
##    - welfare: bare "public" ("public transit/sector").
##    - quebec: bare "21" (matched ages/addresses; kept laicite/francophone).
##    - security: "u.s." moved out so the US cluster isn't double-counted across
##      security AND borders/combined.
##
## 3. DEDUPLICATION. The economy list (and others) had many repeated tokens.
##    clean_str(... ) + unique() removes them.
##
## 4. OVERLAP IS DOCUMENTED, NOT HIDDEN. Every category is a NON-EXCLUSIVE
##    binary "did this respondent MENTION X". They will not sum to 100%.
##    `combined` (US Relations + Trump + Tariffs) is a SUPERSET of its parts and
##    is built programmatically from the component vectors so it can't drift.
##    Report EITHER `combined` OR the components — never both as if additive.
##
## 5. VALIDATION + SENSITIVITY CHECK added at the bottom.
##
## NOTE: terms flagged "REVIEW" below are judgment calls kept in but worth
## eyeballing with validate_category().
################################################################################

## ---- Packages ---------------------------------------------------------------
library(labelled)
library(haven)
library(tidyverse)
library(magrittr)
library(quanteda)
library(gtsummary)

## ---- Data -------------------------------------------------------------------
data("ces25")
# data("ces21")  # uncomment for the 2021 comparison
# table(as_factor(ces21$mip))

## ---- Shared cleaning function ----------------------------------------------
## Applied to BOTH the survey text and the dictionary terms so they match.
##   - normalize U.S./U.S.A. (caps) to "usa"
##   - lowercase
##   - strip clear personal-pronoun uses of "us" (keep country uses — see note 2)
##   - strip remaining punctuation (apostrophes included -> "dont", "lenvironnement")
##   - collapse whitespace
clean_str <- function(x) {
  ## United States abbreviation -> token "usa" (BEFORE lowercasing; caps is a
  ## strong country signal). Lowercase "u.s." falls through and becomes "us"
  ## after punctuation stripping, which we also count as the country (below).
  x <- gsub("\\bU\\.?S\\.?A\\.?\\b", " usa ", x)   # USA / U.S.A.
  x <- gsub("\\bU\\.?S\\.?\\b",      " usa ", x)   # US  / U.S.  (not USE/USB/USD)

  x <- tolower(x)

  ## VALIDATION FINDING: bare lowercase "us" is ~90% genuine country reference
  ## ("us relations", "us trade", "the us", "canada us relations", French "les
  ## us"). So "us" is COUNTED as the country (see borders_terms). We strip only
  ## the clear PERSONAL-PRONOUN uses, where "us" is the object of a verb meaning
  ## "we/our" and carries no country meaning. List derived from the 323 strings.
  pron_verbs <- paste0(
    "\\b(led|lead|leads|leading|make|makes|making|made|protect|protects|",
    "protecting|protected|help|helps|helping|helped|fund|funds|funded|funding|",
    "put|puts|putting|give|gives|giving|gave|tell|tells|telling|told|teach|",
    "teaches|show|shows|cost|costs|owe|owes|benefit|benefits|benefiting|want|",
    "wants|wanted|join|joins|joining|forces) us\\b")
  x <- gsub(pron_verbs, "\\1", x)                  # drop pronoun "us", keep verb
  x <- gsub(paste0("\\b(care of|care for|caring for|lie to|lies to|lying to|",
                   "lied to|vetted for|forces upon|upon|above|than|like) us\\b"),
            "\\1", x)
  x <- gsub("\\bus et coutumes\\b", "coutumes", x) # French "us" = custom, not USA

  ## Any remaining "us" is treated as the country. Residual uncaught pronouns
  ## (~3-5%) are an accepted, documented limitation. NOTE: country-topical
  ## phrases like "relationship with us" / "trade with us" are deliberately
  ## NOT stripped — there "us" means the United States.

  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

## Clean a vector of dictionary terms the same way; drop blanks/dupes.
clean_terms <- function(terms) {
  t <- clean_str(terms)
  unique(t[nzchar(t)])
}

## Clean the open-ended column once into a new variable.
ces25$imp_clean <- clean_str(ces25$cps25_imp_iss)

## ---- runDictionary (cosmetic progress bar removed for speed) ----------------
runDictionary <- function(dataA, word, dictionaryA) {
  dataA   <- dataA %>% mutate(.word = {{ word }})
  corpusA <- tokens(dataA$.word)
  dfmA    <- dfm(tokens_lookup(corpusA, dictionaryA, nested_scope = "dictionary"))
  convert(dfmA, to = "data.frame")
}

## Helper: run a cleaned term vector against imp_clean and return the dummy.
make_dummy <- function(data, terms, label) {
  dict <- dictionary(setNames(list(clean_terms(terms)), label))
  res  <- runDictionary(data, imp_clean, dict)
  as.integer(res[[label]] >= 1)
}

################################################################################
## CATEGORY TERM VECTORS  (pruned; written in raw form, cleaned at build time)
################################################################################

economy_terms <- c(
  "economy","economic","economics","economie","economique","economiques",
  "leconomie","jobs","job","employment","emploi","emploie","unemployment",
  "unemployed","work","wage","wages","income","low income","middle class",
  "tax","taxes","taxs","taxe","taxed","taxing","taxation","impot","impots",
  "dimpot","finances","finance","financial","financiere","budget","budgets",
  "budgetaire","deficit","deficits","dficit","dficits","debt","dette",
  "lendettement","balanced","dollars","argent","money","spending","depenses",
  "dpense","trade","trading","market","recession","growth","loans","cost",
  "cost of living","cost of","rising cost","living","prices","prix","inflation",
  "poor","inequality","redistribution","affordability","afford","fiscal",
  "industry","industrie","commerciale","expensive","cout","investment",
  "investments","investissements"
  # REMOVED: "economist"/"économiste" (duplicated in tariff, not an issue term),
  #          "con" (matches too much).  "work" kept but REVIEW.
)

enviro_terms <- c(
  "climate","climat","climate change","climatique","climatiques","carbon",
  "carbone","co2","ges","warming","rechauffement","environment","environmental",
  "environnement","environnemental","environnementaux","ecology","ecologie",
  "pollution","pollute","polluer","green","greener","planet","earth","water",
  "energy","oil","gas","fossil","fossiles","pipeline","pipelines","pipe",
  "sustainability"
  # REMOVED: bare "change" (huge false positive).  "gas"/"water"/"green" kept
  #          but REVIEW — gas can mean fuel prices, green is also a party.
  # NOTE: "green" intentionally NOT in the leaders list anymore.
)

immigration_terms <- c(
  "immigration","limmigration","immigrations","immigrant","immigrants",
  "imigration","imigrant","immagration","immgration","immegrants","emigration",
  "refugee","refugees","illegale","foreign"
  # REVIEW: "minority","discrimination" moved out — they read as race, not
  #         immigration. Add to race if desired.
)

healthcare_terms <- c(
  "health","healthcare","health care","heath","heathcare","healtcare",
  "hralthcare","sante","soins","soin","medical","medicare","medicaid",
  "medicade","medicine","hospital","hospitals","doctor","doctors","docteur",
  "prescriptions","pharmacare","drug","drugs","mental","disability","disabled",
  "wellbeing","bien etre"
  # REMOVED: "care" and "life" (false positives).
)

housing_terms <- c(
  "housing","logement","logements","affordable","unaffordable","rent","rents",
  "rental","renting","loyer","homeless","homelessness","sans abris","itinerance",
  "home","homes","dwelling","maisons"
)

seniors_terms <- c(
  "pension","pensions","seniors","senior","elderly","retirement","retraite",
  "retired","retirees","aines","ainees","aine","oas","cpp","aging",
  "vieillissement","vieillesse","old people"
  # REVIEW: dropped bare "age"/"ages"/"65" (too noisy). Add back if needed.
)

leaders_terms <- c(
  "carney","carnay","trudeau","poilievre","poliviere","singh","blanchet",
  "scheer","sheer","bernier","otoole","toole","pierre","andrew","justin",
  "liberal","liberals","liberaux","conservative","conservatives","conservateur",
  "conservateurs","ndp","bloc","leader","leaders","leadership","politician",
  "candidate","candidates","prime minister","pm"
  # REMOVED: "may","mark","marc","green","ford","paul" (common word / names /
  #          colour / provincial premier). Costs us Elizabeth May & Ford
  #          mentions but removes large false-positive volume.
)

ethics_terms <- c(
  "corruption","corrupt","corruptions","coruption","honesty","honest",
  "honestly","dishonesty","honnetete","honntet","ethics","ethical","integrity",
  "integretity","transparency","transparent","transparence","accountability",
  "accountable","accountibility","responsibility","responsible government",
  "trust","trustworthy","credibility","liar","lies","lying","truth","morality",
  "moral","morals","greed","governance","governement","fairness"
  # REVIEW: "rights","represented" dropped (too generic). "trust" kept.
)

education_terms <- c(
  "education","educational","school","schools","schooling","university",
  "tuition","student","students"
)

crime_terms <- c(
  "crime","crimes","criminal","criminals","gang","gangs","gun","firearm",
  "violence","safety","cop"
  # REVIEW: "illegal" dropped here (overlaps immigration). Decide where it lives.
)

indigenous_terms <- c(
  "indigenous","indeginous","aboriginal","native","reconciliation",
  "first nations","first nation"
)

welfare_terms <- c(
  "welfare","childcare","daycare","children","child","childrens","dental",
  "family","families","famille","familles","familiale","poverty","pauvret",
  "basic income","social services","social service","social programs",
  "social program","social assistance","social aid","aide sociale","parental"
  # REMOVED: bare "public". REVIEW: "service"/"services" dropped (too generic).
)

election_terms <- c(
  "election","electoral","voting","voter","vote","representation","democracy",
  "proportional","first past the post"
)

women_terms <- c(
  "women","womens","woman","womans","abortion","abortions","unborn",
  "reproductive","femme","femmes","gender","maternity"
)

security_terms <- c(
  "security","scurit","securite","defense","defence","military","war","wars",
  "international","relations","global","china","israel","palestine","palestin",
  "gaza","terrorism","interference","ingerence","weapons","armes",
  "geopolitiques"
  # REMOVED: "u.s." -> belongs to the US-relations cluster, not security.
  # REVIEW: "global"/"relations" generic.
)

quebec_terms <- c(
  "quebec","qubec","quebecois","francophone","laicite","laicit","lacit"
  # REMOVED: bare "21" (matched ages/addresses).
)

race_terms <- c(
  "race","racism","racist","black","white","antisemitism","islamophobia",
  "islamophoby","minority","discrimination"
)

## ---- US-relations cluster (kept SEPARATE from `security`) -------------------
trump_terms <- c(
  "trump","trumps","turmp","donald","dtrump","usatrump","bufoontrump",
  "president","president"  # REVIEW: "president"/"président" -> almost always
  # Trump in 2025 CA context, but verify.
)

tariff_terms <- c(
  "tariff","tariffs","tarif","tarifs","tarrifs","tarriffs","terrifs","thariff",
  "economytariffs","taxestariffs","big beautiful bill"
  # REMOVED: "economist"/"économiste" (duplication bug).
)

borders_terms <- c(
  "us","usa","america","americain","americains","american","americans","americaines",
  "etats unis","les etat unis","keeping canada","canadian identity","neighbours",
  "border","borders","frontiere","frontieres","sovereignty","sovreignty",
  "souverainete","souvernte","annex","annexation","annexion","independence",
  "independance","independent","independant","autonomy","autonomie","51st",
  "integrite territoriale","the states"
  # "us" RE-ADDED: validation showed it is ~90% genuine country reference. The
  # personal-pronoun cases are removed upstream in clean_str(); the small
  # residual is documented. (Bare "states" still omitted as too generic.)
)

## combined = union of the three component vectors (built programmatically so it
## can never silently disagree with its parts).
combined_terms <- unique(c(trump_terms, tariff_terms, borders_terms))

idk_terms <- c(
  "99","unsure","dont know","idk","prefer not","je ne sais pas","je sais pas",
  "jw sais pas","je ne saos pas","je ne connais","je bsais pas",
  "jai pas de reponse"
  # REMOVED: bare "no" and "nothing" (inflated non-response).
)

################################################################################
## BUILD DUMMIES
################################################################################
ces25 <- ces25 %>%
  mutate(
    economy.dum     = make_dummy(ces25, economy_terms,     "economy"),
    enviro.dum      = make_dummy(ces25, enviro_terms,      "enviro"),
    immigration.dum = make_dummy(ces25, immigration_terms, "immigration"),
    healthcare.dum  = make_dummy(ces25, healthcare_terms,  "healthcare"),
    housing.dum     = make_dummy(ces25, housing_terms,     "housing"),
    seniors.dum     = make_dummy(ces25, seniors_terms,     "seniors"),
    leaders.dum     = make_dummy(ces25, leaders_terms,     "leaders"),
    ethics.dum      = make_dummy(ces25, ethics_terms,      "ethics"),
    education.dum   = make_dummy(ces25, education_terms,   "education"),
    crime.dum       = make_dummy(ces25, crime_terms,       "crime"),
    indigenous.dum  = make_dummy(ces25, indigenous_terms,  "indigenous"),
    welfare.dum     = make_dummy(ces25, welfare_terms,     "welfare"),
    election.dum    = make_dummy(ces25, election_terms,    "election"),
    women.dum       = make_dummy(ces25, women_terms,       "women"),
    security.dum    = make_dummy(ces25, security_terms,    "security"),
    quebec.dum      = make_dummy(ces25, quebec_terms,      "quebec"),
    race.dum        = make_dummy(ces25, race_terms,        "race"),
    trump.dum       = make_dummy(ces25, trump_terms,       "trump"),
    tariff.dum      = make_dummy(ces25, tariff_terms,      "tariff"),
    borders.dum     = make_dummy(ces25, borders_terms,     "borders"),
    combined.dum    = make_dummy(ces25, combined_terms,    "combined"),
    idk.dum         = make_dummy(ces25, idk_terms,         "idk")
  )

################################################################################
## SUMMARY TABLE
## Reminder: non-exclusive mentions; will NOT sum to 100%.
## Show EITHER combined.dum OR (trump/tariff/borders) — not both as additive.
################################################################################
ces25 %>%
  tbl_summary(
    include = c(economy.dum, enviro.dum, immigration.dum, healthcare.dum,
                housing.dum, seniors.dum, leaders.dum, ethics.dum, education.dum,
                crime.dum, indigenous.dum, welfare.dum, election.dum, women.dum,
                security.dum, idk.dum, quebec.dum, race.dum, combined.dum,
                tariff.dum, trump.dum, borders.dum),
    label = list(
      economy.dum     ~ "The Economy",
      enviro.dum      ~ "The Environment",
      immigration.dum ~ "Immigration",                 # was '=' (bug) -> '~'
      healthcare.dum  ~ "Healthcare",
      housing.dum     ~ "Housing",
      seniors.dum     ~ "Seniors",
      leaders.dum     ~ "Party Leaders",
      ethics.dum      ~ "Ethical Concerns",
      education.dum   ~ "Education",
      crime.dum       ~ "Crime",
      indigenous.dum  ~ "Indigenous issues \n and Reconciliation",
      welfare.dum     ~ "Welfare",
      election.dum    ~ "Electoral Reform",
      women.dum       ~ "Women's Issues",
      security.dum    ~ "Security and \n International Relations",
      idk.dum         ~ "Don't know / did not answer",
      quebec.dum      ~ "Quebec",
      race.dum        ~ "Race",
      combined.dum    ~ "US Relations, Trump and Tariffs",
      tariff.dum      ~ "Tariffs",
      trump.dum       ~ "Trump",
      borders.dum     ~ "US Relations"
    )
  )

################################################################################
## VALIDATION — eyeball what actually triggered each category.
## This is the step that earns the table its credibility. Run it before trusting
## any number, and prune the *_terms vectors wherever you see junk.
################################################################################
validate_category <- function(data, dummy_col, n_show = 25) {
  hits <- data$imp_clean[data[[dummy_col]] == 1 & !is.na(data[[dummy_col]])]
  cat("\n=== ", dummy_col, " : ", length(hits), " mentions ===\n", sep = "")
  if (length(hits)) print(utils::head(sample(hits), n_show))
  invisible(NULL)
}

# Examples — run for any category you want to inspect:
# validate_category(ces25, "leaders.dum")   # check after removing may/ford/etc.
# validate_category(ces25, "enviro.dum")    # check 'gas'/'water'/'green'
# validate_category(ces25, "healthcare.dum")# confirm 'care'/'life' gone
# validate_category(ces25, "idk.dum")       # confirm 'no'/'nothing' gone

################################################################################
## SENSITIVITY CHECK — how much does pronoun-stripping actually change "us"?
## We KEEP "us" as a country token (validation: ~90% genuine). The remaining
## question is precision: how many cases does the personal-pronoun stripper in
## clean_str() remove? This rebuilds the US dummies on text cleaned WITHOUT the
## pronoun stripper and reports the difference = the false positives removed.
## If the gap is small, the pronoun contamination was minor and the headline
## US-relations number is robust either way.
################################################################################
## Cleaning identical to clean_str() but SKIPPING the pronoun-removal step.
clean_str_nostrip <- function(x) {
  x <- gsub("\\bU\\.?S\\.?A\\.?\\b", " usa ", x)
  x <- gsub("\\bU\\.?S\\.?\\b",      " usa ", x)
  x <- tolower(x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
ces25$imp_raw <- clean_str_nostrip(ces25$cps25_imp_iss)

make_dummy_on <- function(data, col, terms, label) {
  dict    <- dictionary(setNames(list(clean_terms(terms)), label))
  corpusA <- tokens(data[[col]])
  res     <- convert(dfm(tokens_lookup(corpusA, dict, nested_scope = "dictionary")),
                     to = "data.frame")
  as.integer(res[[label]] >= 1)
}

ces25 <- ces25 %>%
  mutate(
    borders.dum_raw  = make_dummy_on(ces25, "imp_raw", borders_terms,  "borders"),
    combined.dum_raw = make_dummy_on(ces25, "imp_raw", combined_terms, "combined")
  )

cat("\n--- pronoun-stripping sensitivity (us kept as country) ---\n")
cat("borders  : stripped =", sum(ces25$borders.dum,  na.rm = TRUE),
    " | no-strip =", sum(ces25$borders.dum_raw,  na.rm = TRUE),
    " | pronoun FPs removed =",
    sum(ces25$borders.dum_raw  - ces25$borders.dum,  na.rm = TRUE), "\n")
cat("combined : stripped =", sum(ces25$combined.dum, na.rm = TRUE),
    " | no-strip =", sum(ces25$combined.dum_raw, na.rm = TRUE),
    " | pronoun FPs removed =",
    sum(ces25$combined.dum_raw - ces25$combined.dum, na.rm = TRUE), "\n")

# To READ the cases the pronoun stripper removed (verify they are true pronouns):
# ces25$imp_raw[ces25$borders.dum_raw == 1 & ces25$borders.dum == 0]


validate_category(ces25, "idk.dum")

validate_category(ces25, "idk.dum", n_show = 100)

#### This code checks any terms in the code above that has not yet been included in the dictionaries
rm(dictionary_terms)
#Capture all items in the environment that start with the term dictionary and store them in
#dicts
dicts<-mget(ls(pattern="_terms"))
dicts
#Take the list of dicdtionaries made above
dicts %>%
  #unlist them
  map(., ~unlist(.x)) %>% unlist() %>%
  #remove duplicates and store in the object dictionary_terms terms
  unique()->dict_terms
#Tokenize the important issue variable
tokens(ces25$cps25_imp_iss) %>%
  #Remove all stopwords
  tokens_remove(c(stopwords("en"), stopwords("fr")))->toks
toks_unmatched <- tokens_select(
  toks,
  pattern = dict_terms,
  selection = "remove"
)
?tokens_select
dfm_unmatched <- dfm(toks_unmatched)
dfm_unmatched %>%
  textstat_frequency()
?topfeatures
topfeatures(dfm_unmatched, n=50)



