# ------------------------------------------------------------------
# Recode VAR006 (1984 CES constituency screener) into a clean
# character / factor variable with proper riding names.
#
# Fixes:
#   - padding mismatch: data values are space-padded ("  1"),
#     label keys are zero-padded ("001")
#   - adds missing label: code 002 = BURIN-BURGEO
#   - dedupe-safe; warns on any data code lacking a label
# ------------------------------------------------------------------

library(dplyr)
library(stringr)

x <- ces84$VAR006   # the haven_labelled vector

# 1. Invert existing labels: names() are ridings, values are codes -> want code -> name
labs   <- attr(x, "labels")
lookup <- setNames(names(labs), unname(labs))   # names = code, value = riding

# 2. Repair the label set
lookup["002"] <- "BURIN-BURGEO"                  # add the missing FED
lookup <- lookup[!duplicated(names(lookup))]     # dedupe-safe (keeps first if any dup exists)

# 3. Normalise data values to the zero-padded 3-char key format
codes <- str_pad(str_trim(as.character(x)), width = 3, side = "left", pad = "0")

# 4. Coverage check BEFORE converting -- surfaces any code with no label
missing <- setdiff(unique(codes), names(lookup))
if (length(missing)) warning("codes with no label: ", paste(missing, collapse = ", "))

# 5. Build both a clean character and an ordered factor
constituency_chr <- unname(lookup[codes])
constituency     <- factor(constituency_chr,
                           levels = lookup[order(as.integer(names(lookup)))])

ces84 %>%
  slice(1:10) %>%
  select(VAR006)
constituency_chr[1:10]
ces84$constituency<-constituency_chr
#check
ces84 %>%
  select(VAR006, constituency) %>%
  slice_sample(n=25)
rm(constituency)
rm(constituency_chr)
# Optional: drop the empty BURIN-BURGEO level if you want observed ridings only
# constituency <- droplevels(constituency)
