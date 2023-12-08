
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to facilitate collaboration and research on
the full life cycle of the Canada Election Study.

It solves two problems.

First, it stores all extant CES data files in one spot, quickly loadable
in R. Second, it makes available work conducted by Matt Polacko and
Simon Kiss to systematically name and recode variables through the
lifecycle of the CES. Both are obstacles to research in Canadian
politics.

## Installation

You can install the development version of `cesdata2` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sjkiss/cesdata2")
```

## How to use it.

Once installed and loaded, individual datasets are *immediately*
available.

``` r
library(cesdata2)
## view sample datasets
library(tidyverse)
#CES21 is immediately available
ces21 %>% 
  select(1:5) %>% 
  glimpse()
#> Rows: 22,328
#> Columns: 5
#> $ cps21_StartDate       <dttm> 2021-09-19 06:14:46, 2021-09-15 15:23:33, 2021-…
#> $ cps21_EndDate         <dttm> 2021-09-19 06:28:25, 2021-09-15 15:46:57, 2021-…
#> $ Status                <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ Progress              <dbl> 100, 100, 100, 100, 100, 100, 100, 100, 100, 100…
#> $ Duration__in_seconds_ <dbl> 818, 1403, 775, 825, 1660, 1332, 1240, 1594, 945…

#As is CES1965

ces21 %>% 
  select(1:5) %>% 
  glimpse()
#> Rows: 22,328
#> Columns: 5
#> $ cps21_StartDate       <dttm> 2021-09-19 06:14:46, 2021-09-15 15:23:33, 2021-…
#> $ cps21_EndDate         <dttm> 2021-09-19 06:28:25, 2021-09-15 15:46:57, 2021-…
#> $ Status                <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ Progress              <dbl> 100, 100, 100, 100, 100, 100, 100, 100, 100, 100…
#> $ Duration__in_seconds_ <dbl> 818, 1403, 775, 825, 1660, 1332, 1240, 1594, 945…
```

These are the datasets currently available with the names required to
load them.

| Dataset title           |     Filename |                                                                                                 Notes                                                                                                  |
|:------------------------|-------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| CES 1965                |      `ces65` |                                                                                                                                                                                                        |
| CES 1968                |      `ces68` |                                                                                                                                                                                                        |
| CES 1972                |  `ces72_nov` |                                                                          Only the November, post-election dataset is included                                                                          |
| CES 1974-1979-1980      |      ces7980 |                                                               Notes about who is included in the 1980 dataset, how variables were used??                                                               |
| CES 1984                |      `ces84` |                                                                                                                                                                                                        |
| CES 1988                |      `ces88` |                                                                                                                                                                                                        |
| CES 1993                |      `ces93` |                                                                                 Notes about Quebec referendum sample?                                                                                  |
| CES 2000                |      `ces00` |                                                                                                                                                                                                        |
| CES 2004-2006-2008-2011 |    `ces0411` | this is the combined file for campaign period, post-election and mailback surveys for each of four federal elections. It requires special handling in the construction of a master dataset. See below. |
| CES 2015 Phone          | `ces15phone` |                                                                                         Phone survey from 2015                                                                                         |
| CES 2015 Web            |   `ces15web` |                                                                                          Web survey from 2015                                                                                          |
| CES 2019 Phone          | `ces19phone` |                                                                                         Phone survey from 2019                                                                                         |
| CES 2019 Web            |   `ces19web` |                                                                                         Phone survey from 2019                                                                                         |
| CES 2021 Web            |      `ces21` |                                                                                         Werb survey from 2021                                                                                          |

# Package Structure

It is worth describing the package structure. We have followed advice on
creating so-called `data` packages [here](https://r-pkgs.org/data.html)
and [here](https://www.davekleinschmidt.com/r-packages/).

Currently the `cesdata2` package comes with two files for each Canada
Election Study.

1.  A raw, unedited data file from publicly available libraries of the
    CES studies, in either SPSS or Stata format. These files are stored
    in the package’s `data-raw` subfolder.

``` r
list.files(path="data-raw")
#>  [1] "1974_1979_1980.sav"                                  
#>  [2] "1984.sav"                                            
#>  [3] "2016-unique-occupations-updated.xls"                 
#>  [4] "2019 Canadian Election Study - Phone Survey v1.0.dta"
#>  [5] "2021_occupations_coded.xlsx"                         
#>  [6] "CES_04060811_ISR_revised.sav"                        
#>  [7] "CES-E-1972-jun-july_F1.sav"                          
#>  [8] "CES-E-1972-nov_F1.sav"                               
#>  [9] "CES-E-1972-sept_F1.sav"                              
#> [10] "CES-E-1974_F1.sav"                                   
#> [11] "CES-E-1993_F1.sav"                                   
#> [12] "CES-E-1997_F1.sav"                                   
#> [13] "CES-E-2000_F1.sav"                                   
#> [14] "CES-E-2019-online_F1.sav"                            
#> [15] "CES15_CPS+PES_Web_SSI Full.dta"                      
#> [16] "ces1965.dta"                                         
#> [17] "ces1968.dta"                                         
#> [18] "CES1988.sav"                                         
#> [19] "CES2015_CPS-PES-MBS_complete.sav"                    
#> [20] "ces21.dta"                                           
#> [21] "mip_2019.xlsx"                                       
#> [22] "recode_scripts"
```

2.  An `.rda` file for each that contains the results of our recode
    scripts. That is to say, it includes recoded variables using our
    systematic conventions. These are stored in the package’s `data`
    subfolder.

``` r
list.files(path="data")
#>  [1] "ces00.rda"      "ces0411.rda"    "ces15phone.rda" "ces15web.rda"  
#>  [5] "ces19phone.rda" "ces19web.rda"   "ces21.rda"      "ces65.rda"     
#>  [9] "ces68.rda"      "ces72_nov.rda"  "ces74.rda"      "ces7980.rda"   
#> [13] "ces84.rda"      "ces88.rda"      "ces93.rda"      "ces97.rda"
```

The recode scripts themselves are stored in the package subfolder
`data-raw/recode_scripts/`. When these scripts are executed via R’s
`source()` command, they:

1.  Import the Stata or SPSS file from `data-raw`
2.  Execute all documented recode functions
3.  Save an `.rda` object, properly named, in the folder `data`. Doing
    so, makes the `.rda` file, with all recodes executed, available to
    users on install and loading.

## Recoding conventions

Polacko and Kiss (particularly the former) have gone to great lengths to
recode and rename variables of interest to allow for combination into a
single tabular dataset that allows for time series analysis.

Wherever possible, the following conventions have been used.

1.  Likert items have been rescaled from 0 to 1
2.  Liberal views are scored 0 and conservative views are scaled
    high.MATT IS THIS CORRECT
3.  Directional items are scored low (0) to high (1)

- Party and leader thermometer ratings are scored (1, strongly dislike,
  100 strongly like)
- Spending preferences are scored such that 0 is less spending 1 is more
  spending (even though this might conflict with the liberal and
  conservative ratings in 2. )
- evaluations of the economy and personal financial situation are scored
  such that 0 is worse or bad and 1 is better or good.

This is an incomplete list of recoded and renamed variables.

| Concept | Variable Name |  notes  |
|:--------|--------------:|:-------:|
| This    |          This |  This   |
| column  |        column | column  |
| will    |            be | aligned |
| left    |         right | center  |

# Establishing A Usable Time Series

In the simple case of combining three single-election datasets, we can
combine them in this way.

1.  Make a list of your datasets.
2.  Provide names for each list item, corresponding to the election year
3.  Make a separate vector of all the variable names you want to select
4.  Combine `map()` and `select(any_of())` and `list_rbind()`to select
    those variables and join them together in a tabular data frame.

``` r
library(tidyverse)
library(haven) 
#make a list of datasets
ces.list<-list(ces00, ces93, ces97)
# Names
names(ces.list)<-c(2000, 1993, 1997)
#Make a vector of desired common variables
myvars<-c("male", "degree", "election")
#Start with ces.list
ces.list %>% 
  #Map onto each data frame in ces.list the function select()
  # select any of the variables in the object myvars
  map(., select, any_of(myvars)) %>% 
  #bind everything into one data frame and save it as ces
  list_rbind(.)->ces
#Glimpse 
glimpse(ces)
#> Rows: 12,471
#> Columns: 3
#> $ male     <dbl+lbl> 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, …
#> $ degree   <dbl+lbl> 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ election <dbl> 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2…
```

### Issues and Special Cases

1.  A variable is not in one of the datasets.

One of the desirable aspects for using the approach of defining common
variables and then mapping the `select()` function to each data frame in
a list outlined above is that it will still add a CES data frame to the
master data frame, even if it does not have the renamed and recoded
variable. It will simply populate rows from that data frame with `NA`.

Consider the variable `gay_rights`, it only starts in CES 1993. But it
is plausible to imagine a research project where attitudes to gay rights
are only one part of the project and one wishes to use data from CES
1988 for other parts.

``` r
# Construct the list
ces.list<-list(ces84,ces88, ces93, ces97, ces00)
#names
names(ces.list)<-c(1984,1988, 1993, 1997, 2000)
# Check to see if ces84 gay_rights exists
length(ces84$gay_rights) # Zero indicates it does not exist
#> [1] 0
#Check to see if ces88 gay_rights exists
length(ces88$gay_rights) #3609 cases of gay_rights values
#> [1] 3609
# Construct common variables
myvars<-c("gender", "vote", "gay_rights", "election")
#Make data frame
ces.list %>% 
  #Select the variables
  map(., select, any_of(myvars)) %>% 
  #bind together providing a new variable Election containing the election year
  list_rbind(.)->ces
```

Now we can confirm what was happen with the variable `gay_rights` in
each year.

``` r
#Check the gay_rights variable in 1984
ces %>% 
  filter(election==1984) %>% 
  select(gay_rights)
#> # A tibble: 3,377 × 1
#>    gay_rights
#>         <dbl>
#>  1         NA
#>  2         NA
#>  3         NA
#>  4         NA
#>  5         NA
#>  6         NA
#>  7         NA
#>  8         NA
#>  9         NA
#> 10         NA
#> # ℹ 3,367 more rows
```

``` r
#Check the gay_rights variable in 1988
ces %>% 
  filter(election==1988) %>% 
  select(gay_rights)
#> # A tibble: 3,609 × 1
#>    gay_rights
#>         <dbl>
#>  1       NA  
#>  2       NA  
#>  3       NA  
#>  4       NA  
#>  5       NA  
#>  6       NA  
#>  7        0.5
#>  8       NA  
#>  9        0.5
#> 10        0  
#> # ℹ 3,599 more rows
```

2.  Panel datasets

    - 1974-1979-1980 There is a complete 1974-1979-1980 Canada Election
      Study file and it is loaded into `cesdata2` as `ces7980`. However,
      we have also loaded the original 1974 file into `cesdata2` as
      `ces74`. Effectively we did not make use of the panel feature for
      1974-1979. There are respondents in `ces7980` that did participate
      in the 1974 study. Users can track them down using the variables
      contained in `ces7980`, but we haven’t down that.

The user can search through `ces7980` in this way to filter respondents
as they wish in the master file that is necessary to make their CES
data-set.

``` r
lookfor(ces7980, "1979 Filter|1980 Filter|1974 Filter|P74|P79")
#>  pos  variable label                   col_type values
#>  487  V1007    1979 FILTER:1           dbl            
#>  1023 V2005    1980 FILTER:1           dbl            
#>  1221 V4002    1979 FILTER:1           dbl            
#>  1227 V4008    1980 FILTER:1           dbl            
#>  1232 V4013    P74-79-80 NATIONAL WGHT dbl            
#>  1233 V4014    P74-79-80 FILTER:1      dbl            
#>  1234 V4015    P74-79-80 MARITIME WGHT dbl            
#>  1235 V4016    P74-79-80 ATLANTIC WGHT dbl            
#>  1236 V4017    P74-79-80 PRAIRIE WGHT  dbl            
#>  1237 V4018    P74-79-80 WESTERN WGHT  dbl            
#>  1238 V4019    P74-79 NATIONAL WGHT    dbl            
#>  1239 V4020    P74-79 FILTER:1         dbl            
#>  1240 V4021    P74-79 MARITIME WGHT    dbl            
#>  1241 V4022    P74-79 ATLANTIC WGHT    dbl            
#>  1242 V4023    P74-79 PRAIRIE WGHT     dbl            
#>  1243 V4024    P74-79 WESTERN WGHT     dbl            
#>  1244 V4025    P79-80 NATIONAL WGHT    dbl            
#>  1245 V4026    P79-80 FILTER:1         dbl            
#>  1246 V4027    P79-80 MARITIME WGHT    dbl            
#>  1247 V4028    P79-80 ATLANTIC WGHT    dbl            
#>  1248 V4029    P79-80 PRAIRIE WGHT     dbl            
#>  1249 V4030    P79-80 WESTERN WGHT     dbl            
#>  1250 V4031    P74-80 NATIONAL WGHT    dbl            
#>  1251 V4032    P74-80 FILTER:1         dbl            
#>  1252 V4033    P74-80 MARITIME WGHT    dbl            
#>  1253 V4034    P74-80 ATLANTIC WGHT    dbl            
#>  1254 V4035    P74-80 PRAIRIE WGHT     dbl            
#>  1255 V4036    P74-80 WESTERN WGHT     dbl
```

It is possible though that the recode script may need to be modified in
order to make potential use of this feature. Good luck.

In addition, MATT CAN YOU EXPLAIN WHAT HAPPENED WITH 1980

    * 1992 Referendum and 1993 Election Survey

The object `ces93` contains respondents that participated in both the
1992 Referendum *and* the 1993 general election. All the original
distinguishing variables are included in `ces93` for the user to filter
and select as necessary.

``` r
ces93 %>% 
  select(contains("RTYPE"), contains("CESTYPE")) %>% 
  as_factor() %>% 
  head()
#> # A tibble: 6 × 12
#>   RTYPE1 RTYPE2 RTYPE3 RTYPE4 RTYPE5 RTYPE6 RTYPE7 RTYPE8 RTYPE9 RTYPE10 RTYPE11
#>   <fct>  <fct>  <fct>  <fct>  <fct>  <fct>  <fct>  <fct>  <fct>  <fct>   <fct>  
#> 1 <NA>   <NA>   Campa… Post-… Mailb… <NA>   <NA>   <NA>   Rdd: … Rdd: 2… Rdd: 1…
#> 2 <NA>   <NA>   Campa… Post-… Mailb… <NA>   <NA>   <NA>   Rdd: … Rdd: 2… Rdd: 1…
#> 3 <NA>   <NA>   Campa… Post-… Mailb… <NA>   <NA>   <NA>   Rdd: … Rdd: 2… Rdd: 1…
#> 4 <NA>   <NA>   Campa… Post-… Mailb… <NA>   <NA>   <NA>   Rdd: … Rdd: 2… Rdd: 1…
#> 5 <NA>   <NA>   Campa… <NA>   <NA>   <NA>   <NA>   <NA>   <NA>   <NA>    Rdd: 1…
#> 6 <NA>   <NA>   Campa… Post-… Mailb… <NA>   <NA>   <NA>   Rdd: … Rdd: 2… Rdd: 1…
#> # ℹ 1 more variable: CESTYPE <fct>
```

    * 2004-2011

The CES 2004-2011 is a very large file and presents some significant
challenges given our renaming and recoding strategy. However, we have
come up with a method that is expansive in that recodes are run on every
respondent and can then be efficiently filtered to select respondent
values for specific sets of surveys.

The underlying data files look like this the following table, for *each*
respondent, there are variables for the 2004, 2006, and 2008 surveys
*whether or not* the respondent actually completed the surveys.
Respondent ID \|Survey \|`ces04_PES_K5A` \| `ces06_PES_B4A`
\|`ces08_PES_B4B` \| \|:—–:\|:—–:\|:—————————:\| :———————-:\| :——-:\| \|
1\|2004 \| `Refused` \| NA\| NA\| \| 2\|2006 \| NA \| Liberal Party of
Canada \|NA\| \|3 \|2008\| NA \| NA \| NDP \|4 \|2004 and 2006\| Other
\| Liberal Party of Canada \| NA\| \|5 \| 2006 and 2008 \| NA \|

Ultimately, each of these columns measure the same variable, the
person’s vote cast, but at different time periods. As currently
constructed, the 2004-2006-2008-2011 will not permit the same name,
e.g. `vote` to be used in more than one column.

As a result, we use the following naming conventions in this data-set
for variables that have been recoded (e.g. small parties excluded,
`Don't Know` responses set to `NA`, `Refused` set to `NA`, etc. )

| Respondent ID |    Survey     | `ces04_PES_K5A` |     `ces06_PES_B4A`     | `ces08_PES_B4B` | `vote04` | `vote06` | `vote08` |
|:-------------:|:-------------:|:---------------:|:-----------------------:|:---------------:|:--------:|:--------:|:--------:|
|       1       |     2004      |    `Refused`    |           NA            |       NA        |    NA    |    NA    |    NA    |
|       2       |     2006      |       NA        | Liberal Party of Canada |       NA        |    NA    | Liberal  |    NA    |
|       4       | 2004 and 2006 |      Other      |         Liberal         |       NA        |    NA    | Liberal  |    NA    |

In order to combine these variables successfully into one tabular
dataset, it is first necessary to: 1) split the 2004-2011 dataset into
separate datasets for each time election, 2) rename the variables taking
out the year indicators, 3) add the split datasets to the list of CES
data sets and then recombine.

We could have separated the each election dataset into a separate one in
the backend, however, there is a lot of overlap in respondent
participation. We did not want to presume how analysts would want to
proceed selecting which respondents who took which surveys should be
counted in or not. There are advantages and disadvantages to different
approaches. So, as a result this step needs to be combined with a step
where the researcher specifies which of these surveys you wish to use.
Here is the problem….

| Var1                                                                      | Freq |
|:--------------------------------------------------------------------------|-----:|
| CPS04                                                                     | 1182 |
| CPS04 PES04                                                               |  671 |
| CPS04 PES04 CPS06 \*                                                      |  107 |
| CPS04 PES04 CPS06 PES06                                                   |  207 |
| CPS04 PES04 MBS04                                                         |  471 |
| CPS04 PES04 MBS04 CPS06                                                   |   71 |
| CPS04 PES04 MBS04 CPS06 PES06                                             |  229 |
| CPS06                                                                     |  493 |
| CPS06 PES06                                                               | 1566 |
| CPS08                                                                     |  806 |
| CPS08 PES08                                                               | 1233 |
| CPS08 PES08 MBS08                                                         | 1218 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 \*\*                                |  109 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08                               |   58 |
| Panel - CPS04 PES04 CPS06 PES08                                           |   29 |
| Panel - CPS04 PES04 CPS06 PES08 MBS08                                     |    7 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08                               |   99 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08                         |  204 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08                                     |   10 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08                               |   19 |
| New RDD_2011 CPS11                                                        |  863 |
| CPS04 PES04 CPS06 CPS11                                                   |    6 |
| CPS04 PES04 CPS06 PES06 CPS11                                             |   13 |
| CPS04 PES04 MBS04 CPS06 CPS11                                             |    6 |
| CPS04 PES04 MBS04 CPS06 PES06 CPS11                                       |   12 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 CPS11                               |   15 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 CPS11                         |    4 |
| Panel - CPS04 PES04 CPS06 PES08 CPS11                                     |    2 |
| Panel - CPS04 PES04 CPS06 PES08 MBS08 CPS11                               |    0 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 CPS11                         |    7 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 CPS11                   |   16 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 CPS11                               |    1 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 CPS11                         |    1 |
| New RDD_2011 CPS11 PES11                                                  | 1492 |
| CPS04 PES04 CPS06 CPS11 PES11                                             |    6 |
| CPS04 PES04 CPS06 PES06 CPS11 PES11                                       |   42 |
| CPS04 PES04 MBS04 CPS06 CPS11 PES11                                       |    2 |
| CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11                                 |   20 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 CPS11 PES11                         |   88 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 CPS11 PES11                   |   22 |
| Panel - CPS04 PES04 CPS06 PES08 CPS11 PES11                               |    7 |
| Panel - CPS04 PES04 CPS06 PES08 MBS08 CPS11 PES11                         |    2 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 CPS11 PES11                   |   52 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 CPS11 PES11             |   83 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 CPS11 PES11                         |    7 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 CPS11 PES11                   |    5 |
| New RDD_2011 CPS11 PES11 MBS11                                            |  584 |
| CPS04 PES04 CPS06 CPS11 PES11 MBS11                                       |    1 |
| CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11                                 |    5 |
| CPS04 PES04 MBS04 CPS06 CPS11 PES11 MBS11                                 |    2 |
| CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11                           |   12 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 CPS11 PES11 MBS11                   |   13 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 CPS11 PES11 MBS11             |   15 |
| Panel - CPS04 PES04 CPS06 PES08 MBS08 CPS11 PES11 MBS11                   |    2 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 CPS11 PES11 MBS11             |   48 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 CPS11 PES11 MBS11       |  105 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 CPS11 PES11 MBS11                   |    1 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 CPS11 PES11 MBS11             |    3 |
| New RDD_2011 CPS11 PES11 MBS11 WBS11                                      |  519 |
| CPS04 PES04 CPS06 CPS11 PES11 MBS11 WBS11                                 |    2 |
| CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11 WBS11                           |    3 |
| CPS04 PES04 MBS04 CPS06 CPS11 PES11 MBS11 WBS11                           |    1 |
| CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11 WBS11                     |   14 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 CPS11 PES11 MBS11 WBS11             |    7 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 CPS11 PES11 MBS11 WBS11       |   20 |
| Panel - CPS04 PES04 CPS06 PES08 MBS08 CPS11 PES11 MBS11 WBS11             |    4 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 CPS11 PES11 MBS11 WBS11       |   20 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 CPS11 PES11 MBS11 WBS11 |  141 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 CPS11 PES11 MBS11 WBS11             |    2 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 CPS11 PES11 MBS11 WBS11       |   10 |

There are multiple ways to split the file. You can split it very
precisely and narrowly. For example, one can pick only the respondents
who completed the CPS04, the CPS04 PES04 or the CPS04 PES04 MBS04
sequence by using `filter()` and a regular expression in `str_detect()`.
Note, to diagnose the accuracy of your regular expression and filter
strategy, you can first `select()` only the `survey` variable and count
what your filter strategy produces, and then actually filter into a new
data frame.

| survey |    n |
|:-------|-----:|
| CPS04  | 1182 |

``` r
ces0411 %>% 
  select(survey) %>% 
  filter(survey=="CPS04"|survey=="CPS04 PES04"|survey=="CPS04 PES04 MBS04") %>% 
  count(survey) %>% 
  kable()
```

| survey            |    n |
|:------------------|-----:|
| CPS04             | 1182 |
| CPS04 PES04       |  671 |
| CPS04 PES04 MBS04 |  471 |

When satisfied, it is essential of course to save the results in a
single-year dataset.

``` r
ces0411 %>% 
  filter(survey=="CPS04"|survey=="CPS04 PES04"|survey=="CPS04 PES04 MBS04")->
# Note we are making a new ces04 object that corresponds in naming to all other election year files
ces04

#One can compare the rows
nrow(ces0411)
#> [1] 13097
nrow(ces04)
#> [1] 2324
```

Please note how this interacts with the above strategy of providing
renamed and recoded variables for each variable. The respondents in
`ces04` *never had the chance to participate in any other year surveys*.
But they will have variables in the new `ces04` dataset for later year
variabes; they will simply all be `NA`.

``` r
#Note I am using the ces04 dataframe
ces04 %>% 
  select(vote04, vote06, vote08, vote11) %>% 
  slice_sample(n=20) %>% 
  kable()
```

| vote04 | vote06 | vote08 | vote11 |
|-------:|-------:|-------:|-------:|
|      1 |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|      4 |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|      1 |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|      2 |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|      3 |     NA |     NA |     NA |
|      1 |     NA |     NA |     NA |
|      4 |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |

Please note that alternative, more expansive selection strategies are
available. For example, we can collect *any* respondent who responded to
any survey by using `str_detect()`. You can see the effects of such a
strategy here:

``` r
ces0411 %>% 
  select(survey) %>% 
  filter(str_detect(survey, "PES08")) %>% 
  count(survey) %>% 
  kable()
```

| survey                                                                    |    n |
|:--------------------------------------------------------------------------|-----:|
| CPS08 PES08                                                               | 1233 |
| CPS08 PES08 MBS08                                                         | 1218 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 \*\*                                |  109 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08                               |   58 |
| Panel - CPS04 PES04 CPS06 PES08                                           |   29 |
| Panel - CPS04 PES04 CPS06 PES08 MBS08                                     |    7 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08                               |   99 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08                         |  204 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08                                     |   10 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08                               |   19 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 CPS11                               |   15 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 CPS11                         |    4 |
| Panel - CPS04 PES04 CPS06 PES08 CPS11                                     |    2 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 CPS11                         |    7 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 CPS11                   |   16 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 CPS11                               |    1 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 CPS11                         |    1 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 CPS11 PES11                         |   88 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 CPS11 PES11                   |   22 |
| Panel - CPS04 PES04 CPS06 PES08 CPS11 PES11                               |    7 |
| Panel - CPS04 PES04 CPS06 PES08 MBS08 CPS11 PES11                         |    2 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 CPS11 PES11                   |   52 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 CPS11 PES11             |   83 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 CPS11 PES11                         |    7 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 CPS11 PES11                   |    5 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 CPS11 PES11 MBS11                   |   13 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 CPS11 PES11 MBS11             |   15 |
| Panel - CPS04 PES04 CPS06 PES08 MBS08 CPS11 PES11 MBS11                   |    2 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 CPS11 PES11 MBS11             |   48 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 CPS11 PES11 MBS11       |  105 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 CPS11 PES11 MBS11                   |    1 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 CPS11 PES11 MBS11             |    3 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 CPS11 PES11 MBS11 WBS11             |    7 |
| Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 CPS11 PES11 MBS11 WBS11       |   20 |
| Panel - CPS04 PES04 CPS06 PES08 MBS08 CPS11 PES11 MBS11 WBS11             |    4 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 CPS11 PES11 MBS11 WBS11       |   20 |
| Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 CPS11 PES11 MBS11 WBS11 |  141 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 CPS11 PES11 MBS11 WBS11             |    2 |
| Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 CPS11 PES11 MBS11 WBS11       |   10 |

This might be desirable to increase sample size, but it might be
undesirable to include panel respondents, who, after having been
interviewed multiple times, may be demonstrate different responses than
other purely cross-sectional respondents. *We leave this to the user.*

For now, we will keep the filtering strategy and save the resulting
object as `ces08`.

``` r
ces0411 %>% 
  filter(str_detect(survey, "PES08"))->ces08
```

3.  Renaming re-coded variables in `ces04`, `ces06`, `ces08` and `ces11`
    data frames.

Because respondents in`ces04` and `ces08` all come from the giant
combined file, all have recoded variables from each election. For
variables they were not eligible to report because they weren’t in that
survey, their values are `NA`

``` r
ces04 %>% 
select(contains('vote')) %>% 
  as_factor()
#> # A tibble: 2,324 × 5
#>    ces08_PES_PROV_VOTE1 vote04       vote06 vote08 vote11
#>    <fct>                <fct>        <fct>  <fct>  <fct> 
#>  1 <NA>                 <NA>         <NA>   <NA>   <NA>  
#>  2 <NA>                 <NA>         <NA>   <NA>   <NA>  
#>  3 <NA>                 Conservative <NA>   <NA>   <NA>  
#>  4 <NA>                 Conservative <NA>   <NA>   <NA>  
#>  5 <NA>                 <NA>         <NA>   <NA>   <NA>  
#>  6 <NA>                 <NA>         <NA>   <NA>   <NA>  
#>  7 <NA>                 NDP          <NA>   <NA>   <NA>  
#>  8 <NA>                 Conservative <NA>   <NA>   <NA>  
#>  9 <NA>                 <NA>         <NA>   <NA>   <NA>  
#> 10 <NA>                 <NA>         <NA>   <NA>   <NA>  
#> # ℹ 2,314 more rows
```

Note, because we used an expansive filtering strategy for `ces08` there
are respondents who actully have responses for variables from 2004.
However, because we are defining this as a dataset for 2008, we will
effectively drop those earlier responses and only take respondents’
values from 2008.

``` r
ces08 %>% 
  select(contains('vote')) %>% 
  as_factor()
#> # A tibble: 3,689 × 5
#>    ces08_PES_PROV_VOTE1     vote04  vote06       vote08       vote11      
#>    <fct>                    <fct>   <fct>        <fct>        <fct>       
#>  1 NDP (New Democrats)      Green   NDP          NDP          Conservative
#>  2 NDP (New Democrats)      Liberal Liberal      Liberal      Liberal     
#>  3 R volunteers: It depends <NA>    Liberal      <NA>         NDP         
#>  4 NDP (New Democrats)      Liberal Liberal      NDP          NDP         
#>  5 Other                    Bloc    NDP          NDP          NDP         
#>  6 L'Action Democratique    Bloc    NDP          NDP          NDP         
#>  7 refused                  <NA>    Conservative <NA>         Conservative
#>  8 Liberal (Grits)          Green   Green        Liberal      Green       
#>  9 Liberal (Grits)          Bloc    NDP          Bloc         Conservative
#> 10 Liberal (Grits)          Liberal Conservative Conservative Conservative
#> # ℹ 3,679 more rows
```

Previously, we had a fairly laborious method of individually renaming
variables such as `vote04` to be `vote`. There is a cleaner and quicker
way to accomplish this. Specifically, for each datafram that comes from
the `ces0411` combined file, we just strip out any instance of `04` in
the `ces04` data frame, `06` in the `ces06` dataframe.

You can see the effects of this below:

``` r

#Take only the ces04 data frame
ces04 %>% 
  #Select variables containing vote and trad
  select(contains('vote'), contains('trad')) %>% 
  #Show the names of the variables
  names()
#>  [1] "ces08_PES_PROV_VOTE1" "vote04"               "vote06"              
#>  [4] "vote08"               "vote11"               "trad043"             
#>  [7] "trad047"              "trad041"              "trad044"             
#> [10] "trad045"              "trad046"              "trad042"             
#> [13] "traditionalism04"     "traditionalism204"    "trad063"             
#> [16] "trad062"              "trad061"              "traditionalism06"    
#> [19] "traditionalism206"    "trad083"              "trad087"             
#> [22] "trad081"              "trad084"              "trad085"             
#> [25] "trad086"              "trad082"              "traditionalism08"    
#> [28] "traditionalism208"    "trad113"              "trad117"             
#> [31] "trad111"              "trad114"              "trad115"             
#> [34] "trad116"              "trad112"              "traditionalism11"    
#> [37] "traditionalism211"
```

Here it is apparent that there are consistent variables for `04`, `06`
and `08` even in the data set that contains only respondents that
participated in the `04` survey.

If we strip out `04` from the variable names we are left with.

``` r
#Take only the ces04 data frame
ces04 %>% 
  #Select variables containing vote and trad
  select(contains('vote'), contains('trad')) %>% 
  #Show the names of the variables
  names() %>% 
  str_remove_all(., "04")
#>  [1] "ces08_PES_PROV_VOTE1" "vote"                 "vote06"              
#>  [4] "vote08"               "vote11"               "trad3"               
#>  [7] "trad7"                "trad1"                "trad4"               
#> [10] "trad5"                "trad6"                "trad2"               
#> [13] "traditionalism"       "traditionalism2"      "trad063"             
#> [16] "trad062"              "trad061"              "traditionalism06"    
#> [19] "traditionalism206"    "trad083"              "trad087"             
#> [22] "trad081"              "trad084"              "trad085"             
#> [25] "trad086"              "trad082"              "traditionalism08"    
#> [28] "traditionalism208"    "trad113"              "trad117"             
#> [31] "trad111"              "trad114"              "trad115"             
#> [34] "trad116"              "trad112"              "traditionalism11"    
#> [37] "traditionalism211"
```

Now, for example, in the `ces04` data frame, `vote04` has become `vote`,
`trad041` has become `trad1`, etc. But `vote06`and `trad06` remains
untouched.

We make the permanent changes to the data-sets in the following way:

``` r

#Rename variables in 2008
#Strip out any instance of `08` in the names of 08
names(ces08)<-str_remove_all(names(ces08), "08")
```

And now, we could proceed, as noted above, creating a unified tabular
ces dataset from with other, more simple, one-election datafiles.

``` r
#List the data frames to be used.
ces.list<-list(ces97, ces00, ces04, ces08)

#Provide names
names(ces.list)<-c(1997, 2000, 2004, 2008)
myvars<-c("gender", "vote", "gay_rights", "trad1", "election", "mode")

#Make data frame
ces.list %>% 
  #Select the variables
  map(., select, any_of(myvars)) %>% 
  #bind together providing a new variable Election containing the election year
  bind_rows(.)->ces
#Show results
head(as_factor(ces))
#> # A tibble: 6 × 5
#>   vote         gay_rights trad1 election mode 
#>   <fct>             <dbl> <dbl>    <dbl> <chr>
#> 1 Conservative       0.25  0        1997 Phone
#> 2 NDP                0.75  0.75     1997 Phone
#> 3 <NA>               0.25  0.25     1997 Phone
#> 4 Liberal           NA     0.25     1997 Phone
#> 5 <NA>              NA     0.75     1997 Phone
#> 6 <NA>               1     1        1997 Phone
#Summarize
summary(as_factor(ces))
#>            vote        gay_rights        trad1          election   
#>  Other       : 113   Min.   :0.000   Min.   :0.000   Min.   :1997  
#>  Liberal     :2335   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:1997  
#>  Conservative:2759   Median :0.250   Median :0.250   Median :2000  
#>  NDP         : 992   Mean   :0.468   Mean   :0.444   Mean   :2002  
#>  Bloc        : 777   3rd Qu.:1.000   3rd Qu.:0.750   3rd Qu.:2008  
#>  Green       : 187   Max.   :1.000   Max.   :1.000   Max.   :2008  
#>  NA's        :6450   NA's   :5025    NA's   :2694    NA's   :2324  
#>      mode          
#>  Length:13613      
#>  Class :character  
#>  Mode  :character  
#>                    
#>                    
#>                    
#> 
```

# Future recodes

Users are also welcome to contribute to the package’s development by
adding new recodes in the recode scripts. The next section shows how to
do that:

## Adding recoded variables

1.  Users must clone the package’s GitHub repository to their desktop.
    Instructions on how to do this can be found
    [here](https://happygitwithr.com/existing-github-first),
    particularly Section 16.2.2.

2.  That done, open the RStudio project in the local directory you have
    just created on your computer which is a clone of the existing
    package repository.

3.  Open the recode script for the data-set you want to work on. Here,
    you will need to navigate to the folder `data-raw/recode_scripts/`
    and select which one you want. In this way you will be able to
    execute, see and diagnose recode commands in the local environment.

4.  Once complete, you will need to `source()` the recode script from
    beginning to end. This is important for a few reasons. First,
    running `source()` is *noticeably* faster than executing code by
    selecting it in RStudio and running `cmd-Enter`. We have been doing
    this and it can take forever.

Second, and more importantly, you will see that the recode script always
starts by loading the original raw data file and then saving an `.rda`
file out into the folder `data`. *This* is the file that is available to
users when they install and load `cesdata2`, *not* the raw data file.

6.  After sourcing all `recode` scripts, it is necessary to re-install
    the package. Once you have done this, you can: 1) exit the
    package, 2) Open a new RScript for analysis (which should be an
    RStudio project) 3) call `library(cesdata2)` and your recoded
    variables should be available to you.

7.  To share with other users, which is highly desirable, *re-open the
    `cesdata2`* project on your computer, make a new Github branch,
    using an informative title, commit these changes to the GitHub
    repository and push them to that new branch, *not* the main branch.

The package is currently set up to permit only Simon Kiss and Matt
Polacko to contribute to the main branch. All others must commit to
other branches. Simon Kiss, package maintainer, will merge branches to
the main branch and communicate when that has happened.

# Outstanding Issues

1.  The 2015 Web survey is currently included in the package, but there
    have been almost no variables recoded.

2.  Value labels

Right now, all the original CES files are imported via the `haven`
package which creates this very awkward `labelled` class variable
instead of standard R factors. In hindsight this was a mistake. It would
be better to go back and convert those variables as value labels
straight to factors.

Care has been taken to ensure value labels are consistent throughout,
however, the user is strongly advised to very quickly after making `ces`
to use `as_factor()` to convert `labelled` variables to factors.

Care has also been taken to *not* assign value labels to truly numeric
or continuous variables. They should show up as class `double`. This
does mean that it may be difficult to know exactly what `1` and `0`
means in those cases. The user is encouraged to look at the naming
conventions in the section above, to quickly crosstab recoded with
original variables in the underlying dataframe or to examine the recode
scripts in the `cesdata2` package themselves.

## Template For Constructing A CES

We provide here a template R code that will get the user up and running
quickly. This can be copied and pasted into an R Script and should work
as long as `cesdata2` is installed from the GitHub repository.

Note: this script belongs in an *analysis* project, *not* in the
`cesdata2` project.

``` r
library(purrr)
library(tidyverse)
#Install cesdata2
#devtools::install_github("sjkiss/github")
library(cesdata2)
#Seprate ces79 and ces80 to two separate files
ces7980 %>% 
  filter(V4002==1)->ces79

ces7980 %>% 
  filter(V4008==1)->ces80
# Drop the `election` variable from ces80
ces80 %>% 
  select(-election)->ces80
#Remove the '80' from the duplicate ces80 variables
# in the ces780
names(ces80)<-str_remove_all(names(ces80), "80")

tail(names(ces80))
#> [1] "ndp_rating" "turnout"    "foreign"    "mip"        "mode"      
#> [6] "election"

### Decide On CES 1993 sample
# this command includes only those respondents that completed the 1993 Campaign and Post-Election Survey
# ces93[!is.na(ces93$RTYPE4), ] -> ces93

  #Use Panel Respondents for 2004
ces0411 %>%
 filter(str_detect(ces0411$survey, "PES04"))->ces04
table(ces0411$survey)
#> 
#>                                                                     CPS04 
#>                                                                      1182 
#>                                                               CPS04 PES04 
#>                                                                       671 
#>                                                      CPS04 PES04 CPS06  * 
#>                                                                       107 
#>                                                   CPS04 PES04 CPS06 PES06 
#>                                                                       207 
#>                                                         CPS04 PES04 MBS04 
#>                                                                       471 
#>                                                   CPS04 PES04 MBS04 CPS06 
#>                                                                        71 
#>                                             CPS04 PES04 MBS04 CPS06 PES06 
#>                                                                       229 
#>                                                                     CPS06 
#>                                                                       493 
#>                                                               CPS06 PES06 
#>                                                                      1566 
#>                                                                     CPS08 
#>                                                                       806 
#>                                                               CPS08 PES08 
#>                                                                      1233 
#>                                                         CPS08 PES08 MBS08 
#>                                                                      1218 
#>                                 Panel - CPS04 PES04 CPS06 PES06 PES08  ** 
#>                                                                       109 
#>                               Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 
#>                                                                        58 
#>                                           Panel - CPS04 PES04 CPS06 PES08 
#>                                                                        29 
#>                                     Panel - CPS04 PES04 CPS06 PES08 MBS08 
#>                                                                         7 
#>                               Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 
#>                                                                        99 
#>                         Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 
#>                                                                       204 
#>                                     Panel - CPS04 PES04 MBS04 CPS06 PES08 
#>                                                                        10 
#>                               Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 
#>                                                                        19 
#>                                                        New RDD_2011 CPS11 
#>                                                                       863 
#>                                                   CPS04 PES04 CPS06 CPS11 
#>                                                                         6 
#>                                             CPS04 PES04 CPS06 PES06 CPS11 
#>                                                                        13 
#>                                             CPS04 PES04 MBS04 CPS06 CPS11 
#>                                                                         6 
#>                                       CPS04 PES04 MBS04 CPS06 PES06 CPS11 
#>                                                                        12 
#>                               Panel - CPS04 PES04 CPS06 PES06 PES08 CPS11 
#>                                                                        15 
#>                         Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 CPS11 
#>                                                                         4 
#>                                     Panel - CPS04 PES04 CPS06 PES08 CPS11 
#>                                                                         2 
#>                               Panel - CPS04 PES04 CPS06 PES08 MBS08 CPS11 
#>                                                                         0 
#>                         Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 CPS11 
#>                                                                         7 
#>                   Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 CPS11 
#>                                                                        16 
#>                               Panel - CPS04 PES04 MBS04 CPS06 PES08 CPS11 
#>                                                                         1 
#>                         Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 CPS11 
#>                                                                         1 
#>                                                  New RDD_2011 CPS11 PES11 
#>                                                                      1492 
#>                                             CPS04 PES04 CPS06 CPS11 PES11 
#>                                                                         6 
#>                                       CPS04 PES04 CPS06 PES06 CPS11 PES11 
#>                                                                        42 
#>                                       CPS04 PES04 MBS04 CPS06 CPS11 PES11 
#>                                                                         2 
#>                                 CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 
#>                                                                        20 
#>                         Panel - CPS04 PES04 CPS06 PES06 PES08 CPS11 PES11 
#>                                                                        88 
#>                   Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 CPS11 PES11 
#>                                                                        22 
#>                               Panel - CPS04 PES04 CPS06 PES08 CPS11 PES11 
#>                                                                         7 
#>                         Panel - CPS04 PES04 CPS06 PES08 MBS08 CPS11 PES11 
#>                                                                         2 
#>                   Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 CPS11 PES11 
#>                                                                        52 
#>             Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 CPS11 PES11 
#>                                                                        83 
#>                         Panel - CPS04 PES04 MBS04 CPS06 PES08 CPS11 PES11 
#>                                                                         7 
#>                   Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 CPS11 PES11 
#>                                                                         5 
#>                                            New RDD_2011 CPS11 PES11 MBS11 
#>                                                                       584 
#>                                       CPS04 PES04 CPS06 CPS11 PES11 MBS11 
#>                                                                         1 
#>                                 CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11 
#>                                                                         5 
#>                                 CPS04 PES04 MBS04 CPS06 CPS11 PES11 MBS11 
#>                                                                         2 
#>                           CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11 
#>                                                                        12 
#>                   Panel - CPS04 PES04 CPS06 PES06 PES08 CPS11 PES11 MBS11 
#>                                                                        13 
#>             Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 CPS11 PES11 MBS11 
#>                                                                        15 
#>                   Panel - CPS04 PES04 CPS06 PES08 MBS08 CPS11 PES11 MBS11 
#>                                                                         2 
#>             Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 CPS11 PES11 MBS11 
#>                                                                        48 
#>       Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 CPS11 PES11 MBS11 
#>                                                                       105 
#>                   Panel - CPS04 PES04 MBS04 CPS06 PES08 CPS11 PES11 MBS11 
#>                                                                         1 
#>             Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 CPS11 PES11 MBS11 
#>                                                                         3 
#>                                      New RDD_2011 CPS11 PES11 MBS11 WBS11 
#>                                                                       519 
#>                                 CPS04 PES04 CPS06 CPS11 PES11 MBS11 WBS11 
#>                                                                         2 
#>                           CPS04 PES04 CPS06 PES06 CPS11 PES11 MBS11 WBS11 
#>                                                                         3 
#>                           CPS04 PES04 MBS04 CPS06 CPS11 PES11 MBS11 WBS11 
#>                                                                         1 
#>                     CPS04 PES04 MBS04 CPS06 PES06 CPS11 PES11 MBS11 WBS11 
#>                                                                        14 
#>             Panel - CPS04 PES04 CPS06 PES06 PES08 CPS11 PES11 MBS11 WBS11 
#>                                                                         7 
#>       Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08 CPS11 PES11 MBS11 WBS11 
#>                                                                        20 
#>             Panel - CPS04 PES04 CPS06 PES08 MBS08 CPS11 PES11 MBS11 WBS11 
#>                                                                         4 
#>       Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 CPS11 PES11 MBS11 WBS11 
#>                                                                        20 
#> Panel - CPS04 PES04 MBS04 CPS06 PES06 PES08 MBS08 CPS11 PES11 MBS11 WBS11 
#>                                                                       141 
#>             Panel - CPS04 PES04 MBS04 CPS06 PES08 CPS11 PES11 MBS11 WBS11 
#>                                                                         2 
#>       Panel - CPS04 PES04 MBS04 CPS06 PES08 MBS08 CPS11 PES11 MBS11 WBS11 
#>                                                                        10
# Use Panel Respondets for 2006
ces0411 %>%
 filter(str_detect(ces0411$survey, "PES06"))->ces06
# Use Panel Respondents for 2008
ces0411 %>% 
  filter(str_detect(ces0411$survey, "PES08"))->ces08

#Use Panel respondents for 2011
ces0411 %>% 
  filter(str_detect(ces0411$survey, "PES11"))->ces11

#Rename variables in 2004
#Strip out any instance of `04` in the names of 04
names(ces04)<-str_remove_all(names(ces04), "04")

#Rename variables in 2006
#Strip out any instance of `06` in the names of 06
names(ces06)<-str_remove_all(names(ces06), "06")
#Rename variables in 2008
#Strip out any instance of `08` in the names of 08
names(ces08)<-str_remove_all(names(ces08), "08")

#Rename variables in 2011
#Strip out any instance of `11` in the names of 11
names(ces11)<-str_remove_all(names(ces11), "11")

# List data frames
ces.list<-list(ces65, ces68, ces72_nov, ces74, ces79, ces80, ces84, ces88, ces93, ces97, ces00, ces04, ces06, ces08, ces11, ces15phone, ces15web, ces19phone, ces19web, ces21)
#Provide names for list
names(ces.list)<-c(1965, 1968, 1972, 1974, 1979, 1980, 1984, 1988, 1993, 1997, 2000, 2004, 2006, 2008, 2011, "2015 Phone", "2015 Web","2019 Phone", "2019 Web", 2021)
#Common variables to be selected
#common_vars<-c('male')
common_vars<-c('male',
               'sector', 
               'occupation',
               'employment', 
               'union_both',
               'region', 'union',
               'degree', 
               'quebec', 
               'age', 
               'religion', 
               'vote', 
               'income',
               'redistribution',
               'market_liberalism', 
               'immigration_rates', 
               'traditionalism',
               'traditionalism2', 
               'trad1', 'trad2', 'immigration_rates',
               'market1','market2',
               'turnout', 'mip', 'occupation', 'occupation3', 'education', 'personal_retrospective', 'national_retrospective', 'vote3',
               'efficacy_external', 'efficacy_external2', 'efficacy_internal', 'political_efficacy', 'inequality', 'efficacy_rich', 'promise', 'trust', 'pol_interest', 'foreign',
               'non_charter_language', 'language', 'employment', 'satdem', 'satdem2', 'turnout', 'party_id', 'postgrad', 'income_tertile', 'income2', 'household', 'enviro', 'ideology', 'income_house', 'enviro_spend', 'mode', 'election')

ces.list %>% 
  map(., select, any_of(common_vars))%>%
  #bind_rows smushes all the data frames together, and creates a variable called election
  bind_rows()->ces
#show what we have.
glimpse(ces)
#> Rows: 122,079
#> Columns: 54
#> $ male                   <dbl+lbl> 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0…
#> $ occupation             <dbl+lbl>  5,  5,  5,  5,  5,  5,  1, NA,  2,  5, NA,…
#> $ employment             <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ union_both             <dbl+lbl> NA,  0,  0, NA, NA, NA,  0, NA,  0, NA, NA,…
#> $ region                 <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ union                  <dbl+lbl> NA,  0,  0, NA, NA, NA,  0, NA,  0, NA, NA,…
#> $ degree                 <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ quebec                 <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ age                    <dbl+lbl> 45, 36, 48, 32, 53, 33, 47, 75, 45, 62, 71,…
#> $ religion               <dbl+lbl> 2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2…
#> $ vote                   <dbl+lbl>  1,  1,  2,  1,  1,  1,  2,  2,  2,  1,  2,…
#> $ income                 <dbl+lbl>  1,  2,  2,  2,  3,  2,  3,  2,  3,  2,  1,…
#> $ turnout                <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ mip                    <dbl+lbl>  9,  0, 15, 15, 15,  6, 11,  9, 16, 15, 15,…
#> $ personal_retrospective <dbl> 0.5, 0.0, 0.0, 1.0, 1.0, 0.5, 1.0, 0.5, 1.0, 0.…
#> $ efficacy_external      <dbl> 0, 1, 1, 1, 1, 0, NA, 0, 1, 0, 0, NA, 0, 1, 1, …
#> $ efficacy_external2     <dbl> 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, NA, 0, 0, 1…
#> $ efficacy_internal      <dbl> 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1,…
#> $ political_efficacy     <dbl> 0.3333333, 0.6666667, 1.0000000, 0.6666667, 0.3…
#> $ foreign                <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0…
#> $ non_charter_language   <dbl+lbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ language               <dbl+lbl>  1,  1, NA,  1,  1,  1,  1,  1,  1,  1,  1,…
#> $ party_id               <dbl+lbl> 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 2, 2, 2, 1, 1…
#> $ income_tertile         <dbl+lbl>  1,  1,  1,  1,  2,  1,  2,  1,  2,  1,  1,…
#> $ income2                <dbl+lbl>  1,  1,  2,  1,  2,  2,  2,  1,  2,  1,  1,…
#> $ mode                   <chr> "Phone", "Phone", "Phone", "Phone", "Phone", "P…
#> $ election               <dbl> 1965, 1965, 1965, 1965, 1965, 1965, 1965, 1965,…
#> $ sector                 <dbl+lbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ occupation3            <dbl+lbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ ideology               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ redistribution         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ market_liberalism      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ immigration_rates      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ traditionalism         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ traditionalism2        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ trad1                  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ trad2                  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ market1                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ market2                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ education              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ national_retrospective <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ vote3                  <dbl+lbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ postgrad               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ enviro                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ pol_interest           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ satdem                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ satdem2                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ household              <dbl+lbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ income_house           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ promise                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ trust                  <dbl+lbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ enviro_spend           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ inequality             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ efficacy_rich          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# 
summary(ces)
#>       male         occupation      employment      union_both   
#>  Min.   :0.00    Min.   :1.00    Min.   :0.00    Min.   :0.0    
#>  1st Qu.:0.00    1st Qu.:1.00    1st Qu.:0.00    1st Qu.:0.0    
#>  Median :0.00    Median :3.00    Median :1.00    Median :0.0    
#>  Mean   :0.52    Mean   :2.76    Mean   :0.58    Mean   :0.3    
#>  3rd Qu.:1.00    3rd Qu.:4.00    3rd Qu.:1.00    3rd Qu.:1.0    
#>  Max.   :2.00    Max.   :5.00    Max.   :1.00    Max.   :1.0    
#>  NA's   :46645   NA's   :89056   NA's   :48858   NA's   :55026  
#>      region          union           degree          quebec     
#>  Min.   :1.00    Min.   :0.00    Min.   :0.0     Min.   :0.0    
#>  1st Qu.:2.00    1st Qu.:0.00    1st Qu.:0.0     1st Qu.:0.0    
#>  Median :2.00    Median :0.00    Median :0.0     Median :0.0    
#>  Mean   :2.26    Mean   :0.27    Mean   :0.3     Mean   :0.3    
#>  3rd Qu.:3.00    3rd Qu.:1.00    3rd Qu.:1.0     3rd Qu.:1.0    
#>  Max.   :3.00    Max.   :1.00    Max.   :1.0     Max.   :1.0    
#>  NA's   :66818   NA's   :59548   NA's   :48787   NA's   :54580  
#>       age            religion          vote           income     
#>  Min.   :  13.0   Min.   :0.00    Min.   :0.00    Min.   :1.00   
#>  1st Qu.:  36.0   1st Qu.:1.00    1st Qu.:1.00    1st Qu.:2.00   
#>  Median :  51.0   Median :1.00    Median :2.00    Median :3.00   
#>  Mean   : 215.9   Mean   :1.24    Mean   :2.01    Mean   :2.98   
#>  3rd Qu.:  67.0   3rd Qu.:2.00    3rd Qu.:3.00    3rd Qu.:4.00   
#>  Max.   :1997.0   Max.   :3.00    Max.   :6.00    Max.   :5.00   
#>  NA's   :40599    NA's   :49957   NA's   :70501   NA's   :51887  
#>     turnout           mip        personal_retrospective efficacy_external
#>  Min.   :0.00    Min.   : 0.00   Min.   :0.00           Min.   :0.00     
#>  1st Qu.:1.00    1st Qu.: 6.00   1st Qu.:0.00           1st Qu.:0.00     
#>  Median :1.00    Median : 7.00   Median :0.50           Median :0.25     
#>  Mean   :0.88    Mean   : 7.75   Mean   :0.48           Mean   :0.39     
#>  3rd Qu.:1.00    3rd Qu.:10.00   3rd Qu.:0.50           3rd Qu.:0.75     
#>  Max.   :1.00    Max.   :17.00   Max.   :1.00           Max.   :1.00     
#>  NA's   :59660   NA's   :80514   NA's   :58454          NA's   :78898    
#>  efficacy_external2 efficacy_internal political_efficacy    foreign     
#>  Min.   :0.00       Min.   :0.00      Min.   :0.000      Min.   :0.00   
#>  1st Qu.:0.00       1st Qu.:0.25      1st Qu.:0.250      1st Qu.:0.00   
#>  Median :0.25       Median :0.25      Median :0.375      Median :0.00   
#>  Mean   :0.42       Mean   :0.44      Mean   :0.422      Mean   :0.14   
#>  3rd Qu.:0.75       3rd Qu.:0.75      3rd Qu.:0.750      3rd Qu.:0.00   
#>  Max.   :1.00       Max.   :1.00      Max.   :1.000      Max.   :1.00   
#>  NA's   :58477      NA's   :36579     NA's   :24184      NA's   :53574  
#>  non_charter_language    language        party_id     income_tertile 
#>  Min.   :0.00         Min.   :0.00    Min.   :0.00    Min.   :1.00   
#>  1st Qu.:0.00         1st Qu.:1.00    1st Qu.:1.00    1st Qu.:1.00   
#>  Median :0.00         Median :1.00    Median :1.00    Median :2.00   
#>  Mean   :0.29         Mean   :0.75    Mean   :1.59    Mean   :2.01   
#>  3rd Qu.:1.00         3rd Qu.:1.00    3rd Qu.:2.00    3rd Qu.:3.00   
#>  Max.   :1.00         Max.   :1.00    Max.   :5.00    Max.   :3.00   
#>  NA's   :54883        NA's   :48879   NA's   :67043   NA's   :73700  
#>     income2          mode              election        sector     
#>  Min.   :1.00    Length:122079      Min.   :1965   Min.   :0.0    
#>  1st Qu.:2.00    Class :character   1st Qu.:2000   1st Qu.:0.0    
#>  Median :3.00    Mode  :character   Median :2019   Median :0.0    
#>  Mean   :2.97                       Mean   :2008   Mean   :0.2    
#>  3rd Qu.:4.00                       3rd Qu.:2019   3rd Qu.:0.0    
#>  Max.   :5.00                       Max.   :2021   Max.   :1.0    
#>  NA's   :73932                                     NA's   :77052  
#>   occupation3        ideology     redistribution  market_liberalism
#>  Min.   :1.0      Min.   :0.00    Min.   :0.00    Min.   :0.00     
#>  1st Qu.:2.0      1st Qu.:0.38    1st Qu.:0.75    1st Qu.:0.25     
#>  Median :3.0      Median :0.50    Median :0.75    Median :0.50     
#>  Mean   :3.5      Mean   :0.52    Mean   :0.78    Mean   :0.48     
#>  3rd Qu.:5.0      3rd Qu.:0.70    3rd Qu.:1.00    3rd Qu.:0.75     
#>  Max.   :6.0      Max.   :1.00    Max.   :1.00    Max.   :1.00     
#>  NA's   :104163   NA's   :82728   NA's   :77479   NA's   :74342    
#>  immigration_rates traditionalism  traditionalism2     trad1      
#>  Min.   :0.00      Min.   :0.00    Min.   :0.00    Min.   :0.00   
#>  1st Qu.:0.50      1st Qu.:0.17    1st Qu.:0.12    1st Qu.:0.00   
#>  Median :0.50      Median :0.33    Median :0.38    Median :0.25   
#>  Mean   :0.56      Mean   :0.36    Mean   :0.39    Mean   :0.35   
#>  3rd Qu.:1.00      3rd Qu.:0.50    3rd Qu.:0.50    3rd Qu.:0.50   
#>  Max.   :1.00      Max.   :1.00    Max.   :1.00    Max.   :1.00   
#>  NA's   :65036     NA's   :72104   NA's   :72549   NA's   :73119  
#>      trad2          market1         market2        education    
#>  Min.   :0.00    Min.   :0.00    Min.   :0.00    Min.   :0.00   
#>  1st Qu.:0.00    1st Qu.:0.25    1st Qu.:0.25    1st Qu.:0.50   
#>  Median :0.50    Median :0.25    Median :0.75    Median :0.50   
#>  Mean   :0.44    Mean   :0.41    Mean   :0.57    Mean   :0.64   
#>  3rd Qu.:0.75    3rd Qu.:0.75    3rd Qu.:0.75    3rd Qu.:1.00   
#>  Max.   :1.00    Max.   :1.00    Max.   :1.00    Max.   :1.00   
#>  NA's   :77442   NA's   :75387   NA's   :77539   NA's   :66993  
#>  national_retrospective     vote3           postgrad         enviro     
#>  Min.   :0.00           Min.   :0.00     Min.   :0.00    Min.   :0.00   
#>  1st Qu.:0.00           1st Qu.:1.00     1st Qu.:0.00    1st Qu.:0.25   
#>  Median :0.50           Median :2.00     Median :0.00    Median :0.50   
#>  Mean   :0.39           Mean   :2.61     Mean   :0.11    Mean   :0.59   
#>  3rd Qu.:0.50           3rd Qu.:4.00     3rd Qu.:0.00    3rd Qu.:1.00   
#>  Max.   :1.00           Max.   :6.00     Max.   :1.00    Max.   :1.00   
#>  NA's   :63724          NA's   :112878   NA's   :65140   NA's   :80772  
#>   pol_interest       satdem         satdem2        household    
#>  Min.   :0.00    Min.   :0.00    Min.   :0.00    Min.   :0.00   
#>  1st Qu.:0.50    1st Qu.:0.25    1st Qu.:0.25    1st Qu.:0.50   
#>  Median :0.70    Median :0.75    Median :0.75    Median :1.00   
#>  Mean   :0.62    Mean   :0.60    Mean   :0.60    Mean   :0.98   
#>  3rd Qu.:0.80    3rd Qu.:0.75    3rd Qu.:0.75    3rd Qu.:1.00   
#>  Max.   :1.00    Max.   :1.00    Max.   :1.00    Max.   :7.50   
#>  NA's   :70829   NA's   :77937   NA's   :68791   NA's   :89198  
#>   income_house      promise          trust        enviro_spend  
#>  Min.   :1.00    Min.   :0.00    Min.   :0.00    Min.   :0.00   
#>  1st Qu.:1.00    1st Qu.:0.50    1st Qu.:0.00    1st Qu.:0.50   
#>  Median :2.00    Median :0.50    Median :0.00    Median :1.00   
#>  Mean   :2.09    Mean   :0.67    Mean   :0.47    Mean   :0.77   
#>  3rd Qu.:3.00    3rd Qu.:1.00    3rd Qu.:1.00    3rd Qu.:1.00   
#>  Max.   :3.00    Max.   :1.00    Max.   :1.00    Max.   :1.00   
#>  NA's   :92814   NA's   :72421   NA's   :81254   NA's   :79448  
#>    inequality     efficacy_rich  
#>  Min.   :0.00     Min.   :0.00   
#>  1st Qu.:0.50     1st Qu.:0.25   
#>  Median :0.75     Median :0.25   
#>  Mean   :0.74     Mean   :0.39   
#>  3rd Qu.:1.00     3rd Qu.:0.50   
#>  Max.   :1.00     Max.   :1.00   
#>  NA's   :103127   NA's   :92908
```

## Credit

As this R package remains a private, by invitation only repository,
there is no way to cite it. Instead, collaborators invited to
participate are requested to cite one of the papers in the reference
list below as recognition of the work that has gone into this. This
package was instrumental in both publications.

## References

Polacko, Matthew, Simon Kiss, and Peter Graefe. “The Changing Nature of
Class Voting in Canada, 1965-2019.” Canadian Journal of Political
Science/Revue Canadienne de Science Politique 55, no. 3 (September 1,
2022): 1–24.

Kiss, Simon, Peter Graefe, and Matt Polacko. “The Evolving
Education-Income Cleavage In Canada.” Electoral Studies, 2023.
