
- [Introduction](#introduction)
  - [Installation](#installation)
- [How to use it.](#how-to-use-it)
- [Package Structure](#package-structure)
  - [Recoding conventions](#recoding-conventions)
- [Establishing A Usable Time
  Series](#establishing-a-usable-time-series)
  - [Issues and Special Cases](#issues-and-special-cases)
- [Future recodes](#future-recodes)
  - [Adding recoded variables](#adding-recoded-variables)
- [Outstanding Issues](#outstanding-issues)
  - [Template For Constructing A CES](#template-for-constructing-a-ces)
- [Credit](#credit)
- [References](#references)

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

# How to use it.

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
```

``` r

#As is CES1965

ces65 %>% 
  select(1:5) %>% 
  glimpse()
#> Rows: 2,118
#> Columns: 5
#> $ v1 <dbl> 7225, 7225, 7225, 7225, 7225, 7225, 7225, 7225, 7225, 7225, 7225, 7…
#> $ v2 <dbl> 1, 2, 3, 4, 5, 6, 8, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22…
#> $ v3 <dbl+lbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, …
#> $ v4 <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ v5 <dbl+lbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

These are the datasets currently available with the names with which
they can be called.

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
#>  [5] "2021_mip_unique.csv"                                 
#>  [6] "2021_occupations_coded_government.xlsx"              
#>  [7] "2021_occupations_coded.xlsx"                         
#>  [8] "CES_04060811_ISR_revised.sav"                        
#>  [9] "CES-E-1972-jun-july_F1.sav"                          
#> [10] "CES-E-1972-nov_F1.sav"                               
#> [11] "CES-E-1972-sept_F1.sav"                              
#> [12] "CES-E-1974_F1.sav"                                   
#> [13] "CES-E-1993_F1.sav"                                   
#> [14] "CES-E-1997_F1.sav"                                   
#> [15] "CES-E-2000_F1.sav"                                   
#> [16] "CES-E-2019-online_F1.sav"                            
#> [17] "CES15_CPS+PES_Web_SSI Full.dta"                      
#> [18] "ces1965.dta"                                         
#> [19] "ces1968.dta"                                         
#> [20] "CES1988.sav"                                         
#> [21] "CES2015_CPS-PES-MBS_complete.sav"                    
#> [22] "ces21.dta"                                           
#> [23] "mip_2019.xlsx"                                       
#> [24] "NOC_2021_4_job_titles.xlsx"                          
#> [25] "NOC_2021_5_job_titles.xlsx"                          
#> [26] "recode_scripts"
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

| Concept                                                                    |     Variable Name |                                                                    notes                                                                     |
|:---------------------------------------------------------------------------|------------------:|:--------------------------------------------------------------------------------------------------------------------------------------------:|
| Demographics                                                               |                   |                                                                                                                                              |
| Public Sector Employee                                                     |          `sector` |                                Dichotomous variable distinguishes public sector from private sector employees                                |
| Public Sector Health Welfare and Education                                 |  `sector_welfare` |  Binary variable distinguishing public sector health, welfare and education employees from all other employees; only available from 2006 on  |
| Security                                                                   | `sector_security` | Binary variable distinguishing security (police, forces, security guards, corrections) from all other employees; only available from 2006 on |
| gender                                                                     |            `male` |                                               Binary variable distinguishing males from others                                               |
| education                                                                  |          `degree` |                                            Binary variable distinguishing BA holders from others                                             |
| Political                                                                  |                   |                                                                                                                                              |
| Vote Choice                                                                |            `vote` |                   respondent vote choice from PES: groups reform, PCs, CA and Conservatives, includes BQ, Green and Other                    |
| Vote Choice                                                                |           `vote3` |                                                   respondent vote choice separates out PPC                                                   |
| Previous vote                                                              |   `previous_vote` |                                          respondent previous recalled vote, no split conservatives                                           |
| Previous vote                                                              |  `previous_vote3` |                                               respondent previous recalled vote, splitting PPC                                               |
| Ideology                                                                   |                   |                                                                                                                                              |
| These have normally been coded to run from 0 to 1, liberal to conservative |                   |                                                                                                                                              |
| Gay Rights                                                                 |      `gay_rights` |                                           Gay couples should be allowed to marry, usually from MBS                                           |

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

#### A variable is not in one of the datasets.

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
```

``` r
#Check to see if ces88 gay_rights exists
length(ces88$gay_rights) #3609 cases of gay_rights values
#> [1] 3609
```

``` r
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

#### Panel datasets

1.  1974-1979-1980

There is a complete 1974-1979-1980 Canada Election Study file and it is
loaded into `cesdata2` as `ces7980`. However, we have also loaded the
original 1974 file into `cesdata2` as `ces74`. Effectively we did not
make use of the panel feature for 1974-1979. There are respondents in
`ces7980` that did participate in the 1974 study. Users can track them
down using the variables contained in `ces7980`, but we haven’t down
that.

The user can search through `ces7980` in this way to filter respondents
as they wish in the master file that is necessary to make their CES
data-set.

``` r
lookfor(ces7980, "FILTER")
#>  pos  variable label              col_type missing values          
#>  487  V1007    1979 FILTER:1      dbl      0                       
#>  1023 V2005    1980 FILTER:1      dbl      0                       
#>  1221 V4002    1979 FILTER:1      dbl      0                       
#>  1227 V4008    1980 FILTER:1      dbl      0                       
#>  1233 V4014    P74-79-80 FILTER:1 dbl      0                       
#>  1239 V4020    P74-79 FILTER:1    dbl      0                       
#>  1245 V4026    P79-80 FILTER:1    dbl      0                       
#>  1251 V4032    P74-80 FILTER:1    dbl      0                       
#>  1261 filter_$ V4008=1 (FILTER)   dbl+lbl  0       [0] Not Selected
#>                                                    [1] Selected
```

It is possible though that the recode script may need to be modified in
order to make potential use of this feature. Good luck.

In addition, MATT CAN YOU EXPLAIN WHAT HAPPENED WITH 1980

2.  1992 Referendum and 1993 Election Survey

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

3.  2004-2011

The CES 2004-2011 is a very large file and presents some significant
challenges given our renaming and recoding strategy. However, we have
come up with a method that is expansive in that recodes are run on every
respondent and can then be efficiently filtered to select respondent
values for specific sets of surveys.

The underlying data files look like this the following table, for *each*
respondent, there are variables for the 2004, 2006, and 2008 surveys
*whether or not* the respondent actually completed the surveys.

| Respondent ID |    Survey     | `ces04_PES_K5A` |     `ces06_PES_B4A`     | `ces08_PES_B4B` |
|:-------------:|:-------------:|:---------------:|:-----------------------:|:---------------:|
|       1       |     2004      |    `Refused`    |           NA            |       NA        |
|       2       |     2006      |       NA        | Liberal Party of Canada |       NA        |
|       3       |     2008      |       NA        |           NA            |       NDP       |
|       4       | 2004 and 2006 |      Other      | Liberal Party of Canada |       NA        |
|       5       | 2006 and 2008 |       NA        |                         |                 |

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

``` r

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
```

There are multiple ways to split the file. You can split it very
precisely and narrowly. For example, one can pick only the respondents
who completed the CPS04 survey by using `filter(survey=="CPS04")`.

Or you can select any respondent who filled out the CPS04 survey amongst
any other surveys by combining `filter` with `str_detect` and a regular
expression.

Compare the two commands, below. Note, to diagnose the accuracy of your
regular expression and filter strategy, you can first `select()` only
the `survey` variable and count what your filter strategy produces, and
then actually filter into a new data frame.

This filter command will *only* include respondents who *only* completed
the CPS from 2004.

``` r
ces0411 %>% 
  select(survey) %>% 
  filter(survey=="CPS04") %>% 
  count(survey) 
#> # A tibble: 1 × 2
#>   survey     n
#>   <fct>  <int>
#> 1 CPS04   1182
```

This filter command will include respondents who *only* completed the
CPS from 2004, who only completed the CPS 04 *and* the PES04 as well as
the respondents who completed the CPS04, the PES04 and the MBS04.

``` r
ces0411 %>% 
  select(survey) %>% 
  #This filter command will only include respondents
  # with the *exact* values specified
  filter(survey=="CPS04"|survey=="CPS04 PES04"|survey=="CPS04 PES04 MBS04") %>% 
  count(survey) 
#> # A tibble: 3 × 2
#>   survey                n
#>   <fct>             <int>
#> 1 CPS04              1182
#> 2 CPS04 PES04         671
#> 3 CPS04 PES04 MBS04   471
```

This command will include *any* respondent who filled out the CPS04.
This includes respondents who count as panel respondents who who were
reinterviewed in later surveys.

``` r
ces0411 %>% 
  select(survey) %>% 
  filter(str_detect(survey, "CPS04")) %>% 
  count(survey) 
#> # A tibble: 60 × 2
#>    survey                                          n
#>    <fct>                                       <int>
#>  1 CPS04                                        1182
#>  2 CPS04 PES04                                   671
#>  3 CPS04 PES04 CPS06  *                          107
#>  4 CPS04 PES04 CPS06 PES06                       207
#>  5 CPS04 PES04 MBS04                             471
#>  6 CPS04 PES04 MBS04 CPS06                        71
#>  7 CPS04 PES04 MBS04 CPS06 PES06                 229
#>  8 Panel - CPS04 PES04 CPS06 PES06 PES08  **     109
#>  9 Panel - CPS04 PES04 CPS06 PES06 PES08 MBS08    58
#> 10 Panel - CPS04 PES04 CPS06 PES08                29
#> # ℹ 50 more rows
```

Such a strategy might be desirable to increase sample size, but it might
be undesirable to include panel respondents, who, after having been
interviewed multiple times, may be demonstrate different responses than
other purely cross-sectional respondents. *We leave this to the user.*

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
```

``` r
nrow(ces04)
#> [1] 2324
```

3.  Renaming re-coded variables in `ces04`, `ces06`, `ces08` and `ces11`
    data frames.

We executed the recode scripts on the merged `ces0411` dataset. This is
a giant tabular dataset that includes variables for each election for
each respondent, *even if the respondent did not participate in the
survey for that variable*. For example, respondents who *only*
participated in the CPS04, will still have values for the *all*
variables, including variables in the 2006, 2008 and 2011 datasets;
their responses will simply be `NA`.

This shows the problem:

``` r
ces0411 %>% 
  filter(str_detect(survey, "PES08")) %>% 
select(survey, contains('vote')) %>% 
  as_factor()
#> # A tibble: 3,689 × 11
#>    survey    ces08_PES_PROV_VOTE1 vote04 previous_vote04 previous_vote043 vote06
#>    <fct>     <fct>                <fct>  <fct>           <fct>            <fct> 
#>  1 Panel - … NDP (New Democrats)  Green  Liberal         Liberal          NDP   
#>  2 Panel - … NDP (New Democrats)  Liber… Liberal         Liberal          Liber…
#>  3 Panel - … R volunteers: It de… <NA>   Conservative    Conservative     Liber…
#>  4 Panel - … NDP (New Democrats)  Liber… Liberal         Liberal          Liber…
#>  5 Panel - … Other                Bloc   Bloc            Bloc             NDP   
#>  6 Panel - … L'Action Democratiq… Bloc   Bloc            Bloc             NDP   
#>  7 Panel - … refused              <NA>   Bloc            Bloc             Conse…
#>  8 Panel - … Liberal (Grits)      Green  NDP             NDP              Green 
#>  9 Panel - … Liberal (Grits)      Bloc   Bloc            Bloc             NDP   
#> 10 Panel - … Liberal (Grits)      Liber… Liberal         Liberal          Conse…
#> # ℹ 3,679 more rows
#> # ℹ 5 more variables: previous_vote06 <fct>, vote08 <fct>,
#> #   previous_vote08 <fct>, vote11 <fct>, previous_vote11 <fct>
```

The problem is that in other, non-panel datasets, there is no need to
indicate the year that it came from.

For example, in the 2000 dataset, `vote` is just that.

``` r
ces00 %>% 
  select(election, contains('vote'))
#> # A tibble: 3,651 × 5
#>    election vote              vote3             previous_vote     previous_vote3
#>       <dbl> <dbl+lbl>         <dbl+lbl>         <dbl+lbl>         <dbl+lbl>     
#>  1     2000 NA                NA                NA                NA            
#>  2     2000  2 [Conservative]  2 [Conservative]  2 [Conservative]  2 [Conservat…
#>  3     2000 NA                NA                NA                NA            
#>  4     2000 NA                NA                 2 [Conservative]  2 [Conservat…
#>  5     2000  2 [Conservative]  2 [Conservative] NA                NA            
#>  6     2000 NA                NA                NA                NA            
#>  7     2000  2 [Conservative]  2 [Conservative]  2 [Conservative]  2 [Conservat…
#>  8     2000  0 [Other]         0 [Other]        NA                NA            
#>  9     2000  1 [Liberal]       1 [Liberal]       1 [Liberal]       1 [Liberal]  
#> 10     2000  2 [Conservative]  2 [Conservative]  3 [NDP]           3 [NDP]      
#> # ℹ 3,641 more rows
```

It would be impossible to combine say, the 2000, 2004 and 2008 datasets
because 2000 contains the variable `vote`, respondents from the 2004 and
2008 datasets would have `vote04`, `vote06`, `vote08` and `vote11`.

The answer is transforming the variable names in data frames into the
consistent names used in all the other datasets to facilitate binding
together.

Previously, we had a fairly laborious method of individually renaming
variables such as `vote04` to be `vote`. There is a cleaner and quicker
way to accomplish this. Specifically, for each dataframe that comes from
the `ces0411` combined file, we just strip out any instance of `04` in
the `ces04` data frame, `06` in the `ces06` dataframe.

You can see the effects of this below:

``` r
# make a ces04 data frame
ces0411 %>% 
  filter(str_detect(survey, "PES04"))->ces04
# Make a ces08 data frame
ces0411 %>% 
  filter(str_detect(survey, "PES08"))->ces08
#Take only the ces04 data frame
ces04 %>% 
  #Select variables containing vote and trad
  select(contains('vote'), contains('trad')) %>% 
  #Show the names of the variables
  names()
#>  [1] "ces08_PES_PROV_VOTE1" "vote04"               "previous_vote04"     
#>  [4] "previous_vote043"     "vote06"               "previous_vote06"     
#>  [7] "vote08"               "previous_vote08"      "vote11"              
#> [10] "previous_vote11"      "trad043"              "trad047"             
#> [13] "trad041"              "trad044"              "trad045"             
#> [16] "trad046"              "trad042"              "traditionalism04"    
#> [19] "traditionalism204"    "trad063"              "trad062"             
#> [22] "trad061"              "traditionalism06"     "traditionalism206"   
#> [25] "trad083"              "trad087"              "trad081"             
#> [28] "trad084"              "trad085"              "trad086"             
#> [31] "trad082"              "traditionalism08"     "traditionalism208"   
#> [34] "trad113"              "trad117"              "trad111"             
#> [37] "trad114"              "trad115"              "trad116"             
#> [40] "trad112"              "traditionalism11"     "traditionalism211"
```

Here it is apparent that there are consistent variables for `04`, `06`
and `08` even in the data set that contains only respondents that
participated in the `04` survey.

But if we strip out `04` from the variable names we are left with.

``` r
#Take only the ces04 data frame
ces04 %>% 
  #Select variables containing vote and trad
  select(contains('vote'), contains('trad')) %>% 
  #Show the names of the variables
  names() %>% 
  str_remove_all(., "04")
#>  [1] "ces08_PES_PROV_VOTE1" "vote"                 "previous_vote"       
#>  [4] "previous_vote3"       "vote06"               "previous_vote06"     
#>  [7] "vote08"               "previous_vote08"      "vote11"              
#> [10] "previous_vote11"      "trad3"                "trad7"               
#> [13] "trad1"                "trad4"                "trad5"               
#> [16] "trad6"                "trad2"                "traditionalism"      
#> [19] "traditionalism2"      "trad063"              "trad062"             
#> [22] "trad061"              "traditionalism06"     "traditionalism206"   
#> [25] "trad083"              "trad087"              "trad081"             
#> [28] "trad084"              "trad085"              "trad086"             
#> [31] "trad082"              "traditionalism08"     "traditionalism208"   
#> [34] "trad113"              "trad117"              "trad111"             
#> [37] "trad114"              "trad115"              "trad116"             
#> [40] "trad112"              "traditionalism11"     "traditionalism211"
```

Now, for example, in the `ces04` data frame, `vote04` has become `vote`,
`trad041` has become `trad1`, etc. But `vote06`and `trad06` remain
untouched.

We make the permanent changes to the data-sets in the following way:

``` r
#Rename variables in 2004
#Strip out any instance of `08` in the names of 08
names(ces04)<-str_remove_all(names(ces04), "04")
#Rename variables in 2008
#Strip out any instance of `08` in the names of 08
names(ces08)<-str_remove_all(names(ces08), "08")
```

And now, we could proceed, as noted above, creating a unified tabular
ces dataset from with other, more simple, one-election datafiles.

``` r
#List the data frames to be used.
ces.list<-list(ces00, ces04, ces08)

#Provide names
names(ces.list)<-c(2000, 2004, 2008)
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
#> 1 <NA>               0.25  0        2000 Phone
#> 2 Conservative       0.25  0        2000 Phone
#> 3 <NA>               0.25  0.25     2000 Phone
#> 4 <NA>               0.75  0.25     2000 Phone
#> 5 Conservative       0.25  0.75     2000 Phone
#> 6 <NA>               0     0.25     2000 Phone
```

``` r
#Summarize
summary(as_factor(ces))
#>            vote        gay_rights         trad1           election   
#>  Other       :  85   Min.   :0.0000   Min.   :0.0000   Min.   :2000  
#>  Liberal     :2353   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:2000  
#>  Conservative:2623   Median :0.2500   Median :0.2500   Median :2004  
#>  NDP         :1113   Mean   :0.4451   Mean   :0.4319   Mean   :2004  
#>  Bloc        : 781   3rd Qu.:1.0000   3rd Qu.:0.7500   3rd Qu.:2008  
#>  Green       : 290   Max.   :1.0000   Max.   :1.0000   Max.   :2008  
#>  NA's        :3236   NA's   :472      NA's   :224                    
#>      mode          
#>  Length:10481      
#>  Class :character  
#>  Mode  :character  
#>                    
#>                    
#>                    
#> 
```

#### Actual panel analysis

Note, that the ces0411 data file lends itself to amazing panel analysis.
However, I have not even begun to think about how the variables would
have to be renamed in orer to construct a proper dataset. It is cetainly
possible, and probably quite easy, I just haven’t done it.

# Future recodes

Users are also welcome to contribute to the package’s development by
adding new recodes in the recode scripts. The next section shows how to
do that.

## Adding recoded variables

1.  Users must clone the package’s GitHub repository to their desktop.
    Instructions on how to do this can be found
    [here](https://happygitwithr.com/existing-github-first),
    particularly Section 16.2.2.

Please note, it is essential that users be able to distinguish between
the copy of the package that has been *cloned* to your hard drive and
the copy of the package that has actually been *installed* for use. They
are in two separate locations.

To demonstrate, you can call `.libpaths()` to find out where your
package has been installed.

``` r
.libPaths()
#> [1] "/Users/skiss/Library/R/x86_64/4.4/library"                            
#> [2] "/Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library"
```

You can open your Finder, or your PC equivalent and actually see it.

<div class="figure">

<img src="man/figures/path.png" alt="Figure 1: Location of installed version of cesdata2" width="100%" />
<p class="caption">
Figure 1: Location of installed version of cesdata2
</p>

</div>

However, the cloned version from GitHub can be stored anywhere you want
on your desktop, somewhere useful and easily accessible for the user.

<img src="man/figures/finder_path.png" width="100%" />

While this may be confusing, it is fairly simple.

    1. Using `cesdata2` for analyses draws on the version of the package installed deep in your hard-drive (Figure 1). 

    2. Opening `cesdata2` to inspect or modify code is done using the cloned copy stored where you want it to be (Figure 2). 

2.  That done, open the RStudio project in the local directory you have
    just created on your computer which is a clone of the existing
    package repository.

3.  Open the recode script for the data-set you want to work on. Here,
    you will need to navigate to the folder `data-raw/recode_scripts/`
    and select which one you want. In this way you will be able to see,
    diagnose, modify and execute recode commands in the local
    environment.

4.  Open the recode script for the file you want to work on, navigate to
    the bottom, *before* the command `save('filename_.rda')` and add
    your recodes. You can see the immediate effect of these recodes by
    running the script from the top to where your cursor is (opt-CMD-B
    on a Mac) and then check the results.

5.  Once complete and satisfied you should `source()` the recode script
    from beginning to end. This is important for a few reasons. First,
    running `source()` is *noticeably* faster than executing code by
    selecting code in RStudio and running `cmd-Enter`. We have been
    doing this and it can take forever.

Users will notice that each recode script always starts by loading the
*original raw data file* and then saving an `.rda` file out into the
folder `data`. *This* is the file that is available to users when they
install and load `cesdata2`, *not* the raw data file.

The reason for doing this is to protect the integrity of the original
data files by ensuring there is no messy re-writing of original
variables and always to be able to track exactly what has been recoded
as what.

6.  Because the installed package has pre-built `rda` files for each
    data-set *it is unnecessary to source anything unless changes have
    been made*.

However, remember, R uses the version of the package stored in the
library, *not* the cloned version you have been working on. After
sourcing all scripts where new recodes have been made, installing the
new version of the package happens by selecting `Build` and then
`Install` in RStudio.

This moves the package contents from location of the cloned directory to
the R library deeper in your computer. From here, any analysis script
that loads `cesdata2` should make use of your recodes.

7.  To share with other users, which is highly desirable, *re-open the
    cloned `cesdata2`* project on your computer, make a new Github
    branch, using an informative title, commit these changes to the
    GitHub repository and push them to that new branch, *not* the main
    branch.

The package is currently set up to permit only Simon Kiss and Matt
Polacko to contribute to the main branch. All others must commit to
other branches. Simon Kiss, package maintainer, will merge branches to
the main branch and communicate when that has happened.

# Outstanding Issues

1.  **2015 Web Survey**

The 2015 Web survey is currently included in the package, but there have
been almost no variables recoded.

2.  **Value labels**

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

library(tidyverse)
#Install cesdata2
#devtools::install_github("sjkiss/github")
library(cesdata2)

#Seprate ces79 and ces80 to two separate files
ces7980 %>% 
  #This variable indicates Rs who completed the 1979 survey, no panel respondents
  filter(V4002==1)->ces79

ces7980 %>% 
  #This variable indicates Rs who completed the 1980 survey, may also havecompleted the 1979 survey; a user should check.
  filter(V4008==1)->ces80
# Drop the `election` variable from ces80
# This is a quirk of the election80 variable; 
# I'll explain another time. Trust me; it is necessary and in this order.
ces80 %>% 
  select(-election)->ces80
#Remove the '80' from the duplicate ces80 variables
# in the ces780
names(ces80)<-str_remove_all(names(ces80), "80")
#Check
tail(names(ces80))
#> [1] "previous_vote" "mode"          "mip"           "foreign"      
#> [5] "turnout"       "election"
```

``` r

### Decide On CES 1993 sample
# this command includes only those respondents that completed the 1993 Campaign and Post-Election Survey
# ces93[!is.na(ces93$RTYPE4), ] -> ces93

  #Use Panel Respondents for 2004
ces0411 %>%
 filter(str_detect(ces0411$survey, "PES04"))->ces04

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
#> $ age                    <dbl> 45, 36, 48, 32, 53, 33, 47, 75, 45, 62, 71, 24,…
#> $ religion               <dbl+lbl> 2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2…
#> $ vote                   <dbl+lbl>  1,  1,  2,  1,  1,  1,  2,  2,  2,  1,  2,…
#> $ income                 <dbl+lbl>  1,  2,  2,  2,  3,  2,  3,  2,  3,  2,  1,…
#> $ turnout                <dbl+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ mip                    <dbl+lbl> 18,  0, 15, 15, 15,  6, 11, 18, 16, 15, 15,…
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
```

# Credit

As this R package remains a private, by invitation only repository,
there is no way to cite it. Instead, collaborators invited to
participate are requested to cite one of the papers in the reference
list below as recognition of the work that has gone into this. This
package was instrumental in both publications.

# References

Polacko, Matthew, Simon Kiss, and Peter Graefe. “The Changing Nature of
Class Voting in Canada, 1965-2019.” Canadian Journal of Political
Science/Revue Canadienne de Science Politique 55, no. 3 (September 1,
2022): 1–24.

Kiss, Simon, Peter Graefe, and Matt Polacko. “The Evolving
Education-Income Cleavage In Canada.” Electoral Studies, 2023.
