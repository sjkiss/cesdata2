
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
available by calling:

``` r
library(devtools)
devtools::install_github("sjkiss/cesdata2")
library(cesdata2)
## basic example code
tail(names(ces21))
#> [1] "postgrad"      "inequality"    "efficacy_rich" "pol_interest" 
#> [5] "foreign"       "enviro_spend"
tail(names(ces65))
#> [1] "foreign"           "mip"               "redistribution"   
#> [4] "market_liberalism" "traditionalism2"   "immigration_rates"
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

# Package Structure

Before explaining how variables are recoded, it is worth describing the
package structure. We have followed advice on creating so-called `data`
packages [here](https://r-pkgs.org/data.html) and
[here](https://www.davekleinschmidt.com/r-packages/).

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
#>  [1] "ces00.rda"      "ces0411.rda"    "ces15phone.rda" "ces19phone.rda"
#>  [5] "ces19web.rda"   "ces21.rda"      "ces65.rda"      "ces68.rda"     
#>  [9] "ces72_nov.rda"  "ces74.rda"      "ces7980.rda"    "ces84.rda"     
#> [13] "ces88.rda"      "ces93.rda"      "ces97.rda"
```

The recode scripts themselves are stored in the package subfolder
`data-raw/recode_scripts/`. When these scripts are executed via R’s
`source()` command, they:

1.  Import the Stata or SPSS file from `data-raw`
2.  Execute all documented recode functions
3.  Save an `.rda` object, properly named in the folder `data`

Doing so, makes the `.rda` file, with all recodes executed, available to
users on install and loading.

## Recoding conventions

Polacko and Kiss (particularly the former) have gone to great lengths to
recode and rename variables of interest to allow for combination into a
single tabular dataset that allows for time series analysis.

Wherever possible, the following conventions have been used.

1.  Likert items have been rescaled from 0 to 1
2.  Liberal views are scored 0 and conservative views are scaled high.

This is an incomplete list of recoded and renamed variables.

| Concept | Variable Name |  notes  |
|:--------|--------------:|:-------:|
| This    |          This |  This   |
| column  |        column | column  |
| will    |            be | aligned |
| left    |         right | center  |

# Establishing A Usable Time Series

## Future recodes

However, users are also welcome to contribute to the package’s
development by adding new recodes in the recode scripts. The next
section shows how to do that:

### Adding recoded variables

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
starts by loading the original raw data file and then saving an `.rdata`
file out into the folder `data`. *This* is the file that is available to
users when they install and load `cesdata2`

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

## Constructing a dataset for analysis.

In the simple case of combinining three single-election datasets, we can
combine them in this way.

1.  Make a list of your datasets.
2.  Provide names for each list item, corresponding to the election year
3.  Make a separate vector of all the variable names you want to select
4.  Combine `map()` and `select(any_of())` and `list_rbind()`to select
    those variables and join them together in a tabular data frame.

``` r
#make a list of datasets
ces.list<-list(ces00, ces93, ces97)
#Make a vector of desired common variables
myvars<-c("male", "degree")
library(tidyverse)
library(haven) 
ces.list %>% 
  map(., select, any_of(myvars)) %>% 
  list_rbind(., names_to=c("election"))->ces
glimpse(ces)
#> Rows: 12,471
#> Columns: 3
#> $ election <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ male     <dbl+lbl> 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, …
#> $ degree   <dbl+lbl> 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
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
myvars<-c("gender", "vote", "gay_rights")
#Make data frame
ces.list %>% 
  #Select the variables
  map(., select, any_of(myvars)) %>% 
  #bind together providing a new variable Election containing the election year
  list_rbind(., names_to=c("Election"))->ces
```

Now we can confirm what was happen with the variable `gay_rights` in
each year.

``` r
#Check the gay_rights variable in 1984
ces %>% 
  filter(Election==1984) %>% 
  select(gay_rights)
#> # A tibble: 3,377 × 1
#>    gay_rights
#>     <dbl+lbl>
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
  filter(Election==1988) %>% 
  select(gay_rights)
#> # A tibble: 3,609 × 1
#>    gay_rights
#>     <dbl+lbl>
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

There are two panel sets in the complete series, 1979-1980 and
2004-2006-2008-2011. These present some challenges for the strategy we
chose of consistently renaming variables.

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
sequence by using `filter()` and a regular epression in `str_detect()`.
Note, to diagnose the accuracy of your regular expressin and filter
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
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|      1 |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|      4 |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|      1 |     NA |     NA |     NA |
|      2 |     NA |     NA |     NA |
|      2 |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|      3 |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|     NA |     NA |     NA |     NA |
|      2 |     NA |     NA |     NA |
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
other purely cross-sectional respondents. We leave this to the user.

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
select(contains('vote'))
#> # A tibble: 2,324 × 5
#>    ces08_PES_PROV_VOTE1            vote04    vote06    vote08    vote11
#>               <dbl+lbl>         <dbl+lbl> <dbl+lbl> <dbl+lbl> <dbl+lbl>
#>  1                   NA NA                       NA        NA        NA
#>  2                   NA NA                       NA        NA        NA
#>  3                   NA  2 [Conservative]        NA        NA        NA
#>  4                   NA  2 [Conservative]        NA        NA        NA
#>  5                   NA NA                       NA        NA        NA
#>  6                   NA NA                       NA        NA        NA
#>  7                   NA  3 [NDP]                 NA        NA        NA
#>  8                   NA  2 [Conservative]        NA        NA        NA
#>  9                   NA NA                       NA        NA        NA
#> 10                   NA NA                       NA        NA        NA
#> # ℹ 2,314 more rows
```

Note, because we used an expansive filtering strategy for `ces08` there
are respondents who actully have responses for variables from 2004.
However, because we are defining this as a dataset for 2008, we will
effectively drop those earlier responses and only take respondents’
values from 2008.

``` r
ces08 %>% 
  select(contains('vote')) 
#> # A tibble: 3,689 × 5
#>             ces08_PES_PROV_VOTE1       vote04           vote06    vote08  vote11
#>                        <dbl+lbl>    <dbl+lbl>        <dbl+lbl> <dbl+lbl> <dbl+l>
#>  1  3 [NDP (New Democrats)]       5 [Green]   3 [NDP]           3 [NDP]  2 [Con…
#>  2  3 [NDP (New Democrats)]       1 [Liberal] 1 [Liberal]       1 [Libe… 1 [Lib…
#>  3 97 [R volunteers: It depends] NA           1 [Liberal]      NA        3 [NDP]
#>  4  3 [NDP (New Democrats)]       1 [Liberal] 1 [Liberal]       3 [NDP]  3 [NDP]
#>  5  0 [Other]                     4 [Bloc]    3 [NDP]           3 [NDP]  3 [NDP]
#>  6  8 [L'Action Democratique]     4 [Bloc]    3 [NDP]           3 [NDP]  3 [NDP]
#>  7 99 [refused]                  NA           2 [Conservative] NA        2 [Con…
#>  8  1 [Liberal (Grits)]           5 [Green]   5 [Green]         1 [Libe… 5 [Gre…
#>  9  1 [Liberal (Grits)]           4 [Bloc]    3 [NDP]           4 [Bloc] 2 [Con…
#> 10  1 [Liberal (Grits)]           1 [Liberal] 2 [Conservative]  2 [Cons… 2 [Con…
#> # ℹ 3,679 more rows
```

The most straightforward way to do this is to strip out year identifying
numbers from the variables you *want* to keep.

So, for the `ces04` dataset, we want to keep the variables recoded with
`04` appended. You can do this really quickly with the `clean_names()`
function in the `janitor` package.

``` r
library(janitor)
ces04 %>% 
  clean_names(., replace=c("vote04"="vote", 
              "gender04"="gender", 
              "degree04"="degree")) %>% 
  select(survey, vote, gender, degree)->ces04
#Check the names
names(ces04)
#> [1] "survey" "vote"   "gender" "degree"
```

And for `ces08`

``` r
ces08 %>% 
  clean_names(., replace=c("gender08"="gender",
                           "degree08"="degree",
                           "vote08"="vote")) %>% 
  select(gender, degree, vote)->ces08
#names(ces08)
```

And now, we could proceed, as noted above, creating a unified tabular
ces dataset from with other, more simple, one-election datafiles.

``` r
data("ces93")
data("ces97")
data("ces00")
#List the data frames to be used.
ces.list<-c(ces93, ces97, ces00, ces04, ces08)
#Provide names
names(ces.list)<-c(1993, 1997, 2000, 2004, 2008)
```

3.  The 1980 Canada Election Study

Something happened in the 1980 election because it was called so
quickly, the CES team was not able to field a full survey or something.
As a result, some demographic variables were not asked.

# Outstanding Issues

1.  It would be desirable to add a `mode` variable in the recode scripts
    so that potentially phone and web surveys from 2015 and 2019 could
    be used in the same analysis and the analyst could distinguish which
    mode provided the case. The consistent naming scheme could remain
    for common variables (e.g. `male` for male gendered people, `vote`
    for vote cast), but then there could be

2.  Value labels

Right now, all the original CES files are imported via the `haven`
package which creates this very awkwared `labelled` class variable
instead of standard R factors. In hindsight this was a mistake. It would
be better to go back and convert those variables as value labels
straight to factors.



## References
