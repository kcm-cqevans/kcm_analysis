# kcm_analysis

Speed up data cleaning and analysis using common functions frequently used by Metro. 

## Changelog
- 2/25/24: fixed survey_collapse function 
- 2/25/24L: added viz functions 

## Installation 
load the package: 

```
devtools::install_github("celinafg/kcm_analysis")
library(kcmanalysis)
```
## Usage 
Italicized variables are OPTIONAL. 

## rename_generic 
Rename column values based on custom patterns. It accepts the following parameters: 

| Variable      | Type        | Description|
| ----------- | ----------- | -----------|
| data      | DATAFRAME       |your data |
|column |STRING |name of the column with values you want to change |
|pattern |VECTOR| vector of strings that include the originalValueName = newValueName|
| *na_action* | STRING | handle NA values with strings KEEP or REMOVE 
| *verbose* | BOOLEAN | when TRUE, enables console descriptions of operations. logs all values changed | 

```
# pass your own pattern here. make sure it's a vector, with a key:value pair. 

pattern <- c(  
    "EN" = "ENGLISH"
    "VI" = "VIETNAMESE"
)

# In column `userlanguage`, find all patterns that match `pattern` and keep null values. 
renamed_language <- rename_generic(data, column="userlanguage", pattern_replacements=pattern, na_action = "keep", verbose=FALSE)

```

#### survey_reshape 
Reshapes a dataframe from wide to long format. Accepts the following parameters: 

| Variable      | Type        | Description|
| ----------- | ----------- | -----------|
| svy_reshape_cols| VECTOR / STRING |  If only choosing to pivot one column, use one string. If multiple columns, a vector of strings. |
| data | DATAFRAME| your data| 


```
svy_reshape_cols <- c("screenreader", "userlanguage")

reshaped_survey <- survey_reshape(data, pivot_cols=svy_reshape_cols)


``` 

#### survey_collapse
Accepts the following parameters: 

| Variable      | Type        | Description|
| ----------- | ----------- | -----------|
| data | DATAFRAME| your data |
| depvar | STRING | dependent variable column name |
| response | STRING | response column name | 
|wgt | STRING | weights column name |
| *groupingvars* | VECTOR | Columns used for additional grouping. NULL if no additonal grouping is needed. |
| *verbose* | BOOLEAN | when TRUE, enables console description of operations | 


```
collapsed <- survey_collapse(data, depvar="zipcode", response="nonr_tr_freq", wgt = "weights2" )

```

### Viz Functions 
(based on functions written by Claire)
1. viz_agree
2. viz_dv_flip
3. viz_dv_group
4. viz_fonts
5. viz_hist
6. viz_hist_flip

