# kcm_analysis

Speed up data cleaning and analysis using common functions frequently used by Metro. 

### Installation 

### Usage 
Italicized variables are OPTIONAL. 

#### rename_generic 
Rename column values based on custom patterns. It accepts the following parameters: 
  - data (DATAFRAME): your data 
  - column (STRING): name of the column with values you want to change 
  - pattern (VECTOR): vector of strings that include the originalValueName = newValueName
  - *na_action* (STRING): handle NA values with the strings: 
    - "keep" 
    - "remove" 
  - *verbose* (BOOLEAN): when TRUE, enables console descriptions of operations. logs all values changed. 

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
- data (DATAFRAME): your data
- pivot_cols (STRING) (VECTOR): If only choosing to pivot one column, use one string. If multiple columns, a vector of strings. 
- 

```
svy_reshape_cols <- c("screenreader", "userlanguage")

reshaped_survey <- survey_reshape(data, pivot_cols=svy_reshape_cols)


``` 

#### survey_collapse
Accepts the following parameters: 
- data (DATAFRAME) = your data 
- depvar (STRING) = dependent variable column name 
- response (STRING) = response column name 
- wgt (STRING) = weight column name 
- *groupingvars* (VECTOR) = Columns used for additional grouping. NULL if no additional grouping is needed. 
- *verbose* (BOOLEAN): when TRUE, enables console descriptions of operations

```
collapsed <- survey_collapse(data, depvar="zipcode", response="nonr_tr_freq", wgt = "weights2" )

```

