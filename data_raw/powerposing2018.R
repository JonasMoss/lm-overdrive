library("magrittr")

## The data in the file "powerposing2018.R" is taken from the supplementary
## materials to:
## 
## "P-Curving a More Comprehensive Body of Research on Postural Feedback Reveals 
## Clear Evidential Value for Power-Posing Effects: Reply to Simmons and 
## Simonsohn (2017)" (Psychological Science)
## 
## Paper URL: http://journals.sagepub.com/doi/abs/10.1177/0956797617746749
## File URL: https://osf.io/jx3av/

powerposing2018_pre = readxl::read_xlsx("powerposing2018.xlsx")

## Remove studies with no main result
powerposing2018_pre %>% 
  dplyr::filter(!is.na(`(Main) Results`)) %>%
  dplyr::select(`(Main) Results`) %>%
  "["(, "(Main) Results", drop = TRUE)->
  powerposing2018

type_and_z = stringr::str_split(string   = powerposing2018, 
                                pattern  = "=", 
                                simplify = TRUE)

z_values = as.numeric(type_and_z[, 2])
types = substr(type_and_z[, 1], 1, 1)
df1 = rep(NA, length(types))
df2 = rep(NA, length(types))

## Add df to the f-values
tmp = type_and_z[types == "F", 1]
tmp = stringr::str_split(tmp, "F\\(", simplify = TRUE)[, 2]
tmp = stringr::str_split(tmp, ",", simplify = TRUE)
tmp[, 2] = stringr::str_remove(tmp[, 2], "\\)")
tmp = apply(tmp, 2, as.numeric)
df1[types == "F"] = tmp[ , 1]
df2[types == "F"] = tmp[ , 2]
z_values[types == "F"] = sqrt(z_values[types == "F"] )

## Add df to the t-values
tmp = type_and_z[types == "t", 1]
tmp = stringr::str_split(tmp, "t\\(", simplify = TRUE)[, 2]
tmp = stringr::str_split(tmp, "\\)", simplify = TRUE)[, 1]
df2[types == "t"] = as.numeric(tmp)

## Add M and vi.
vi = 1/df2*4
M = 1/vi

## Remove chi2 and z
z_values[types == "z" | types == "c"] = NA


plot(M, z_values/sqrt(M), log = "xy", pch = 19, cex = 1.4)
lines(M, qt(0.975, df = df2)/sqrt(M))
lines(M, qt(0.95, df = df2)/sqrt(M))
