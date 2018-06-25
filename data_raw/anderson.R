library("magrittr")

anderson = readxl::read_xls("data/anderson.xls")

anderson %>% dplyr::transmute(reference = `Full Reference`,
                              outcome = Outcome,
                              best = `Best?` == "y",
                              experimental = Setting == "Exp",
                              country = Country,
                              correlation = Correlation,
                              ni = `Sample size`,
                              yi = `Fisher's Z`,
                              vi = `Std Err`^2) ->
  anderson

years = stringr::str_split(stringr::str_split(drop(anderson$reference),
                                              c(" \\("), simplify = TRUE)[ , 2],
                           c("\\)"), simplify = TRUE)[, 1]
anderson$year = as.integer(years)
anderson$in_press = (years == "in press")
anderson$year[104:106] = 2009
anderson$year[is.na(anderson$year)] = 2010

save(anderson, file = "data/anderson.Rda")
load("data/anderson.Rda")

#
#
# anderson$author = stringr::str_split(drop(anderson$reference), c(" \\("), simplify = TRUE)[ ,1]
#
# anderson$study  = stringr::str_trim(stringr::str_remove(
#   str = stringr::str_split(drop(anderson$reference), c("\\)"), simplify = TRUE)[ ,2],
#                     pattern = c("\\. ")))
#
#
#
# years = stringr::str_split(stringr::str_split(drop(anderson$reference), c(" \\("), simplify = TRUE)[ , 2], c("\\)"), simplify = TRUE)[, 1]
# in_press = (years == "in press")
# anderson$year = as.integer(stringr::str_split(stringr::str_split(drop(anderson$reference), c(" \\("), simplify = TRUE)[ , 2], c("\\)"), simplify = TRUE)[, 1])
# anderson$year[104:106] = 2009
# anderson$year[is.na(anderson$year)] = 2010
