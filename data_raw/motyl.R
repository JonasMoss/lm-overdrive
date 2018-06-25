library("magrittr")

motyl = readxl::read_xls("data/motyl.xls")

motyl %>%
  dplyr::filter(stringr::str_detect(copied.statistic, "^F\\(1") |
                  stringr::str_detect(copied.statistic, "^t")) ->
  motyl

motyl %>%
  dplyr::mutate(z = ifelse(stringr::str_detect(copied.statistic, "^F\\(1"),
                           sqrt(test.statistic),
                           test.statistic)) ->
  motyl

## Use only those with df given:
motyl = dplyr::filter(motyl, !is.na(df.denominator) & !is.na(z))

motyl %>%
  dplyr::mutate(prop_excluded = excluded.participants/sample.size) ->
  motyl

motyl$vi = 1/motyl$df.denominator
motyl$vi[sapply(motyl$design == "Between", isTRUE)] =
  motyl$vi[sapply(motyl$design == "Between", isTRUE)]*4

motyl$yi = motyl$z*sqrt(motyl$vi)

save(anderson, file = "data/motyl.Rda")
load("data/motyl.Rda")
