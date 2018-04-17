## Does things to the motyl xls file. Finish up later (with documentation!)
library("magrittr")

motyl = readxl::read_excel("data/motyl.xls")

motyl %>%
  dplyr::filter(stringr::str_detect(copied.statistic, "^F\\(1") |
                stringr::str_detect(copied.statistic, "^t")) ->
  motyl

motyl %>%
  dplyr::mutate(z = ifelse(stringr::str_detect(copied.statistic, "^F\\(1"),
                           sqrt(test.statistic),
                           test.statistic)) ->
  motyl

motyl %>%
  dplyr::mutate(prop_excluded = excluded.participants/sample.size) ->
  motyl


## Use only those with df given:
motyl = dplyr::filter(motyl, !is.na(df.denominator) & !is.na(z))
motyl = dplyr::filter(motyl, abs(z)/sqrt(df.denominator) < 2)

motyl = dplyr::filter(motyl,
                 expcor == "Experimental (i.e., all IVs were manipulated)")
motyl = dplyr::filter(motyl, design == "Between")
motyl = dplyr::filter(motyl, df.denominator < 300)
motyl = dplyr::filter(motyl, z < 6)

motyl_data       = data.frame(z = abs(motyl$z))
motyl_data$M     = motyl$df.denominator
motyl_data$M_scaled     = scale(motyl$df.denominator)
motyl_data$lower = qt(0.975, motyl_data$M)
motyl_data$upper = rep(0, length(motyl_data$M))
motyl_data$d     = abs(motyl_data$z)/sqrt(motyl_data$M)
motyl_data$dist_indices = rep(1, length(motyl_data$M))
motyl_data$dist_indices[motyl_data$z < motyl_data$lower] = 5



