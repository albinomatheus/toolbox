#' read_EDS: cause-specific mortality database
#'
#' Create Demographic-Sanitary Statistics database
#'
#' @export
#' 
read_EDS <- function(file) {
  # Input: file path
  file <- file
  # Output: tidy data frame w/ causes of death (CoD)
  data <- read_csv2(here(file)) %>%
    naniar::replace_with_na_all(condition = ~ .x %in% na_strings) %>%
    mutate(
      causa_mortis = paste0(causa_mortis_1, ": ", causa_mortis_2),
      M_total = as.numeric(M_total),
      F_total = as.numeric(F_total)
    )
  # Output: create year and location variable w/ file name information
  data <- data |>
    pivot_longer(c(6:ncol(data) - 1),
      names_to = "sexo_idade",
      values_to = "obitos"
    ) %>%
    mutate(obitos = coalesce(obitos, 0L)) |>
    mutate(tmp = gsub("(\\.\\.csv)", "", file)) |>
    separate(tmp, into = c("path1", "path2", "path3", "local", "ano")) |>
    select(ano, local, CID_1, CID_2, causa_mortis, sexo_idade, obitos)

  # TODO: Merge database with ICD and Sex-age "dictionaries"
  sexo_idade <- read_csv2(here::here("data/raw/EDS/DIC_sexoidade.csv"))
  causamortis <- read_csv(here("data/raw/EDS/DIC_causamortis.csv"))
  classification <- read_csv2(here("data/raw/icd/CCHM.csv"))
  cchm <- causamortis |> left_join(classification)

  data <- reduce(list(data,sexo_idade,cchm), left_join) 
    
  return(data)
}