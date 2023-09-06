#' Extract deaths from Demographic and Sanitary Statistics
#'
#' Smooth death counts using PCLM method
#'
#' @export
#' 
extract_deaths <- function(data, location, sex, year1, year2, omega = 100) {
  # 1) Informações básicas
  diff <- year2 - year1
  
  # Full database filtered 
  d_0 <- data |> 
    dplyr::filter(between(as.numeric(ano), year1, year2),
                  local == location,
                  sexo == sex,
                  is.na(idadeL) == F) |> 
    dplyr::select(ano, local, idadeL, idadeU, sexo, obitos)
  
  # Totals (with missing values)
  d_na <- data |> 
    dplyr::mutate(ano = as.numeric(ano)) |> 
    dplyr::filter(local == location,
                  sexo == sex,
                  between(ano, year1, year2)) |> 
    dplyr::summarise(total = sum(obitos))
  
  # List of years between 2 dates
  years_between <- unique(as.numeric(d_0$ano))  
  
  # List of dataframes - deaths by year
  df_list <- years_between %>%
    purrr::map(~ d_0 |> 
                 dplyr::filter(as.numeric(ano) == .x) |> 
                 dplyr::group_by(idadeL, idadeU) |> 
                 dplyr::reframe(
                  age = calcAgeAbr(as.integer(idadeL)),
                  obitos = sum(obitos),
                  width = case_when(idadeU - idadeL == 1 ~ 1,
                                     TRUE ~ idadeU - idadeL + 1)) |> 
                 dplyr::distinct() |> 
                 dplyr::mutate(idadeL = ifelse(idadeL >= 70, 70, idadeL)) %>%
                 dplyr::group_by(idadeL) %>%
                 dplyr::summarise(obitos = sum(obitos), width = sum(width)) %>%
                 dplyr::mutate(
                  age = idadeL,
                  idadeU = idadeL + width) %>%
                 dplyr::select(idadeL, idadeU, age, obitos, width)
               )
  
  # Lista de Dataframes detalhada
  df <- df_list %>%
    purrr::map_dfr(~ DemoTools::graduate_pclm(
      Value = .x$obitos,
      Age = as.numeric(.x$idadeL),
      AgeInt = case_when(.x$idadeU - .x$idadeL == 1 ~ 1,
                                     TRUE ~ idadeU - idadeL + 1),
      OAnew = omega), 
      .id = "year")
  
  # Lista detalhada por ano
  full <- df |>
    tidyr::pivot_longer(-year, names_to = "age", values_to = "deaths") |> 
    dplyr::mutate(
      age = as.numeric(age),
      year = as.numeric(year) + years_between[1] - 1)
  
  # Lista por ano em idades simples
  single <- df_list %>%
    purrr::map_dfr(~ DemoTools::graduate_pclm(
      Value = .x$obitos,
      Age = as.numeric(.x$idadeL),
      AgeInt = case_when(.x$idadeU - .x$idadeL == 1 ~ 1,
                                     TRUE ~ idadeU - idadeL + 1),
      OAnew = omega), 
      .id = "year") |> 
    tidyr::pivot_longer(-year, names_to = "age", values_to = "deaths") |> 
    dplyr::mutate(
      age = as.integer(age),
      year = as.numeric(year) + years_between[1] - 1
    ) |> 
    dplyr::group_by(age) |> 
    dplyr::reframe(deaths = sum(deaths)) |> 
    dplyr::mutate(deaths = d_na$total * (deaths/sum(deaths)),
                  local = location,
                  year = paste0(year1, "-", year2),
                  sex = sex)
  
  # Resultados consolidados
  deaths_full <- single$deaths
  
  # Mortes em agrupamentos abreviados
  d_abr <- 
    DemoTools::groupOAG(
      DemoTools::single2abridged(deaths_full),
      Age = c(0,1, seq(5,omega,5)), 
      OAnew = 85)
  
  # Mortes em grupos quinquenais
  d_gr <- 
    DemoTools::groupAges(
      deaths_full, 
      Age = 1:length(deaths_full) - 1, 
      N = 5, 
      shiftdown = 0, 
      OAnew = 85)
  
  grouped <- 
    base::data.frame(
      age = seq(0, 85, by = 5),
      deaths = d_gr,
      local = location,
      year = paste0(year1, "-", year2),
      sex = sex)  |> 
    dplyr::as_tibble()
  
  abridged <-  
    base::data.frame(
      age = c(0, 1, seq(5, 85, by = 5)),
      deaths = d_abr,
      local = location,
      year = paste0(year1, "-", year2),
      sex = sex)  |> 
    dplyr::as_tibble()
  
  output <-     
    base::list(
      df_list = df_list,
      full = full,
      single = single,
      grouped = grouped,
      abridged = abridged)
  
  is.sequential <- \(x) {
    if (all(diff(x) == diff(x)[1])) {
      return(print("Soma do período calculada com sucesso"))
    } else {
      return(print("Informação não representa a soma de todo o período"))
    }
}
  
  is.sequential(years_between)
  
  return(output)
}