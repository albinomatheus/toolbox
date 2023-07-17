#' Extract deaths from Demographic and Sanitary Statistics
#'
#' Smooth death counts using PCLM method
#'
#' @export
#' 
extract_deaths <- function(data, location, sex, year1, year2) {
  # 1) Informações básicas
  data <- data
  location <- location
  year1 <- year1
  year2 <- year2
  sex <- sex
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
  
  df_list <- years_between %>%
    purrr::map(~ d_0 |> 
                 dplyr::filter(as.numeric(ano) == .x) |> 
                 dplyr::group_by(idadeL, idadeU) |> 
                 dplyr::reframe(obitos = sum(obitos),
                                width = idadeU - idadeL) |> 
                 dplyr::distinct()
    )
  
  df <- df_list %>%
    purrr::map_dfr(~ DemoTools::graduate_pclm(
      Value = .x$obitos,
      Age = as.numeric(.x$idadeL),
      AgeInt = .x$width,
      OAnew = 100,
      OAG = TRUE), 
      .id = "year")
  
  df_full <- df |>
    tidyr::pivot_longer(-year, names_to = "age", values_to = "deaths") |> 
    dplyr::mutate(
      age = as.numeric(age),
      year = as.numeric(str_extract(year, "\\d")) + years_between[1] - 1)
  
  d_single <- df_full |> 
    dplyr::group_by(age) |> 
    dplyr::reframe(deaths = sum(deaths)) |> 
    dplyr::mutate(deaths = d_na$total*(deaths/sum(deaths)))
  
  d_abr <- 
    DemoTools::single2abridged(d_single$deaths)
  
  d_gr <- 
    DemoTools::groupAges(
      d_single$deaths, 
      Age = d_single$age, 
      N = 5, 
      shiftdown = 0, 
      OAnew = 85)
  
  grouped <- 
    base::data.frame(
      age = seq(0, 85, by = 5),
      deaths = d_gr,
      local = location,
      ano1 = year1,
      ano2 = year2,
      sex = sex)  |> 
    dplyr::as_tibble()
  
  abridged <-  
    base::data.frame(
      age = c(0, 1, seq(5, 85, by = 5), 85, 85, 85),
      deaths = d_abr,
      local = location,
      ano1 = year1,
      ano2 = year2,
      sexo = sex)  |> 
    dplyr::as_tibble() |> 
    dplyr::group_by(age, local, ano1, ano2, sexo) |> 
    dplyr::reframe(deaths = sum(deaths)) |> 
    dplyr::ungroup()
    
  single <- 
    base::data.frame(
      age = d_single$age,
      deaths = d_single$deaths,
      local = location,
      ano1 = year1,
      ano2 = year2,
      sexo = sex)  |> 
    dplyr::as_tibble()
  
  output <-     
    base::list(
      completa = df_full,
      grupos = grouped, 
      abreviada = abridged, 
      idade_simples = single)

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