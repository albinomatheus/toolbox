#' Estimate population counts from Census Statistics
#'
#' Smooth population counts using PCLM method
#'
#' @export
#' 
get_age_structure <- function(data, location, year, sex, omega) {
  # 1) Dados sem valores missing
  data2 <- data  |>
    dplyr::filter(
      local == as.character(location),
      ano == as.numeric(year),
      idade != "ND",
      sexo == as.character(sex))  |>
    dplyr::mutate(idade = as.numeric(idade))
  
  # 2) Dados com valores missing 
  data_na <- data |> 
    dplyr::filter(
      local == as.character(location),
      ano == as.numeric(year),
      sexo == as.character(sex))  |>
    dplyr::mutate(idade = as.numeric(idade)) |> 
    dplyr::summarise(total = sum(pop))
  
  # 3) PCLM
  pclm_fit <- 
    DemoTools::graduate_pclm(
      Value = data2$pop[data2$sexo == sex], 
      Age = as.numeric(data2$idade[data2$sexo == sex]), 
      AgeInt = data2$ageint[data2$sexo == sex],
      OAnew = omega)
  pclm_res <- 
    dplyr::tibble(
      pop_true = pclm_fit, 
      prop = pclm_fit/sum(pclm_fit), 
      pop_est = data_na$total*prop)
  
  # 4) Resultados Consolidados
  pop_full <- pclm_res$pop_est
  pop_abr <- 
    DemoTools::groupOAG(
      DemoTools::single2abridged(pop_full),
      Age = c(0,1, seq(5,omega,5)), 
      OAnew = 85)
  pop_gr <- 
    DemoTools::groupAges(
      pop_full, 
      Age = 1:length(pop_full) - 1, 
      N = 5, 
      shiftdown = 0, 
      OAnew = 85)
  
  grouped <- 
    base::data.frame(
      age = seq(0, 85, by = 5),
      pop = pop_gr,
      local = location,
      year = year,
      sex = sex)  |> 
    dplyr::as_tibble()
  
  abridged <-  
    base::data.frame(
      age = c(0, 1, seq(5, 85, by = 5)),
      pop = pop_abr,
      local = location,
      year = year,
      sex = sex)  |> 
    dplyr::as_tibble()
  
  full <- 
    base::data.frame(
      age = 1:length(pop_full) - 1,
      pop = pop_full,
      local = location,
      year = year,
      sex = sex)  |> 
    dplyr::as_tibble()
  
  output <- 
    base::list(
      pop_group = grouped, 
      pop_abridged = abridged, 
      pop_full = full)
  
  return(output)
}