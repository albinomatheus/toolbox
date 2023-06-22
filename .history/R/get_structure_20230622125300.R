#' Estimate population counts from Census Statistics
#'
#' Smooth population counts using PCLM method
#'
#' @export
#' 
get_structure <- function(data, area, year, sex, omega) {
  # 1) Input with missing values
  data2 <- data  |>
    dplyr::filter(
      area == as.character(area),
      year == as.numeric(year),
      age != "ND",
      sex == as.character(sex))  |>
    dplyr::mutate(age = as.numeric(age))
  
  # 2) Input without missing values
  data_na <- data |> 
    dplyr::filter(
      area == as.character(area),
      year == as.numeric(year),
      sex == as.character(sex))  |>
    dplyr::mutate(age = as.numeric(age)) |> 
    dplyr::summarise(total = sum(pop))
  
  # 3) PCLM
  pclm_fit <- 
    DemoTools::graduate_pclm(
      Value = data2$pop[data2$sex == sex], 
      Age = as.numeric(data2$age[data2$sex == sex]), 
      AgeInt = data2$ageint[data2$sex == sex],
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
      pop = round(pop_gr),
      area = area,
      year = year,
      sex = sex)  |> 
    dplyr::as_tibble()
  
  abridged <-  
    base::data.frame(
      age = c(0, 1, seq(5, 85, by = 5)),
      pop = round(pop_abr),
      area = area,
      year = year,
      sex = sex)  |> 
    dplyr::as_tibble()
  
  full <- 
    base::data.frame(
      age = 1:length(pop_full) - 1,
      pop = round(pop_full),
      area = area,
      year = year,
      sex = sex)  |> 
    dplyr::as_tibble()
  
  output <- 
    base::list(
      pop_5x1 = grouped, 
      pop_abridged = abridged, 
      pop_1x1 = full)
  
  return(output)
  
}