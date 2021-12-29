#' Adjusting population age structure
#'
#' Smooth population counts using Arriaga and Sprague methods
#'
#' @export

adjust_age <- function(data, location = "Estado de São Paulo", year = 2010, AgeL = 0, AgeH = 75) {
  AgeL <- AgeL
  AgeH <- AgeH
  data <- data %>% 
    filter(local == as.character(location), 
           ano == as.numeric(year),
           idade != "ND") %>%
    mutate(idade = as.numeric(idade))
# Suavização Arriaga
  # Sexo Masculino
  m <- DemoTools::smooth_age_5(Value = data$pop[data$sexo == "M"], 
                         Age = data$idade[data$sexo == "M"],
                         method = "Arriaga",
                         OAG = TRUE) 
  m <- DemoTools::graduate_sprague(Value = m, Age = seq(0,max(data$idade), by = 5), OAG = TRUE) 
  m <- DemoTools::groupAges(m, Age = 1:length(m) - 1, N = 5, shiftdown = 0, OAnew = 80)
  # Sexo Feminino
  f <- DemoTools::smooth_age_5(Value = data$pop[data$sexo == "F"], 
                    Age = data$idade[data$sexo == "F"],
                    method = "Arriaga",
                    OAG = TRUE) 
  f <- DemoTools::graduate_sprague(Value = f, Age = seq(0,max(data$idade), by = 5), OAG = TRUE) 
  f <- DemoTools::groupAges(f, Age = 1:length(f) - 1, N = 5, shiftdown = 0, OAnew = 80)
  # Banco de dados consolidado
  pop <- data.frame(age = seq(AgeL, AgeH, by = 5), "F" = round(f), "M" = round(m),
                    local = location, ano = year) %>% 
    as_tibble()
  return(pop)
}