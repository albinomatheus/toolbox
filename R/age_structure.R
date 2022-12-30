#' Adjusting age structure
#'
#' Smooth population counts using PCLM method
#'
#' @export
#' 
age_structure <- function(data, location, year) {
  ####### Informações Básicas
  # Dados sem valores missing
  data <- data %>% 
    filter(local == as.character(location), 
           ano == as.numeric(year),
           idade != "ND") %>%
    mutate(idade = as.numeric(idade))
  # Dados com valores missing 
  data_nd <- data %>% 
    filter(local == as.character(location), 
           ano == as.numeric(year))
  # População Total 
  pop_tot <- sum(data_nd$pop)
  # Idade Máxima: 105 anos
  nlast = 104 - max(data$idade)
  ###########################################
  # Suavização PCLM
  # Sexo Masculino
  input_m <- pclm(as.numeric(data$idade[data$sexo == "M"]), 
                  data$pop[data$sexo == "M"], 
                  nlast = nlast, 
                  ci.level = 95)
  input_m <- tibble(pop_true = round(input_m$fitted), 
                    x = pop_true/sum(pop_true), 
                    pop_est = round(pop_tot*x))
  m <- input_m$pop_est
  
  m_abr <- single2abridged(m)
  m_ga <- groupAges(m, Age = 1:length(m) - 1, N = 5, shiftdown = 0, OAnew = 85)
  # Sexo Feminino
  input_f <- pclm(as.numeric(data$idade[data$sexo == "F"]), 
                  data$pop[data$sexo == "F"], 
                  nlast = nlast, 
                  ci.level = 95)
  input_f <- tibble(pop_true = round(input_f$fitted), 
                    x = pop_true/sum(pop_true), 
                    pop_est = round(pop_tot*x))
  f <- input_f$pop_est
  
  f_abr <- single2abridged(f)
  f_ga <- groupAges(f, Age = 1:length(f) - 1, N = 5, shiftdown = 0, OAnew = 85)
  # Banco de dados consolidado
  pop_grouped <- data.frame(age = seq(0, 85, by = 5), 
                            f = f_ga, m = m_ga, local = location, ano = year) %>% 
    as_tibble()
  pop_abridged <- data.frame(age = c(0, 1, seq(5, 85, by = 5), 85, 85, 85), 
                             f = f_abr, m = m_abr, local = location, ano = year) %>% 
    as_tibble() |> 
    group_by(age, local, ano) |> 
    summarise(f = sum(f),
              m = sum(m)) |> 
    ungroup()
  
  pop_detail <- data.frame(age = 1:length(f) - 1, 
                           f = f, m = m, local = location, ano = year) %>% 
    as_tibble()
  
  results <- list(pop_grupos = pop_grouped, 
                  pop_abreviada = pop_abridged, 
                  pop_idade_simples = pop_detail)
  return(results)
}