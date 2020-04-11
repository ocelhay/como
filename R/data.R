################################################################################
#
#' Geographic distribution of COVID-19 cases worldwide
#' 
#' @format A tibble with 5 columns and 9922 rows:
#' \describe{
#'   \item{\code{date}}{Date}
#'   \item{\code{cases}}{Number of COVID-19 cases on date}
#'   \item{\code{deaths}}{Number of COVID-19 deaths on date}
#'   \item{\code{country}}{Country name}
#'   \item{\code{cumulative_death}}{Cumulative deaths by date}
#' }
#' 
#' @source The European Centre for Disease Prevention and Control
#'   \url{https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide}
#'   
#
################################################################################
"cases"


################################################################################
#
#' Severity/mortality by age catetory
#' 
#' @format A data.frame with 3 columns and 21 rows:
#' \describe{
#'   \item{\code{age_category}}{Age groups/categories}
#'   \item{\code{ifr}}{}
#'   \item{\code{ihr}}{}
#' }
#' 
#' @source The Novel Coronavirus Pneumonia Emergency Response Epidemiology Team. 
#'   The Epidemiological Characteristics of an Outbreak of 2019 Novel Coronavirus 
#'   Diseases (COVID-19) — China, 2020[J]. China CDC Weekly, 2020, 2(8): 113-122;
#'   and Estimation of SARS-CoV-2 mortality during the early stages of an 
#'   epidemic: a modelling study in Hubei, China and northern Italy Anthony 
#'   Hauser, Michel J Counotte, Charles C Margossian, Garyfallos Konstantinoudis, 
#'   Nicola Low, Christian L Althaus, Julien Riou medRxiv 2020.03.04.20031104; 
#'   doi: https://doi.org/10.1101/2020.03.04.20031104
#'   
#
################################################################################
"mort_sever_default"


################################################################################
#
#' Social contact matrix at home
#' 
#' @format A list with 152 elements corresponding to 152 countries with each
#'   element a tibble with 16 rows and 16 columns
#' 
#' @source Projecting social contact matrices in 152 countries using contact 
#'   surveys and demographic data Prem K, Cook AR, Jit M (2017) Projecting 
#'   social contact matrices in 152 countries using contact surveys and 
#'   demographic data. PLOS Computational Biology 13(9): e1005697. 
#'   https://doi.org/10.1371/journal.pcbi.1005697
#'   
#
################################################################################
"contact_home"


################################################################################
#
#' Social contact matrix at school
#' 
#' @format A list with 152 elements corresponding to 152 countries with each
#'   element a tibble with 16 rows and 16 columns
#' 
#' @source Projecting social contact matrices in 152 countries using contact 
#'   surveys and demographic data Prem K, Cook AR, Jit M (2017) Projecting 
#'   social contact matrices in 152 countries using contact surveys and 
#'   demographic data. PLOS Computational Biology 13(9): e1005697. 
#'   https://doi.org/10.1371/journal.pcbi.1005697
#'   
#
################################################################################
"contact_school"


################################################################################
#
#' Social contact matrix at work
#' 
#' @format A list with 152 elements corresponding to 152 countries with each
#'   element a tibble with 16 rows and 16 columns
#' 
#' @source Projecting social contact matrices in 152 countries using contact 
#'   surveys and demographic data Prem K, Cook AR, Jit M (2017) Projecting 
#'   social contact matrices in 152 countries using contact surveys and 
#'   demographic data. PLOS Computational Biology 13(9): e1005697. 
#'   https://doi.org/10.1371/journal.pcbi.1005697
#'   
#
################################################################################
"contact_work"


################################################################################
#
#' Social contact matrix - other
#' 
#' @format A list with 152 elements corresponding to 152 countries with each
#'   element a tibble with 16 rows and 16 columns
#' 
#' @source Projecting social contact matrices in 152 countries using contact 
#'   surveys and demographic data Prem K, Cook AR, Jit M (2017) Projecting 
#'   social contact matrices in 152 countries using contact surveys and 
#'   demographic data. PLOS Computational Biology 13(9): e1005697. 
#'   https://doi.org/10.1371/journal.pcbi.1005697
#'   
#
################################################################################
"contact_other"


################################################################################
#
#' UN 2019 Revision of World Population Prospects
#' 
#' @format A tibble with 4221 rows and 5 columns:
#' \describe{
#'   \item{\code{country}}{Country name}
#'   \item{\code{age_category}}{Age group/category}
#'   \item{\code{pop}}{Population}
#'   \item{\code{birth}}{Birth rate}
#'   \item{\code{death}}{Death rate}
#' }
#' 
#' 
#' @source \url{https://population.un.org/wpp/Download/Standard/Population/}
#'   
#
################################################################################
"population"

