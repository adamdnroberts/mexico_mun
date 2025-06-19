#install.packages('devtools')
#devtools::install_github("kalimu/genderizeR")
library(genderizeR)

all_states_final <- read.csv("~/mexico_mun/raw/all_states_final.csv")
all_states_final$mun_id <- sprintf("%05d", all_states_final$mun_code)

load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")
nn_muns <- unique(nearest_neighbor_PRD$neighbor)

candidate_names <- all_states_final %>%
  filter(year <= 2000 & mun_id %in% nn_muns) %>%
  group_by(mun_id, year) %>%
  summarize(incumbent_candidate_name = first(incumbent_party_candidate), incumbent_party = first(incumbent_party), 
            runnerup_candidate_name = first(runnerup_party_candidate), runnerup_party = first(runnerup_party))

candidate_names$first_name <- findGivenNames(candidate_names$incumbent_candidate_name, country = "MX")


