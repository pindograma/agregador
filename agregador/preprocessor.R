# preprocessor.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
# 
# This file is licensed under the GNU General Public License, version 3.

library(tidyverse)
library(lubridate)

early_polls = read_csv('data/early_polls.csv')
late_polls = read_csv('data/late_polls.csv')

early_polls_2020 = read_csv('data/early_polls_2020_2.csv')

recent_polls_2020 = read_csv('data/recent_polls_2020.csv') %>%
  filter(DT_FIM_PESQUISA >= make_date(2020, 9, 1))

polls = bind_rows(early_polls, late_polls, early_polls_2020, recent_polls_2020) %>%
  mutate(imputed_ci = case_when(
    is.na(confidence_interval_final) ~ '?',
    confidence_interval_final == 100 ~ '?',
    confidence_interval_final < 80 ~ '?',
    T ~ paste0(format(round(confidence_interval_final, 1), decimal.mark = ','), '%')
  )) %>%
  mutate(imputed_error = case_when(
    is.na(error_final) ~ '?',
    error_final > 10 ~ '?',
    error_final == 0 ~ '?',
    T ~ paste0(format(round(error_final, 1), decimal.mark = ','), 'p.p.')
  )) %>%
  select(year, NR_IDENTIFICACAO_PESQUISA, SG_UE, polled_UE, CD_CARGO, estimulada,
         result, DT_INICIO_PESQUISA, DT_FIM_PESQUISA, self_hired, turno,
         NUMERO_CANDIDATO, company_id, pretty_name, first_round_date, second_round_date,
         candidate_registry_date, partisan, hirer, imputed_ci, imputed_error,
         undecided) %>%
  group_split(year, CD_CARGO, SG_UE, turno, NR_IDENTIFICACAO_PESQUISA, company_id, estimulada) %>%
  map_dfr(function(x) {
    r = rbind(x, x[1,])
    
    r[nrow(r), 'result'] = first(r$undecided)
    r[nrow(r), 'NUMERO_CANDIDATO'] = 99
    
    r
  }) %>%
  ungroup()

polls %>% write.csv('data/preprocessed_polls.csv', row.names = F)
