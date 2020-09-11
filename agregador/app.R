# app.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
# 
# This file is licensed under the GNU General Public License, version 3.

library(tidyverse)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(cowplot)
library(plotly)
library(RColorBrewer)

Rcpp::sourceCpp('polling_average.cpp')
source('theme.R')
source('constants.R')

early_polls_2020 = read_csv('data/early_polls_2020.csv')
polls = read_csv('data/preprocessed_polls.csv')

cities = read_csv('data/cities.csv') %>%
  expand_grid(year = c(2012, 2014, 2016, 2018, 2020)) %>%
  semi_join(polls, by = c('SG_UE', 'year')) %>%
  mutate(order = case_when(
    SG_UE == '71072' ~ 1,
    SG_UE == '60011' ~ 2,
    SG_UE == '38490' ~ 3,
    SG_UE == '13897' ~ 4,
    SG_UE == '41238' ~ 5,
    SG_UE == '02550' ~ 6,
    SG_UE == '75353' ~ 7,
    SG_UE == '25313' ~ 8,
    SG_UE == '93734' ~ 9,
    SG_UE == '04278' ~ 10,
    SG_UE == '88013' ~ 11,
    SG_UE == '09210' ~ 12,
    SG_UE == '27855' ~ 13,
    SG_UE == '90514' ~ 14,
    SG_UE == '17612' ~ 15,
    SG_UE == '12190' ~ 16,
    SG_UE == '20516' ~ 17,
    SG_UE == '31054' ~ 18,
    SG_UE == '90670' ~ 19,
    SG_UE == '00035' ~ 20,
    SG_UE == '06050' ~ 21,
    SG_UE == '81051' ~ 22,
    SG_UE == '03018' ~ 23,
    SG_UE == '01392' ~ 24,
    SG_UE == '57053' ~ 25,
    SG_UE == '73440' ~ 26,
    T ~ Inf
  ))

place_cargo_opts = c('BR-1', cities %>% distinct(SG_UF) %>% filter(SG_UF != 'BR') %>% pull() %>% paste0('-3'))

cities = cities %>% arrange(order)

general_elections = tibble(
  place_cargo = place_cargo_opts,
  labels = c(
    'Brasil - Presidente',
    'Acre - Governador',
    'Alagoas - Governador',
    'Amazonas - Governador',
    'Amapá - Governador',
    'Bahia - Governador',
    'Ceará - Governador',
    'Espírito Santo - Governador',
    'Goiás - Governador',
    'Maranhão - Governador',
    'Minas Gerais - Governador',
    'Mato Grosso do Sul - Governador',
    'Mato Grosso - Governador',
    'Pará - Governador',
    'Paraíba - Governador',
    'Pernambuco - Governador',
    'Piauí - Governador',
    'Paraná - Governador',
    'Rio de Janeiro - Governador',
    'Rio Grande do Norte - Governador',
    'Rondônia - Governador',
    'Roraima - Governador',
    'Rio Grande do Sul - Governador',
    'Santa Catarina - Governador',
    'Sergipe - Governador',
    'São Paulo - Governador',
    'Tocantins - Governador',
    'Distrito Federal - Governador'
  )
)

rating = read_csv('data/pollster_rating_2020_final.csv')

candlist_old = read_csv('data/pindograma_candlist.csv') %>%
  mutate(short = str_replace_all(NOME_URNA_CANDIDATO, 'PROFESSOR', 'PROF. ')) %>%
  mutate(short = str_replace_all(short, '\\(.*?\\)', '')) %>%
  mutate(short = str_squish(short)) %>%
  mutate(short = ifelse(nchar(short) >= 21, word(short, start = 1, end = -2), short)) %>%
  mutate(short = ifelse(nchar(short) >= 21, word(short, start = 1, end = -2), short)) %>%
  mutate(NOME_URNA_CANDIDATO = str_squish(short)) %>%
  left_join(party_palette, by = c('NUMERO_CANDIDATO' = 'party'))

brew_cand_colors = function(n) {
  if (n < 3) {
    brks = if (n == 1) c('#9e0142') else c('#9e0142', '#3288bd')
  } else if (n <= 11) {
    brks = brewer.pal(n, 'Spectral')
  } else {
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    brks = sample(col_vector, n)
  }
  
  brks
}

candlist20 = early_polls_2020 %>%
  rename(ANO_ELEICAO = year, SIGLA_UE = SG_UE, CODIGO_CARGO = CD_CARGO) %>%
  rename(NOME_URNA_CANDIDATO = candidate) %>%
  distinct(ANO_ELEICAO, SIGLA_UE, CODIGO_CARGO, NOME_URNA_CANDIDATO, NUMERO_CANDIDATO) %>%
  mutate(short = str_replace_all(NOME_URNA_CANDIDATO, 'PROFESSOR', 'PROF. ')) %>%
  mutate(short = str_replace_all(short, '\\(.*?\\)', '')) %>%
  mutate(short = str_squish(short)) %>%
  mutate(short = ifelse(nchar(short) >= 21, word(short, start = 1, end = -2), short)) %>%
  mutate(short = ifelse(nchar(short) >= 21, word(short, start = 1, end = -2), short)) %>%
  mutate(NOME_URNA_CANDIDATO = str_squish(short)) %>%
  group_by(SIGLA_UE) %>%
  mutate(party_name = '?', party = '?') %>%
  mutate(party_color = brew_cand_colors(n())) %>%
  ungroup() %>%
  mutate(NUM_TURNO = 1)

fake_candlist = crossing(
  ANO_ELEICAO = c(2012, 2014, 2016, 2018, 2020),
  SIGLA_UE = cities$SG_UE,
  CODIGO_CARGO = c(1, 3, 11),
  NUM_TURNO = c(1, 2)
) %>%
  mutate(NUMERO_CANDIDATO = 99, NOME_URNA_CANDIDATO = 'BRANCOS / NULOS / OUTROS') %>%
  left_join(party_palette, by = c('NUMERO_CANDIDATO' = 'party'))

candlist = bind_rows(candlist_old, candlist20, fake_candlist)

prepare_chart_data = function(yr, city, rnd, cargo = 11) {
  wma_n = 5
  
  pollz = polls %>%
    filter(year == yr & polled_UE == city & CD_CARGO == cargo & turno == rnd) %>%
    mutate(applied_weight = 1) %>%
    group_by(NUMERO_CANDIDATO) %>%
    mutate(first_poll_date = min(DT_FIM_PESQUISA)) %>%
    ungroup()
  
  dat = pollz %>%
    group_by(DT_FIM_PESQUISA, NUMERO_CANDIDATO, first_round_date, second_round_date, first_poll_date) %>%
    summarize(
      day_average = weighted.mean(result, applied_weight, na.rm = T),
      day_weight = sum(applied_weight, na.rm = T)
    ) %>%
    ungroup()
  
  start_date = min(if_else(rnd == 1, min(dat$first_poll_date), first(dat$first_round_date)), today())
  end_date = min(if_else(rnd == 1, first(dat$first_round_date), first(dat$second_round_date)), today())
  
  days_df = expand_grid(
    date = seq(start_date, end_date, by = 'days'),
    NUMERO_CANDIDATO = pull(distinct(dat, NUMERO_CANDIDATO))
  ) %>%
    mutate(year = yr, CD_CARGO = cargo, polled_UE = city, turno = rnd) %>%
    left_join(dat %>% select(-first_poll_date), by = c('date' = 'DT_FIM_PESQUISA', 'NUMERO_CANDIDATO' = 'NUMERO_CANDIDATO')) %>%
    left_join(dat %>% distinct(NUMERO_CANDIDATO, first_poll_date), by = c('NUMERO_CANDIDATO' = 'NUMERO_CANDIDATO')) %>%
    group_by(NUMERO_CANDIDATO) %>%
    mutate(go_back = case_when(
      as.numeric(date - first_poll_date) < wma_n ~ as.numeric(which.min(is.na(day_average))),
      as.numeric(date - first_poll_date) >= wma_n ~ as.numeric(row_number() - (wma_n - 1))
    )) %>%
    ungroup() %>%
    group_by(date) %>%
    mutate(day_average = ifelse(date < first_poll_date, NA, ifelse(is.na(day_average) & any(!is.na(day_average)), 0, day_average))) %>%
    mutate(day_weight = ifelse(date < first_poll_date, NA, ifelse(is.na(day_weight) & any(!is.na(day_weight)), mean(day_weight, na.rm = T), day_weight))) %>%
    ungroup()
  
  list(
    pollz,
    days_df %>%
      mutate(imputed = is.na(day_average)) %>%
      group_split(NUMERO_CANDIDATO) %>%
      map_dfr(function(x) fillEmptyDays(x, first(x$first_poll_date))) %>%
      tibble() %>%
      group_by(NUMERO_CANDIDATO) %>%
      mutate(final_average = calculateFinalAverage(day_average, day_weight, imputed, wma_n, T, 0.9, 0.8)) %>%
      ungroup()
    )
}

show_city_chart = function(yr, city, rnd, cargo = 11) {
  d = prepare_chart_data(yr, city, rnd, cargo)
  
  d1_with_names = d[[1]] %>%
    left_join(candlist, by = c(
      'year' = 'ANO_ELEICAO',
      'polled_UE' = 'SIGLA_UE',
      'CD_CARGO' = 'CODIGO_CARGO',
      'NUMERO_CANDIDATO' = 'NUMERO_CANDIDATO',
      'turno' = 'NUM_TURNO'
    )) %>%
    mutate(NOME_URNA_CANDIDATO = str_to_title(paste0(NOME_URNA_CANDIDATO, '  ')))
  
  d2_with_names = d[[2]] %>%
    left_join(candlist, by = c(
      'year' = 'ANO_ELEICAO',
      'polled_UE' = 'SIGLA_UE',
      'CD_CARGO' = 'CODIGO_CARGO',
      'NUMERO_CANDIDATO' = 'NUMERO_CANDIDATO',
      'turno' = 'NUM_TURNO'
    )) %>%
    mutate(NOME_URNA_CANDIDATO = str_to_title(paste0(NOME_URNA_CANDIDATO, '  '))) %>%
    group_by(NUMERO_CANDIDATO) %>%
    mutate(NOME_URNA_CANDIDATO = ifelse(n_distinct(NOME_URNA_CANDIDATO) == 1, NOME_URNA_CANDIDATO, paste0('Candidato(a) do ', party_name, '  '))) %>%
    ungroup()
  
  lbls = unique(d2_with_names$NOME_URNA_CANDIDATO)
  brks = unique(d2_with_names$party_color)
  
  crd = first(d1_with_names$candidate_registry_date)
  plot = ggplot(d2_with_names, aes(
    x = date,
    y = final_average,
    color = factor(party_color), group = 1,
    text = paste0('<b>', str_squish(NOME_URNA_CANDIDATO), '</b>: ', round(final_average), '%')
  )) +
    geom_line(size = 0.5) +
    geom_point(data = d1_with_names, aes(x = DT_FIM_PESQUISA, y = result, text = NA), alpha = 0.35, shape = 19, size = 1) +
    xlab('') + ylab('') +
    scale_color_identity(guide = 'legend', breaks = brks, labels = lbls) +
    scale_y_continuous(labels = function(x) paste0(x, '%')) +
    geom_vline(xintercept = as.numeric(crd), linetype = 'dashed', color = pg_dark_gray) +
    theme_pindograma() +
    theme(legend.title = element_blank()) +
    theme(legend.position = 'bottom') +
    theme(legend.text = element_text(size = 14))
  
  list(plot, d[[1]])
}

ui <- fixedPage(
  includeCSS('pindograma.css'),
  includeCSS('styles.css'),
  includeCSS('Fantasque.css'),
  useShinyjs(),
  
  tags$head(
    HTML('<base target="_parent">'),
    includeHTML('seo.html'),
    tags$script(src = 'update_window_size.js')
  ),
  
  fixedRow(
    column(3,
      class = 'col-lg-2 col-xs-12 img-responsive',
      tags$a(href = 'https://pindograma.com.br',
        tags$img(src = 'LOGO.jpg', class = 'pindograma-logo')
      )
    ),
    
    column(9,
      class = 'col-lg-10 hidden-xs',
      includeHTML('header.html')
    )
  ),
  
  fixedRow(
    column(3,
      class = 'col-xs-12 col-md-4 col-xl-3 text-center',
      tags$h4('agregador de pesquisas', class = 'pindograma-agg-title')
    )
  ),
  
  fixedRow(
    style = 'margin-top: 20px;',
    
    column(5,
      class = 'col-xs-12 col-md-4 col-xl-3',
      
      tags$div('Escolha uma eleição:', class = 'pindograma-form-prefix blue'),
      
      radioGroupButtons(
        'current_or_not',
        choices = c('Anteriores', '2020'),
        selected = '2020',
        justified = T),
      
      hidden(radioGroupButtons(
        'year',
        choices = c('2012', '2014', '2016', '2018'),
        selected = '2018',
        justified = T)),
      
      selectizeInput(
        'place_cargo', NULL,
        choices = c(' ' = ''),
        width = '100%',
        options = list(
          placeholder = 'Digite ou selecione uma cidade...',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      
      hidden(radioGroupButtons(
        'round',
        choices = setNames(c(1, 2), c('1º Turno', '2º Turno')),
        selected = 2,
        justified = T))
    ),
  ),
  
  fixedRow(
    column(12,
      plotlyOutput('aggregatorPlot', width = '100%')
    )
  ),
  
  fixedRow(
    column(12,
      class = 'visible-xs-block',
      uiOutput('legend_copy.ui')
    )
  ),
  
  fixedRow(style='overflow-x: auto; padding-top: 30px;',
    column(12, align = 'center',
      tableOutput('table')
    )
  ),
  
  fixedRow(
    column(12,
      tags$div(HTML(nota_string), class = 'pindograma-big-footer')
    )
  ),
  
  fixedRow(
    column(12, class = 'pindograma-small-footer',
      hidden(textOutput('contractors'))
    )
  ),
  
  fixedRow(
    tags$hr(),
    tags$div(class = 'pindograma-footer-container',
      tags$p(HTML(credits), class = 'pindograma-footer')
    ),
  ),
  
  includeHTML('footer.html')
)

server <- function(input, output, session) {
  values = reactiveValues(selected_place_cargo = F, selected_year = 2020)
  
  observeEvent(input$current_or_not, {
    req(input$current_or_not, input$year)
    
    print('curr')
    
    if (input$current_or_not == '2020') {
      values$selected_year = 2020
      hideElement('year')
    } else {
      values$selected_year = as.double(input$year)
      showElement('year')
    }
    
    if (values$selected_year %in% mayor_years) {
      place_cargo_options = c(c(' ' = ''), setNames(
        cities %>% filter(year == values$selected_year) %>% pull(SG_UE),
        cities %>% filter(year == values$selected_year) %>% pull(final_name))
      )
    } else {
      place_cargo_options = c(c(' ' = ''), setNames(general_elections$place_cargo, general_elections$labels))
    }
    
    values$selected_place_cargo = F
    updateSelectizeInput(session, 'place_cargo', choices = place_cargo_options, selected = NULL)
    
    output$aggregatorPlot = renderPlotly({})
    output$legend_copy.ui = renderUI({})
    output$table = renderTable({})
    hideElement('round')
    hideElement('contractors')
  })
  
  observeEvent(input$year, {
    req(input$current_or_not, input$year)
    
    if (input$current_or_not == '2020') {
      values$selected_year = 2020
    } else {
      values$selected_year = as.double(input$year)
    }
    
    if (values$selected_year %in% mayor_years) {
      place_cargo_options = c(c(' ' = ''), setNames(
        cities %>% filter(year == values$selected_year) %>% pull(SG_UE),
        cities %>% filter(year == values$selected_year) %>% pull(final_name))
      )
    } else {
      place_cargo_options = c(c(' ' = ''), setNames(general_elections$place_cargo, general_elections$labels))
    }
    
    values$selected_place_cargo = F
    updateSelectizeInput(session, 'place_cargo', choices = place_cargo_options, selected = NULL)
    
    output$aggregatorPlot = renderPlotly({})
    output$legend_copy.ui = renderUI({})
    output$table = renderTable({})
    hideElement('round')
    hideElement('contractors')
  })
  
  observeEvent(input$place_cargo, {
    req(input$year, input$place_cargo)
    
    if (values$selected_year %in% mayor_years) {
      cargo = 11
      place = input$place_cargo
    } else {
      cargo = as.double(word(input$place_cargo, 2, sep = '-'))
      place = word(input$place_cargo, 1, sep = '-')
    }
    
    number_of_rounds = polls %>%
      filter(year == values$selected_year & CD_CARGO == cargo & polled_UE == place) %>%
      distinct(turno) %>%
      nrow()
    
    number_of_rounds = polls %>%
      filter(year == values$selected_year & CD_CARGO == cargo & polled_UE == place) %>%
      distinct(turno) %>%
      nrow()
    
    if (number_of_rounds != 1) {
      showElement('round')
    } else {
      hideElement('round')
    }
    
    values$selected_place_cargo = T
  })
  
  observeEvent(c(input$width, input$year, input$place_cargo, input$round), {
    req(input$width, input$year, input$place_cargo)
    
    if (!values$selected_place_cargo) {
      return()
    }
    
    if (values$selected_year %in% mayor_years) {
      cargo = 11
      place = input$place_cargo
    } else {
      cargo = as.double(word(input$place_cargo, 2, sep = '-'))
      place = word(input$place_cargo, 1, sep = '-')
    }
    
    number_of_rounds = polls %>%
      filter(year == values$selected_year & CD_CARGO == cargo & polled_UE == place) %>%
      distinct(turno) %>%
      nrow()
    
    if (number_of_rounds != 1) {
      req(input$round)
      round = as.double(input$round)
    } else {
      round = 1
    }
    
    plot_ = show_city_chart(values$selected_year, place, round, cargo)
    
    plot = plot_[[1]]
    
    our_polls = plot_[[2]]
    number_cands = length(unique(our_polls$NUMERO_CANDIDATO))
    
    output$aggregatorPlot = renderPlotly({
      ggplotly(plot, tooltip = 'text') %>%
        config(displayModeBar = F) %>%
        layout(
          showlegend = F,
          hovermode = 'x unified',
          hoverlabel = list(font = list(size = 12)),
          xaxis = list(
            fixedrange = T,
            tickfont = list(size = 14),
            linewidth = 4,
            tickwidth = 4
          ),
          yaxis = list(
            fixedrange = T,
            tickfont = list(size = 14),
            tickwidth = 4,
            ticklen = 8
          ),
          spikedistance = -1,
          annotations = list(
            x = as.numeric(first(our_polls$candidate_registry_date)) - 3,
            xref = 'x',
            y = 0,
            yref = 'paper',
            text = 'prazo final para registro de candidaturas',
            textangle = -90,
            font = list(size = 10, color = pg_dark_gray),
            showarrow = F
          )
        ) %>%
        style(hoverinfo = 'skip', traces = (number_cands + 1):(number_cands * 2))
      })
    
    legend_height = 20 * number_cands / input$width
    
    if (input$width < 800) {
      output$legendPlot_copy = renderPlot({
        plot_grid(get_legend(
          plot + guides(col = guide_legend(ncol = input$width))
        ))
      })
      output$legend_copy.ui = renderUI({
        plotOutput('legendPlot_copy', height = legend_height)
      })
    } else {
      output$legend_copy.ui = renderUI({})
    }
    
    our_polls_pre_output = our_polls %>%
      left_join(candlist %>% filter(ANO_ELEICAO == values$selected_year & SIGLA_UE == place & CODIGO_CARGO == cargo & NUM_TURNO == round), by = c(
        'NUMERO_CANDIDATO' = 'NUMERO_CANDIDATO'
      )) %>%
      left_join(rating %>% select(-pretty_name), by = 'company_id') %>%
      mutate(NOME_URNA_CANDIDATO = str_to_title(NOME_URNA_CANDIDATO)) %>%
      filter(ANO_ELEICAO != 2018 | SG_UE != 'BR' | NUMERO_CANDIDATO != 13 | (NOME_URNA_CANDIDATO == 'Lula' & estimulada == 0) | (NOME_URNA_CANDIDATO == 'Fernando Haddad' & (estimulada == 1 | is.na(estimulada)))) %>% # URGENT FIXME
      pivot_wider(
        c(NR_IDENTIFICACAO_PESQUISA, DT_FIM_PESQUISA, DT_INICIO_PESQUISA,
          self_hired, partisan, pretty_name, imputed_ci, imputed_error, grade, hirer, estimulada),
        names_from = NOME_URNA_CANDIDATO,
        values_from = result,
        names_prefix = 'cand_') %>%
      arrange(desc(DT_FIM_PESQUISA)) %>%
      mutate(DT_INICIO_PESQUISA = tolower(format(DT_INICIO_PESQUISA, '%d/%b'))) %>%
      mutate(DT_FIM_PESQUISA = tolower(format(DT_FIM_PESQUISA, '%d/%b'))) %>%
      mutate(NR_IDENTIFICACAO_PESQUISA = paste0(
        substring(NR_IDENTIFICACAO_PESQUISA, 1, 2), '-',
        substring(NR_IDENTIFICACAO_PESQUISA, 3, 7), '/',
        substring(NR_IDENTIFICACAO_PESQUISA, 8, 12))) %>%
      mutate(Empresa = paste0(
        'pindograma-flex-container pindograma-div',
        pretty_name,
        'pindograma-sorta-small',
        case_when(
          partisan ~ hired_string,
          self_hired ~ probably_hired_string,
          T ~ ''
        ),
        'pindograma-dend',
        'pindograma-small', 'Número TSE: ', NR_IDENTIFICACAO_PESQUISA, '\n',
        'Margem de Erro: ', imputed_error, '\n',
        'Intervalo de Confiança: ', imputed_ci,
        'pindograma-dend pindograma-dend',
        'pindograma-cnt', 'NOTA', 'pindograma-grade', grade, 'pindograma-dend',
        'pindograma-dend pindograma-dend'
      )) %>%
      mutate(Data = paste0(DT_INICIO_PESQUISA, ' - ', DT_FIM_PESQUISA)) %>%
      mutate(Tipo = ifelse(estimulada == 0, 'Espontânea', 'Estimulada')) %>%
      mutate_if(is.numeric, function(x) round(x)) %>%
      mutate_if(is.numeric, function(x) ifelse(is.na(x), '', paste0(x, '%')))
    
    our_polls_output = our_polls_pre_output %>%
      select(Empresa, Data, Tipo, contains('cand'))
    
    diagonalize = function(x) {
      paste0(
        '<div class="rotated-header-container"><div class="rotated-header-content">',
        str_squish(x),
        '</div></div>'
      ) %>%
        str_replace('cand_', '')
    }
    
    output$table = renderTable(
      { our_polls_output },
      sanitize.colnames.function = function(x) {
        ifelse(startsWith(x, 'cand_'), diagonalize(x), x)
      },
      sanitize.text.function = function(x) {
        x %>%
          str_replace_all('\\n', '<br>') %>%
          str_replace_all('pindograma-div', '<div>') %>%
          str_replace_all('pindograma-dend', '</div>') %>%
          str_replace_all('pindograma-small', '<div class="pindograma-small">') %>%
          str_replace_all('pindograma-sorta-small', '<div class="pindograma-sorta-small">') %>%
          str_replace_all('pindograma-flex-container', '<div class="pindograma-flex-container">') %>%
          str_replace_all('pindograma-grade', '<div class="pindograma-grade">') %>%
          str_replace_all('pindograma-cnt', '<div class="pindograma-grade-container">')
      }
    )
    
    output$contractors = renderText({
      paste0(contractor_string, paste(pmap_chr(list(
        our_polls_pre_output$pretty_name,
        our_polls_pre_output$Data,
        our_polls_pre_output$hirer
      ), function(x, y, z) {
        paste0('Pesquisa ', x, ' (', y, '): ', z, '.')
      }), collapse = ' '))
    })
    
    showElement('contractors')
  })
}

shinyApp(ui = ui, server = server)
