# theme.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
# 
# This file is licensed under the GNU General Public License, version 3.

pg_dark_gray = '#6e787f'
pg_light_gray = '#dce9ef'
pg_yellow = '#f4cb1c'
pg_green = '#8db33a'
pg_blue = '#6c82a0'
pg_orange = '#d27103'

theme_pindograma = function() {
  theme_classic() +
    theme(axis.line.x = element_line(color = pg_dark_gray, size = 2)) +
    theme(axis.line.y = element_blank()) +
    theme(axis.ticks.length.x = unit(.35, 'cm')) +
    theme(axis.ticks.length.y = unit(.25, 'cm')) +
    theme(axis.ticks = element_line(color = pg_dark_gray, size = 1.25)) +
    theme(text = element_text(family = 'Fantasque Sans Mono')) +
    theme(axis.text = element_text(size = 16)) +
    theme(axis.title = element_text(size = 14, color = pg_dark_gray)) +
    theme(plot.title = element_text(size = 18, color = pg_dark_gray))
}

theme_pindograma_table = function(gt_table) {
  gt_table %>%
    tab_style(list(
      'vertical-align: middle',
      cell_borders(
        sides = c('left', 'right'),
        color = "#ffffff",
        weight = px(1.5),
        style = "solid"
      ),
      cell_text(font = 'Fantasque Sans Mono', size = px(17))
    ), cells_column_labels(everything())) %>%
    tab_style(cell_fill(color = '#ffffff'), cells_stubhead()) %>%
    tab_style(cell_text(font = 'Helvetica', weight = 'bold'), cells_title()) %>%
    tab_style(list(
      cell_text(font = 'Fantasque Sans Mono', size = px(15)),
      cell_fill(color = pg_yellow),
      cell_borders(
        sides = c('left', 'right'),
        color = "#ffffff",
        weight = px(1.5),
        style = "solid"
      )
    ), cells_body()) %>%
    tab_options(
      table.border.top.width = 0,
      column_labels.border.bottom.color = '#ffffff',
      column_labels.border.top.color = '#ffffff',
      column_labels.background.color = pg_orange,
      table_body.hlines.color = '#ffffff',
      table_body.vlines.color = '#ffffff',
      table_body.border.bottom.color = '#ffffff',
      table_body.border.top.color = '#ffffff',
      table_body.vlines.width = px(2),
      stub.background.color = pg_blue,
      stub.border.color = '#ffffff',
      table.border.top.color = '#ffffff',
      heading.border.bottom.style = '#ffffff'
    )
}

theme_pindograma_table_stub = function(gt_table) {
  gt_table %>%
    tab_style(cell_text(font = 'Fantasque Sans Mono', size = px(15)), cells_stub())
}

theme_pindograma_table_grand_summary = function(gt_table) {
  gt_table %>%
    tab_style(cell_text(font = 'Fantasque Sans Mono', size = px(15)), cells_grand_summary()) %>%
    tab_style(cell_text(font = 'Fantasque Sans Mono', size = px(15)), cells_row_groups())
}

pg_dot_palette = c(pg_orange, pg_green, pg_blue, pg_yellow)