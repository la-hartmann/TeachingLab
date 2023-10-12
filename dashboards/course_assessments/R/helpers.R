### Add Calibri Fonts
font_add(family = "Calibri", regular = "www/Calibri.ttf")
font_add(family = "Calibri Bold", regular = "www/Calibri Bold.ttf")
font_add(family = "Roboto", regular = "www/Roboto-Black.ttf")

### Knowledge Assessments Data ###
knowledge_assessments <- readr::read_rds("data/knowledge_assessments.rds")

### Knowledge Assessments Answers ###
knowledge_assessments_answers <- readRDS("data/know_assess_answers.rds")

### Chart function ###
plot_know_assess <- function(data,
                             title = "Knowledge Assessments % Correct Before/After",
                             color = c("#040404", "#04abeb"),
                             level = NULL) {
  if (is.null(level)) {
    plot <- data |>
      # mutate(know_assess = str_wrap(know_assess, width = 50)) |>
      ggplot(aes(x = prepost, y = percent, fill = prepost, tooltip = paste0(percent, "%"))) +
      geom_col_interactive(aes()) +
      geom_text(aes(label = paste0(percent, "% (n = ", n, ")"), color = prepost), vjust = -0.45, fontface = "bold") +
      facet_wrap_interactive(~know_assess,
        labeller = labeller_interactive(
          aes(
            tooltip = paste0("Click to see ", know_assess, " details!"),
            data_id = "level1_highlight",
            onclick = glue::glue(
              'Shiny.setInputValue("custom_bar_click", {clicked_level: "level2", drilled_place: "<str_remove_all(know_assess, " % Correct")>"})',
              .open = "<", .close = ">"
            )
          ),
          know_assess = label_wrap_gen(50)
        ),
        interactive_on = "both"
      ) +
      labs(title = NULL) +
      scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
      scale_fill_manual(values = color) +
      scale_color_manual(values = color) +
      theme_tl() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text_interactive(hjust = 0.5, face = "bold"),
        plot.title = element_text(face = "bold")
      )

    # 6 * length(unique(data$know_assess)) old code to determine height, reimplement when we have a better method for plotting multiple at once
    girafe(ggobj = plot, height_svg = 7.5, width_svg = 7.5) |>
      girafe_options(
        opts_tooltip(
          opacity = 0.7,
          offx = 20, offy = -10, use_fill = TRUE, use_stroke = TRUE
        )
      )
  } else if (level == "level2") {
    plot <- data |>
      ggplot(aes(x = prepost, y = percent, fill = prepost)) +
      geom_col_interactive(aes(
        tooltip = paste0(percent, "%")
      ), width = 0.75) +
      geom_text(aes(label = paste0(percent, "% (n = ", n, ")"), color = prepost), vjust = -0.4, fontface = "bold", size = 3) +
      facet_wrap_interactive(~question2,
        labeller = labeller_interactive(aes(
          tooltip = paste0("Click for \"", question2, "\" % selected!"),
          data_id = paste0("level2_highlight", question2),
          onclick = glue::glue(
            'Shiny.setInputValue("custom_bar_click", {clicked_level: "level3", drilled_place: "<str_replace_all(question2, "\n", " ")>"})',
            .open = "<", .close = ">"
          )
        )),
        interactive_on = "both"
      ) +
      labs(
        title = title,
        caption = label_interactive("← Back",
          data_id = "id_caption",
          tooltip = "Go back",
          onclick = 'Shiny.setInputValue("custom_bar_click", {clicked_level: null})'
        )
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
      scale_fill_manual(values = color) +
      scale_color_manual(values = color) +
      theme_tl() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text_interactive(hjust = 0.5, face = "bold", size = 10.5),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text_interactive(size = 11)
      )

    girafe(ggobj = plot, height_svg = 8.5, width_svg = 8.5) |>
      girafe_options(
        opts_tooltip(
          opacity = 0.7,
          offx = 20, offy = -10, use_fill = TRUE, use_stroke = TRUE
        )
      )
    # dplyr::group_by(question2, know_assess) |>
    # e_chart(x = prepost, reorder = FALSE) |>
    # e_bar(percent, color = color, colorBy = "data") |>
    # e_labels(formatter = htmlwidgets::JS("(params) => params.value[1] + '%';"), fontWeight = "bold", fontSize = 14) |>
    # e_axis(axis = "y", min = 0, max = 100, formatter = "{value}%") |>
    # e_labels(fontWeight = "bold", fontSize = 18,
    #          position = "top",
    #          verticalAlign = "top",
    #          formatter = htmlwidgets::JS("(params) => params.seriesName;")) |>
    # e_legend(show = FALSE, type = "plain", orient = "horizontal", top = "50", icon = "circle") |>
    # e_facet(
    #   rows = 3,
    #   # rows = length(unique(data$question2)),
    #   cols = 2,
    #   v_panel_space = 6
    # ) |>
    # e_title(text = title, left = "center", top = 10) |>
    # e_on(
    #   query = "series.bar",
    #   # Set input values
    #   handler = glue::glue(
    #     "function(params){
    #        Shiny.setInputValue(
    #         'custom_bar_click',
    #         {clicked_level: 'level3', drilled_place: params.name}, {priority: 'event'}
    #        );
    #      }",
    #     .open = "<<", .close = ">>"
    #   ),
    #   event = "click"
    # )
  } else if (level == "level3") {
    plot <- data |>
      ggplot(aes(x = fct_reorder(answer, percent, .desc = TRUE), y = percent, fill = factor(prepost))) +
      geom_col_interactive(aes(
        tooltip = prepost,
      ), position = position_dodge2(width = 0.75)) +
      geom_text(aes(label = paste0(round(percent, 0), "% (n = ", number_selected, ")"), color = prepost),
        vjust = -0.4,
        fontface = "bold",
        position = position_dodge(width = 0.75),
        size = 4.25,
        show.legend = FALSE
      ) +
      facet_wrap(~question2, scale = "free") +
      labs(
        title = NULL,
        caption = label_interactive("← Back",
          data_id = "id_caption",
          onclick = glue::glue('Shiny.setInputValue("custom_bar_click", {clicked_level: "level2", drilled_place: "<data$know_assess[1]>"})',
                               .open = "<", .close = ">"),
          tooltip = "Go back"
        )
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
      scale_fill_manual(values = color) +
      scale_color_manual(values = color) +
      theme_tl(legend = TRUE) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_markdown(size = 13.75),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.title = element_text(face = "bold", size = 20),
        panel.spacing = grid::unit(1, "lines"),
        plot.caption = element_text_interactive(size = 13),
        legend.position = c(0.97, 1.05),
        legend.title = element_blank()
      )

    girafe(ggobj = plot, height_svg = 9.5, width_svg = 12.5) |>
      girafe_options(
        opts_tooltip(
          opacity = 0.7,
          offx = 20, offy = -10, use_fill = TRUE, use_stroke = TRUE
        )
      )
    # e_chart(x = prepost, reorder = FALSE) |>
    # e_bar(percent, name = "Before", color = color, colorBy = "data") |>
    # e_labels(formatter = htmlwidgets::JS("(params) => params.value[1] + '%';"), fontWeight = "bold", fontSize = 14) |>
    # e_axis(axis = "y", min = 0, max = 100, formatter = "{value}%") |>
    # e_legend(bottom = "5%", show = FALSE) |>
    # e_title(text = title, left = "center", top = 10) |>
    # e_on(
    #   query = "series.bar",
    #   # Set input values
    #   handler = glue::glue(
    #     "function(params){
    #        Shiny.setInputValue(
    #         'custom_bar_click',
    #         {clicked_level: null, drilled_place: params.name}, {priority: 'event'}
    #        );
    #      }",
    #     .open = "<<", .close = ">>"
    #   ),
    #   event = "click"
    # )
  }
}
