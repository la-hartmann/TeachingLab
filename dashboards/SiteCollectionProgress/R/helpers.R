### Diagnostic Completion Dashboard ###

# We start with a random city in the back button and have a random city jump button
get_random_partner <- function(){ sample(unique_partners, 1) }

# Add a nicely styled and centered label above a given input
labeled_input <- function(id, label, input) {
  div(
    id = id,
    span(label, style = "font-size: small;"),
    input
  )
}

# Text grob with color matching current theme
themed_text <- function(text) {
  grid::textGrob(
    text,
    gp = grid::gpar(col = thematic::thematic_get_option("fg"))
  )
}

first_up <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}