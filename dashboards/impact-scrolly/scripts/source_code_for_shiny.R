library(ggplot2)
library(ggthemes)
library(tidyverse)
library(knitr)
library(shiny)
library(shinyjs)
library(metathis)
library(TeachingLab)
library(bookdown)
library(tidytext)
library(ggtext)
library(ggiraph)
# devtools::install_github("statistiekcbs/scrollytell")
library(scrollytell)

# data <- readr::read_csv("data/final_data.csv")
data_ipg <- readr::read_rds("data/final_data.rds")
data_lafayette <- read_rds("data/lafayette_data.rds")

### FUNCTIONS & TEXT

longdiv <- function(...){
  div(
    ...,
    class = "container",
    style = "height:100vh"
  )
}

render_text <- function(num){
  
  div(
    text(num), class = "text"
  )
  
}

text <- function(num){
  p(
    switch(num,
           text1,
           text2,
           text3,
           text4,
           text5,
           text6,
           text7,
           text8
    )
  )
}

text0 <- HTML("<span style='font-size:20px'> How has Teaching Lab Professional Learning Impacted Student Understanding? </span>
              <br><br> 
              <p> In 2019-2020 there were 245 observations, while in 2020-2021 there were 22 observations from the IPG forms. 
              Looking at this by school, we can also see that there are very few sites with more than 50 observations, or schools with over 5 
              observations.")

text1 <- HTML("<H2>Questions and tasks address the text by attending to its particular qualitative features: its meaning/purpose and/or language, structure(s), or knowledge demands. </H2>
              <br> <p> Ratings offered included 
              <br>
              <font color='#04abeb'>1- Questions and tasks do not attend to the qualitative features of the text to build understanding,
              <br>2- Few questions and tasks attend to the qualitative features of the text to build understanding,
              <br>
              3- Many questions and tasks attend to the qualitative features of the text to build understanding,
              <br>
              4- Most questions and tasks attend to the qualitative features of the text to build understanding</font><p>")

text2 <- HTML("<H2>Questions and tasks require students to use evidence from the text to demonstrate understanding and to support their ideas about the text. </H2>
              <br> <p> Ratings offered included 
              <br>
              <font color='#04abeb'>1- Questions and tasks can be answered without evidence from the text,
              <br>2- Few questions and tasks require students to cite evidence from the text,
              <br>
              3- Many questions and tasks require students to cite evidence from the text,
              <br>
              4- Most questions and tasks require students to cite evidence from the text</font><p>")

text3 <- HTML("<H2>Questions and tasks attend to the words (academic vocabulary), phrases, and sentences within the text.</H2>
              <br> <p> Ratings offered included 
              <br>
              <font color='#04abeb'>1- No questions and tasks focus students on the words that matter most and how they are used in the text,
              <br>2- Vocabulary questions and tasks rarely focus students on the words that matter most and how they are used in the text,
              <br>
              3- Vocabulary questions and tasks mostly focus students on the words that matter most and how they are used in the text,
              <br>
              4- Vocabulary questions and tasks consistently focus students on the words, phrases, and sentences that matter most and how they are used in the text</font><p>")

text4 <- HTML("<H2>Questions and tasks are sequenced to build knowledge by guiding students to delve deeper into the text and graphics. </H2>
              <br> <p> Ratings offered included 
              <br>
              <font color='#04abeb'>
              1- Questions and tasks seem random and are not intentionally sequenced to support building knowledge,
              <br>
              2- Few questions and tasks are intentionally sequenced to support building knowledge,
              <br>
              3- Some questions and tasks are intentionally sequenced to support building knowledge,
              <br>
              4- Most questions and tasks are intentionally sequenced to support building knowledge
              </font><p>")

text5 <- HTML("<H2>The teacher poses questions and tasks for students to do the majority of the work: speaking/listening, reading, and/or writing; Students do the majority of the work of the lesson.</H2>
              <br> <p> Ratings offered included 
              <br>
              <font color='#04abeb'>
              1- Teacher provides few or no opportunities and few or very few students take the opportunities provided,
              <br>
              2- Teacher provides some opportunities, and some students take them,
              <br>
              3- Teacher provides many opportunities, and some students take them; or teacher provides some opportunities and most students take them,
              <br>
              4- Teacher provides many opportunities, and most students take them
              </font><p>")

text6 <- HTML("<H2>The teacher cultivates reasoning and meaning making by allowing students to productively struggle; Students persevere through difficulty.</H2>
              <br> <p> Ratings offered included 
              <br>
              <font color='#04abeb'>
              1- Teacher provides few or no opportunities and few or very few students take the opportunities provided,
              <br>
              2- Teacher provides some opportunities, and some students take them,
              <br>
              3- Teacher provides many opportunities, and some students take them; or teacher provides some opportunities and most students take them,
              <br>
              4- Teacher provides many opportunities, and most students take them
              </font><p>")

text7 <- HTML("<H2>The teacher expects evidence and precision from students and probes students’ answers accordingly; Students provide text evidence to support their ideas and display precision in their oral and/or written responses.</H2>
              <br> <p> Ratings offered included 
              <br>
              <font color='#04abeb'>
              1- Teacher provides few or no opportunities and few or very few students take the opportunities provided,
              <br>
              2- Teacher provides some opportunities, and some students take them,
              <br>
              3- Teacher provides many opportunities, and some students take them; or teacher provides some opportunities and most students take them,
              <br>
              4- Teacher provides many opportunities, and most students take them
              </font><p>")

text8 <- HTML("<H2>The teacher creates the conditions for student conversations where students are encouraged to talk about each other’s thinking; Students talk and ask questions about each other’s thinking, in order to clarify or improve their understanding. </H2>
              <br> <p> Ratings offered included 
              <br>
              <font color='#04abeb'>
              1- Teacher provides few or no opportunities and few or very few students take the opportunities provided,
              <br>
              2- Teacher provides some opportunities, and some students take them,
              <br>
              3- Teacher provides many opportunities, and some students take them; or teacher provides some opportunities and most students take them,
              <br>
              4- Teacher provides many opportunities, and most students take them
              </font><p>")

text9 <- HTML("The teacher deliberately checks for understanding throughout the lesson and adapts the lesson according to student understanding; When appropriate, students refine written and/or oral responses. </H2>
              <br> <p> Ratings offered included 
              <br>
              <font color='#04abeb'>1- Questions and tasks can be answered without evidence from the text,
              <br>2- Few questions and tasks require students to cite evidence from the text,
              <br>
              3- Many questions and tasks require students to cite evidence from the text,
              <br>
              4- Most questions and tasks require students to cite evidence from the text</font><p>")

text10 <- HTML("<H2>When appropriate, the teacher explicitly attends to strengthening students’ language and reading foundational skills; Students demonstrate use of language conventions and decoding skills, activating such strategies as needed to read, write, and speak with grade-level fluency and skill. </H2>
              <br> <p> Ratings offered included 
              <br>
              <font color='#04abeb'>1- Questions and tasks can be answered without evidence from the text,
              <br>2- Few questions and tasks require students to cite evidence from the text,
              <br>
              3- Many questions and tasks require students to cite evidence from the text,
              <br>
              4- Most questions and tasks require students to cite evidence from the text</font><p>")

# concludingtext <- HTML("<p><span style='font-size:24px'><b>The Risk of Automation</b></span>
#                         <br>
#                             <span style='font-size:18px'>This data led researchers Carl Frey and Michael Osborne to predict that 47% of jobs are at serious risk of automation over the next couple decades.
#                         <br>
#                             <br>The visuals above suggest that the ills of automation may not be evenly distributed across jobs.
#                             Less educated workers are more likely to face job loss as a product of automation. Those with high school diplomas or less find themself concentrated near the top of the y-axis, while those with bachelor’s degrees or higher face a lower risk of automation.
#                         <br>
#                             <br>A job’s salary is also predictive of automation probability. As the median income of a profession increases, the likelihood of automation displacing its workers decreases.
#                             This could suggest that automation will increasingly bifurcate the already divided labor market, making those at the top wealthier at the expense of the worse-off.
#                         <br>
#                             <br>Automation’s impact on work necessitates a policy response. The fact that automation will have different effects on different industries and different workers is a reminder that this public policy will have to be strategic and thoughtful.</span></p>")
# 
# technicalnotes <- HTML("<p>
#                 <span style='font-size:18px'><i>Technical Notes</i></span><br>
#                 <br>
#                 <span style='font-size:12px'>
#                 To learn more about how I made this app, please see the <a href='https://connorrothschild.github.io/r/automation-scrollytell/' target='_blank'>accompanying blog post</a>
#                 <br>
#                 Employment and education data comes from the
#                 <a href='https://www.bls.gov/emp/documentation/education-training-system.htm' target='_blank'>Bureau of Labor Statistics</a>. 
#                 <br>
#                 Employment and income data also comes from the <a href='https://www.bls.gov/oes/current/oes_nat.htm#11-0000' target='_blank'>BLS</a>.
#                 <br>
#                 Data on occupation and the risk of automation comes from <a href='https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf' target='_blank'>Frey and Osborne (2013)</a>. 
#                 <br>
#                 <br>
#                 Education is coded as typical education, meaning that the coded variable corresponds to the level of education that is most prevalent within a given occupation.
#                 If 51% of accountants hold a bachelor's degree, their typical education will be coded as such.
#                 Summary statistics for each level of education are calculated via the weighted mean of each occupation given its number of workers.
#                 <br>
#                 <br>
#                 For more information on the technical details of this analysis, please see the <a href='https://connorrothschild.github.io/r/automation/' target='_blank'>accompanying blog post</a>. 
#                 <br>
#                 <br>
#                 The R packages powering this site include 
#                 <a href='https://www.tidyverse.org/' target='_blank'>tidyverse</a>,
#                 <a href='http://shiny.rstudio.com/' target='_blank'>shiny</a>,
#                 <a href='https://github.com/ropensci/plotly' target='_blank'>plotly</a>, and 
#                 <a href='https://github.com/statistiekcbs/scrollytell' target='_blank'>scrollytell</a>.
#                 </span>
#                 </p>")

concludingtext <- HTML("<p><span style='font-size:24px'><b>In Sum</b></span>
                        <br>The influence of Teaching Lab Professional Learning...</span></p>")

technicalnotes <- HTML("<p>
                <span style='font-size:18px'><i>Thank You</i></span><br>
                <br>
                <span style='font-size:12px'>
                Thank you for viewing this presentation!
                <br>
                <br>
                The R packages powering this site include 
                <a href='https://www.tidyverse.org/' target='_blank'>tidyverse</a>,
                <a href='http://shiny.rstudio.com/' target='_blank'>shiny</a>,
                <a href='https://github.com/ropensci/plotly' target='_blank'>plotly</a>, and 
                <a href='https://github.com/statistiekcbs/scrollytell' target='_blank'>scrollytell</a>.
                </span>
                </p>")


data_time_ela_n <- data_lafayette %>%
  rename_with( ~ str_remove_all(.x, "\\.\\.\\.[:digit:][:digit:]")) %>%
  group_by(`Timeline of Obs`) %>%
  summarise(across(!c(1:3), ~ sum(!is.na(as.numeric(.x)), na.rm = T))) %>%
  pivot_longer(!`Timeline of Obs`) %>%
  mutate(`Timeline of Obs` = factor(`Timeline of Obs`, levels = c("Summer 2019", "Fall 2019", "Winter 2020", "Spring 2020",
                                                                  "Winter 2021", "Spring 2021"))) %>%
  arrange(desc(`Timeline of Obs`))


data_time_ela <- data_lafayette %>%
  rename_with( ~ str_remove_all(.x, "\\.\\.\\.[:digit:][:digit:]")) %>%
  group_by(`Timeline of Obs`) %>%
  summarise(across(!c(1:3), ~ mean(as.numeric(.x), na.rm = T))) %>%
  pivot_longer(!`Timeline of Obs`) %>%
  mutate(`Timeline of Obs` = factor(`Timeline of Obs`, levels = c("Summer 2019", "Fall 2019", "Winter 2020", "Spring 2020",
                                                                  "Winter 2021", "Spring 2021"))) %>%
  arrange(desc(`Timeline of Obs`)) %>%
  bind_cols(n = data_time_ela_n$value) %>%
  mutate(reveal = case_when(name == "CA2a. Questions and tasks address the text by attending to its particular qualitative features: its meaning/purpose and/or language, structure(s), or knowledge demands." ~ 1,
                            name == "CA2b. Questions and tasks require students to use evidence from the text to demonstrate understanding and to support their ideas about the text." ~ 2,
                            name == "CA2c. Questions and tasks attend to the words (academic vocabulary), phrases, and sentences within the text." ~ 3,
                            name == "CA2d. Questions and tasks are sequenced to build knowledge by guiding students to delve deeper into the text and graphics." ~ 4,
                            name == "CA3a. The teacher poses questions and tasks for students to do the majority of the work: speaking/listening, reading, and/or writing; Students do the majority of the work of the lesson" ~ 5,
                            name == "CA3b. The teacher cultivates reasoning and meaning making by allowing students to productively struggle; Students persevere through difficulty." ~ 6,
                            name == "CA3c. The teacher expects evidence and precision from students and probes students’ answers accordingly; Students provide text evidence to support their ideas and display precision in their oral and/or written responses." ~ 7,
                            name == "CA3d. The teacher creates the conditions for student conversations where students are encouraged to talk about each other’s thinking; Students talk and ask questions about each other’s thinking, in order to clarify or improve their understanding." ~ 8,
                            name == "CA3e. The teacher deliberately checks for understanding throughout the lesson and adapts the lesson according to student understanding; When appropriate, students refine written and/or oral responses." ~ 9,
                            name == "CA3f. When appropriate, the teacher explicitly attends to strengthening students’ language and reading foundational skills; Students demonstrate use of language conventions and decoding skills, activating such strategies as needed to read, write, and speak with grade-level fluency and skill." ~ 10)) %>%
  mutate(name = html_wrap(name, 40))

# interactive_ela_plots <- function(data, filter) {
#   plot1 <- data %>%
#     dplyr::filter(name == filter) %>%
#     ggplot(aes(x = `Timeline of Obs`, y = value, fill = `Timeline of Obs`, color = `Timeline of Obs`, 
#                tooltip = paste0("Average: ", round(value, 2)), data_id = value)) +
#     geom_col_interactive() +
#     geom_text(aes(label = paste0("n = ", n)), position = position_dodge(), vjust = -1) +
#     guides(fill = guide_legend(nrow = 1, byrow = T)) +
#     labs(x = "Season", y = "Average positive indicator(s)", title = filter) +
#     scale_fill_manual(values = c("Summer 2019" = "#040404", "Fall 2019" = "#032533", 
#                                  "Winter 2020" = "#024762", "Spring 2020" = "#016891", 
#                                  "Winter 2021" = "#008AC0", "Spring 2021" = "#00ACF0")) +
#     scale_color_manual(values = c("Summer 2019" = "#040404", "Fall 2019" = "#032533", 
#                                   "Winter 2020" = "#024762", "Spring 2020" = "#016891", 
#                                   "Winter 2021" = "#008AC0", "Spring 2021" = "#00ACF0")) +
#     theme_tl(legend = T) +
#     theme(legend.position = "bottom",
#           legend.box = "horizontal",
#           legend.direction = "horizontal",
#           strip.text = element_markdown(lineheight = 1.1, hjust = 0.5),
#           plot.title = element_markdown(size = 24, face = "bold"))
#   girafe(ggobj = plot1,
#          options = list(
#            opts_sizing(rescale = T, width = 0.7)
#          ))
# }


### ALL PLOT OBJECTS

# Convert into ggplotly
introPlot <- data_ipg %>%
  group_by(`Name of Site (Parish, District, Network)`) %>%
  count(sort = T) %>%
  ggplot(aes(x = fct_reorder(`Name of Site (Parish, District, Network)`, n), y = n, fill = `Name of Site (Parish, District, Network)`,
             tooltip = n, data_id = n)) +
  geom_col_interactive() +
  scale_fill_tl(n = 12) +
  labs(x = "Parish, District, or Network", y = "Count", title = "Observation Count by Site") +
  coord_flip() +
  theme_tl()
introPlot <- girafe(ggobj = introPlot)
