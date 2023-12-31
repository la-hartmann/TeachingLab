library(tidyverse)
library(tidytext)
library(wordcloud2)
library(grid)
library(svgparser)

session_survey <- get_session_survey() |>
  select(Facilitation_Feedback, 
         `What went well in today’s session?`,
         `What could have been better about today’s session?`) |>
  pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
  drop_na(Response) |>
  filter(Response %!in% TeachingLab::na_df)
course_survey <- get_course_survey() |>
  select(`Overall, what went well in this course?`,
         `Overall, what could have been better in this course?`,
         `What is the learning from this course that you are most excited about trying out?`,
         `Which activities best supported your learning in this course?`,
         `Feel free to leave us any additional comments, concerns, or questions.`) |>
  pivot_longer(everything(), names_to = "Question", values_to = "Response") |>
  drop_na(Response) |>
  filter(Response %!in% TeachingLab::na_df)

session_course_full <- session_survey |>
  full_join(course_survey)

tidy <- session_course_full |>
  transmute(
    line = 1:nrow(session_course_full),
    text = Response
  ) |>
  unnest_tokens(word, text) |>
  anti_join(get_stopwords())

x <- tidy |>
  group_by(word) |>
  summarise(freq = n()) |>
  arrange(desc(freq))

# SVG taken from nytimes webpage source

# logo <- "<svg viewBox='-10 -2 60 65' class='css-tmjxlh'><defs></defs><g stroke='none' stroke-width='1' fill='none' fill-rule='evenodd'><g fill='#000'><path d='M43.6284633,34.8996508 C41.83393,39.6379642 38.53153,43.2989842 33.7932167,45.2371375 L33.7932167,34.8996508 L39.46463,29.8027175 L33.7932167,24.7777375 L33.7932167,17.6709842 C38.9621033,17.3120775 42.5514567,13.5074375 42.5514567,8.84136417 C42.5514567,2.73966417 36.7369967,0.5859375 33.4345967,0.5859375 C32.71707,0.5859375 31.9270167,0.5859375 30.7789167,0.872890833 L30.7789167,1.16013083 C31.20949,1.16013083 31.8550633,1.08846417 32.0709233,1.08846417 C34.36827,1.08846417 36.0911367,2.16518417 36.0911367,4.2469575 C36.0911367,5.82620417 34.7988433,7.40545083 32.5017833,7.40545083 C26.83037,7.40545083 20.15419,2.81133083 12.9038167,2.81133083 C6.44292333,2.81133083 1.99242333,7.6207375 1.99242333,12.5023842 C1.99242333,17.3120775 4.79201,18.8913242 7.73521667,19.9680442 L7.80717,19.6808042 C6.87378333,19.1066108 6.22763667,18.1018442 6.22763667,16.5223108 C6.22763667,14.3688708 8.23774333,12.5743375 10.7503767,12.5743375 C16.8520767,12.5743375 26.68675,17.6709842 32.7887367,17.6709842 L33.36293,17.6709842 L33.36293,24.8496908 L27.6918033,29.8027175 L33.36293,34.8996508 L33.36293,45.3804708 C30.9942033,46.2416175 28.5532367,46.6010975 26.0406033,46.6010975 C16.5648367,46.6010975 10.53509,40.8577308 10.53509,31.3102975 C10.53509,29.0135242 10.8220433,26.7878442 11.46819,24.6341175 L16.20593,22.5526308 L16.20593,43.6576042 L25.8253167,39.4226775 L25.8253167,17.8146042 L11.6834767,24.1315908 C13.1191033,19.9680442 16.06231,16.9531708 19.5799967,15.2303042 L19.50833,15.0150175 C10.0322767,17.0967908 0.84375,24.2754975 0.84375,35.0432708 C0.84375,47.4622442 11.32457,56.0768642 23.5285433,56.0768642 C36.4497567,56.0768642 43.7720833,47.4622442 43.84375,34.8996508 L43.6284633,34.8996508 Z'></path></g></g></svg>"
# 
# png(
#   filename = here::here("images/downloads/logo.png"),
#   width = 7, height = 7, units = "in", res = 500, bg = "white"
# )
# 
# grid.draw(read_svg(logo))

# This creates the wordcloud. I sometimes have to refresh the page to get it to
# render, I assume because it varies where it places the words. It's based on the
# wordcloud2 javascript library, FYI.

wordcloud2::letterCloud(data = x,
                        word = "Teaching Lab",
                        letterFont = "Calibri",
                        fontFamily = "Calibri",
                        color = TeachingLab::tl_palette(color = "blue", n = 20),
                        # widgetsize = c(1080, 1080),
                        size = 0.5)
                        
wordcloud2(x,
           figPath = here::here("images/Logos/just_logo.png"),
           size = .5,
           widgetsize = c(1000, 1000),
           color = "#04abeb",
           backgroundColor = "white"
)

# Finally, I couldn't figure out how to save it from R. I opened the viewer in chrome,
# and then saved the image from there.
