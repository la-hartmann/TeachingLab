# library(imager)
# library(imagerExtra)
# library(magick)
# library(pdftools)
# library(tidyverse)

# new_pdf <- pdftools::pdf_convert("data/student_work/test_math_file.pdf")
magick_pdf <- magick::image_read_pdf("data/student_work/test_math_file.pdf", density = 30)

ocr_data_page_1 <- magick::image_ocr_data(magick_pdf[1])
ocr_text_page_1 <- magick::image_ocr(magick_pdf[1])

# cimg_pdf <- magick_pdf[1] |>
#   image_quantize(colorspace = 'gray') |> 
#   imager::magick2cimg()
# 
# processed_text <- DenoiseDCT(cimg_pdf, 0.01) %>% ThresholdAdaptive(., 0.1, range = c(0,1))
# 
# processed_text |>
#   imagerExtra::OCR()

ocr_data_page_1 |> dplyr::summarise(mean(confidence))

# tibble::view(ocr_data_page_1)
ocr_text_page_1
