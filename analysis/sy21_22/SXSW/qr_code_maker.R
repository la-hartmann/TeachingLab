library(qrcode)

code <- qr_code("https://teachinglab.github.io/SXSW_report.html")

generate_svg(code, filename = here::here("images/SXSW/qr_code.svg"))
