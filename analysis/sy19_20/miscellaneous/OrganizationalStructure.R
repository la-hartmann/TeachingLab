library(highcharter)
library(tidyverse)
library(googledrive) # To export to google drive
library(htmlwidgets) # To save html
library(here)
library(webshot) # For png

(org_structure <- highchart() %>%
  hc_chart(type = 'organization', inverted = T) %>% # Type of chart
  hc_title(text = '', color = "black",
           style = list(fontSize = "35px", fontFamily = "Open Sans Bold", fontWeight = "bold")) %>% # Title
  hc_add_series( # Data
    name = 'Teaching Lab',
    data = list(
      list(from = 'CEO', to = 'CFO'),
      list(from = 'CEO', to = 'Ops'),
      list(from = 'CFO', to = 'D, Accounting & Finance'),
      list(from = 'CEO', to = 'Partnerships'),
      list(from = 'CEO', to = 'External Affairs'),
      list(from = 'CEO', to = 'Content Design'),
      list(from = 'CEO', to = 'Executive Assistant'),
      list(from = 'CEO', to = 'State Level Partnerships'),
      list(from = 'Ops', to = 'Comms'),
      list(from = 'Ops', to = 'People Ops'),
      list(from = 'Ops', to = 'Learning & Research'),
      list(from = 'Learning & Research', to = 'Data Analyst'),
      list(from = 'Partnerships', to = 'P1'),
      list(from = 'Partnerships', to = 'P2'),
      list(from = 'Partnerships', to = 'P3'),
      list(from = 'Partnerships', to = 'P4'),
      list(from = 'P1', to = 'P11'),
      list(from = 'P1', to = 'P12'),
      list(from = 'P1', to = 'P13'),
      list(from = 'P1', to = 'P14'),
      list(from = 'P1', to = 'P15'),
      list(from = 'P1', to = 'P16'),
      list(from = 'P2', to = 'P21'),
      list(from = 'P2', to = 'P22'),
      list(from = 'P2', to = 'P23'),
      list(from = 'P4', to = 'P41'),
      list(from = 'P4', to = 'P42'),
      list(from = 'Content Design', to = 'CD1'),
      list(from = 'Content Design', to = 'CD2'),
      list(from = 'Content Design', to = 'CD3'),
      list(from = 'Content Design', to = 'Project Manager'),
      list(from = 'CD1', to = 'CD11'),
      list(from = 'CD1', to = 'CD12'),
      list(from = 'CD2', to = 'CD21'),
      list(from = 'CD2', to = 'CD22'),
      list(from = 'CD2', to = 'CD23'),
      list(from = 'CD2', to = 'CD24'),
      list(from = 'CD3', to = 'CD31'),
      list(from = 'State Level Partnerships', to = 'SLP1'),
      list(from = 'State Level Partnerships', to = 'SLP2'),
      list(from = 'State Level Partnerships', to = 'SLP3'),
      list(from = 'Data Analyst', to = 'Consultant1')
    ),
    levels = list(
      list(level = 0, color = '#980104', dataLabels = list(color = 'black', position = 'center', style = list(fontSize = "35px"),
                                                           x = 30, y = -8), 
           linkColor = "black"),
      list(level = 1, color = '#980104', dataLabels = list(color = 'black', position = 'right', style = list(fontSize = "35px")), 
           linkColor = "#045777"),
      list(level = 2, color = '#980104', dataLabels = list(color = 'black', position = 'center', style = list(fontSize = "35px")), 
           linkColor = "#04ABEB"),
      list(level = 3, color = '#980104', dataLabels = list(color = 'black', position = 'center', style = list(fontSize = "35px")),
           linkColor = "white"),
      list(level = 4, color = '#980104', dataLabels = list(color = 'black', position = 'center', style = list(fontSize = "35px")))
    ),
    nodes = list(
      list(id = 'CEO', width = 475, height = 200, title = 'CEO', name = 'Sarah Johnson', color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1608053346065-MQDYNTPYBLFCHLRKN386/ke17ZwdGBToddI8pDm48kFqSi103JKAvrWfYgkYOMyx7gQa3H78H3Y0txjaiv_0fDoOvxcdMmMKkDsyUqMSsMWxHk725yiiHCCLfrh8O1z5QPOohDIaIeljMHgDF5CVlOqpeNLcJ80NK65_fV7S1Ua7cK7F1YwSEr-a3613BxBkXZQcu1gjy2u0ykcGcxpfrZ1zupkJLE0_to5EqUigerg/SJohnson+%281%29+%282%29.jpg?format=300w'),
      list(offset = -115, id = 'Ops', width = 300, title = 'MD, Org Learning & Ops', name = "HaMy Vu", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594148806947-UJYS5LBRSH2NBFEM7VNV/ke17ZwdGBToddI8pDm48kOgdWN1_x5HeFyeLV0VRAM5Zw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7TQvSJ-dUvq06FiA6bZ_jnXgfCrOOtRMK44WkF9_fWEUfrF73pzXYR7N__pf4YYLug/hamy-vu-sm.png?format=300w'),
      list(offset = -990, width = 300, id = 'CFO', title = 'Interim CFO', name = "Daissan Colbert", color = "#ff89ff", image = 'https://ca.slack-edge.com/TGVBHJUDA-U01AK403V8V-0efd2f92987a-512'),
      list(width = 300, id = 'D, Accounting & Finance', title = 'D, Accounting & Finance', name = "TBH", color = "#ff89ff", image = 'https://wellesleysocietyofartists.org/wp-content/uploads/2015/11/image-not-found.jpg'),
      list(offset = -25, width = 300, id = 'Partnerships', title = 'MD, Partnerships', name = "Sheena Lights", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594148853696-BPDVSQ8NBLJ1SCDBTL6H/ke17ZwdGBToddI8pDm48kOR-UgY8NrrZWpXhMYPNEYNZw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7cbpD-ALo5EVlYbhs4zusnsFuYA_uaDwal_cSO7KxpEls1PVpqM7CqH277rg752K3w/sheena-lights-sm.png?format=300w'),
      list(width = 300, id = 'External Affairs', title = 'D, External Affairs', name = "Teecee Hutcherson", color = "#007ad0", image = 'https://wellesleysocietyofartists.org/wp-content/uploads/2015/11/image-not-found.jpg'),
      list(offset = 50, width = 300, id = 'Content Design', title = 'MD, Content Design', name = "Vaishali Joshi", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594149046231-ZBQMKL094IGWO9YU4V2K/ke17ZwdGBToddI8pDm48kOgdWN1_x5HeFyeLV0VRAM5Zw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7TQvSJ-dUvq06FiA6bZ_jnXgfCrOOtRMK44WkF9_fWEUfrF73pzXYR7N__pf4YYLug/vaishali-joshi-sm.png?format=300w'),
      list(offset = 25, width = 300, id = "Project Manager", title = "Project Manager", name = "Kelly Sanders", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1614094274227-VPTEQJ4SDUNS86ZHCVVR/ke17ZwdGBToddI8pDm48kOysYHTF9kd9GH2IKLno8INZw-zPPgdn4jUwVcJE1ZvWQUxwkmyExglNqGp0IvTJZUJFbgE-7XRK3dMEBRBhUpwWsJnkJRO8PjwSjJTBCBBzyNEP-dE3Ug7lZ5xlK8_785dEqZ_gPcBLjJ05a0r4PmM/IMG_3383.jpg?format=300w'),
      list(offset = 150, width = 300, id = 'Executive Assistant', title = 'Executive Assistant', name = "Taylor Wicker", color = "#007ad0", image = 'https://wellesleysocietyofartists.org/wp-content/uploads/2015/11/image-not-found.jpg'),
      list(width = 300, layout = "hanging", offset = 1150, id = 'State Level Partnerships', title = 'D, State Level Partnerships', name = "Octavia Nixon", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594148996170-3PSCEJ0AVLSYGUY6YHOC/ke17ZwdGBToddI8pDm48kOgdWN1_x5HeFyeLV0VRAM5Zw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7TQvSJ-dUvq06FiA6bZ_jnXgfCrOOtRMK44WkF9_fWEUfrF73pzXYR7N__pf4YYLug/octavia-nixon-sm.png?format=300w'),
      list(linkColor = "white", id = 'Comms', title = 'D, Strat Initiatives & Comms', name = "Nichole Herring", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594149093304-R24ILGRA39AVSJHH0G6Q/ke17ZwdGBToddI8pDm48kOgdWN1_x5HeFyeLV0VRAM5Zw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7TQvSJ-dUvq06FiA6bZ_jnXgfCrOOtRMK44WkF9_fWEUfrF73pzXYR7N__pf4YYLug/nichole-herring-sm.png?format=300w'),
      list(id = 'People Ops', title = 'M, People Ops', name = "Melissa Ramos", color = "#007ad0", image = 'https://ca.slack-edge.com/TGVBHJUDA-UH4SNDWBV-5a39db034450-512'),
      list(layout = "hanging", id = 'Learning & Research', title = 'D, Learning & Research', name = "Shaye Worthman", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1614198637301-QFOR66XO83AIYCVQBCA0/ke17ZwdGBToddI8pDm48kNKSPxW9-3SXDfcM11sbE0dZw-zPPgdn4jUwVcJE1ZvWQUxwkmyExglNqGp0IvTJZUJFbgE-7XRK3dMEBRBhUpzmzhkK92ndxkc1lJomehTFKfFPRuUbfzFdauWdLiS7JkjOz7PWkNKdDOfrDUrYQlE/Headshot%2Btec.jpg?format=300w'),
      list(layout = "hanging", id = 'Data Analyst', title = 'Data Analyst', name = "Duncan Gates", color = "#007ad0", image = 'https://ca.slack-edge.com/TGVBHJUDA-U01MRR8KBRU-b28935aa19b1-512'),
      list(layout = "hanging", id = "P1", title = "D, Partnerships - GB", name = "Rene Arnold", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594149195275-38R94INRW7SQNTAM8FQ1/ke17ZwdGBToddI8pDm48kOgdWN1_x5HeFyeLV0VRAM5Zw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7TQvSJ-dUvq06FiA6bZ_jnXgfCrOOtRMK44WkF9_fWEUfrF73pzXYR7N__pf4YYLug/rene-arnold-sm.png?format=300w'),
      list(layout = "hanging", id = "P2", title = "D, Partnerships - Math", name = "Tamala Wiley", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594148782201-BKPBVDZI1FKD5KPCKMO4/ke17ZwdGBToddI8pDm48kB-cXOIlg-lhgCeNU5zNDB1Zw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7XxG-9FZQiNMT_ZdcQnlMXZsYofu31Gvi5XDm1TTaPEmrBZ0Jq7WjeZT9X6Cx1h7CQ/tamala-wiley-sm.png?format=300w'),
      list(id = "P3", title = "D, Special Projects", name = "Spencer Russell", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594149071580-QIRBAOQX2HN0FB7QK54X/ke17ZwdGBToddI8pDm48kCIUILwhZ1SX3A20zlw1HadZw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7T-j82ScS_xjTqFYGqFrT72_ZggFlf5FK0kMaUV6N4eobmB8855HSly_AaNnDu9iOA/spencer-russell-sm.png?format=300w'),
      list(layout = "hanging", id = "P4", title = "D, Partnerships - EL", name = "Kristen Biggs", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1607101714323-VYW92VS5QSJSR8766C4L/ke17ZwdGBToddI8pDm48kMcHqMOAYqkn-ttN7K3yGAZZw-zPPgdn4jUwVcJE1ZvWQUxwkmyExglNqGp0IvTJZUJFbgE-7XRK3dMEBRBhUpwu_Uy61MNniKR19_Evq19I10fCn6vfd5VLj2vU_o5XLx3Fr3NPpJ9EMsaB9pHq1Bc/KB+Headshot.png?format=300w'),
      list(layout = "hanging", id = "P11", title = "M, Partnerships", name = "Jalinda Soto", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594148969799-X9RYPC9JYM5UU78F9K9Q/ke17ZwdGBToddI8pDm48kOgdWN1_x5HeFyeLV0VRAM5Zw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7TQvSJ-dUvq06FiA6bZ_jnXgfCrOOtRMK44WkF9_fWEUfrF73pzXYR7N__pf4YYLug/jalinda-soto-sm.png?format=300w'),
      list(layout = "hanging", id = "P12", title = "M, Partnerships", name = "Lauren Myer", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594148943948-K789AAARNATYN41HKYHT/ke17ZwdGBToddI8pDm48kIkFsUIO7doDXoUE1S4YOYxZw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7aXK0t8ahyzoOLFEHArbPTKrcgAJh9dJ7OcjGInxXkIfZ4OZTo4K8VVblgRXzlxHPw/lauren-myer-sm.png?format=300w'),
      list(layout = "hanging", id = "P13", title = "M, Partnerships", name = "Holli Fears", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594149023096-TJUIYGPW8TFHUFRDYD1N/ke17ZwdGBToddI8pDm48kOgdWN1_x5HeFyeLV0VRAM5Zw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7TQvSJ-dUvq06FiA6bZ_jnXgfCrOOtRMK44WkF9_fWEUfrF73pzXYR7N__pf4YYLug/holli-fears-sm.png?format=300w'),
      list(layout = "hanging", id = "P14", title = "M, Partnerships", name = "Jasmin Porter", color = "#007ad0", image = 'https://ca.slack-edge.com/TGVBHJUDA-U01DE1ZPJ5A-eca901913413-512'),
      list(layout = "hanging", id = "P15", title = "M, Partnerships", name = "Sarah Tierney", color = "#007ad0", image = 'https://wellesleysocietyofartists.org/wp-content/uploads/2015/11/image-not-found.jpg'),
      list(layout = "hanging", id = "P16", title = "M, Partnerships", name = "Tara McDonald", color = "#007ad0", image = 'https://ca.slack-edge.com/TGVBHJUDA-ULJL1NMGU-e44148166e74-512'),
      list(layout = "hanging", id = "P21", title = "M, Partnerships", name = "Monica Moton", color = "#007ad0", image = 'https://ca.slack-edge.com/TGVBHJUDA-U01AJA3MWLE-787cebabb8a2-512'),
      list(layout = "hanging", id = "P22", title = "M, Partnerships", name = "Nadalee Williams", color = "#007ad0", image = 'https://ca.slack-edge.com/TGVBHJUDA-U01B5F5A1ME-7f1a11e10789-512'),
      list(layout = "hanging", id = "P23", title = "M, Partnerships", name = "Julie Poluszejko", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1610725535029-I31208O4PTD19MTR1QFH/ke17ZwdGBToddI8pDm48kMucMYyr-GymD6JuRa1wxmJZw-zPPgdn4jUwVcJE1ZvWQUxwkmyExglNqGp0IvTJZUJFbgE-7XRK3dMEBRBhUpzTZk3TZ725eFL9UIi83QE8s3jVzQ7hWaRhMy6K1rIfLps97WtjG4Zmf_Xs-OsVdOw/JP.headshot.jpg?format=300w'),
      list(layout = "hanging", id = "P41", title = "M, Partnerships", name = "Mandi Van Dellen", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1607101872792-DH1ICLZH7ESB1IJTNCU1/ke17ZwdGBToddI8pDm48kFZlxnammp80UVoXfdg-jSt7gQa3H78H3Y0txjaiv_0fDoOvxcdMmMKkDsyUqMSsMWxHk725yiiHCCLfrh8O1z5QPOohDIaIeljMHgDF5CVlOqpeNLcJ80NK65_fV7S1UU3_sPjFQrLVGTA0yumzk7g2r1If-DQu1VBGeolxR0mxaurZZORn6CzeqSn0MavC7w/IMG_9894.jpg?format=300w'),
      list(layout = "hanging", id = "P42", title = "M, Partnerships", name = "Quintin Bostic", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1611761272050-IV8QLR9LL4D7X8R5M3KE/ke17ZwdGBToddI8pDm48kMKmrKmA4BFd2y8C5yd-0E17gQa3H78H3Y0txjaiv_0fDoOvxcdMmMKkDsyUqMSsMWxHk725yiiHCCLfrh8O1z5QHyNOqBUUEtDDsRWrJLTmTqyr0YSX1lQOSnWxEpwDInSIjMzXt0lBHd0gx024TABTdhzxUbt444nLj9tz52Xm/Headshot.jpg?format=300w'),
      list(layout = "hanging", id = "CD1", title = "D, ELA Content Design", name = "Adrienne Williams", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594148671756-JI1OVGZ9BW8TYSNGHA9C/ke17ZwdGBToddI8pDm48kOgdWN1_x5HeFyeLV0VRAM5Zw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7TQvSJ-dUvq06FiA6bZ_jnXgfCrOOtRMK44WkF9_fWEUfrF73pzXYR7N__pf4YYLug/adrienne-williams-sm.png?format=300w'),
      list(layout = "hanging", id = "CD2", title = "D, Math Content Design", name = "Ryan Colon", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594149175791-V99K6OC478ACR1OKSEN8/ke17ZwdGBToddI8pDm48kOgdWN1_x5HeFyeLV0VRAM5Zw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7TQvSJ-dUvq06FiA6bZ_jnXgfCrOOtRMK44WkF9_fWEUfrF73pzXYR7N__pf4YYLug/ryan-colon-sm.png?format=300w'),
      list(layout = "hanging", id = "CD3", title = "D, State Content Design", name = "Mandy Flora", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594149132767-MQCMTWZDHSB1PDTECQCF/ke17ZwdGBToddI8pDm48kOgdWN1_x5HeFyeLV0VRAM5Zw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7TQvSJ-dUvq06FiA6bZ_jnXgfCrOOtRMK44WkF9_fWEUfrF73pzXYR7N__pf4YYLug/mandy-flora-sm.png?format=300w'),
      list(layout = "hanging", id = "CD11", title = "M, Content Design", name = "Addie Kelley", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1594148880184-G2UO6F1GFNESA9312G14/ke17ZwdGBToddI8pDm48kOgdWN1_x5HeFyeLV0VRAM5Zw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7TQvSJ-dUvq06FiA6bZ_jnXgfCrOOtRMK44WkF9_fWEUfrF73pzXYR7N__pf4YYLug/addie-kelley-sm.png?format=300w'),
      list(layout = "hanging", id = "CD12", title = "M, Content Design", name = "Adrianne Sublett", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1607101630567-JD6E6B51RBB3PI4IZEL6/ke17ZwdGBToddI8pDm48kD6XSTkkCLK0ud7x2vjxp8lZw-zPPgdn4jUwVcJE1ZvWEtT5uBSRWt4vQZAgTJucoTqqXjS3CfNDSuuf31e0tVFmQ2fjIZRwwuiy4LE95TjmvQOa6hhFCGkaN5YnQxcol-87Nsj43NRAr6WuWZv5DKs/Adrianne+Sublett%27s+Headshot+August+2020.png?format=300w'),
      list(layout = "hanging", id = "CD21", title = "M, Content Design", name = "Cole Farnum", color = "#007ad0", image = 'https://wellesleysocietyofartists.org/wp-content/uploads/2015/11/image-not-found.jpg'),
      list(layout = "hanging", id = "CD22", title = "M, Content Design", name = "Erik Reitenger", color = "#007ad0", image = 'https://images.squarespace-cdn.com/content/v1/5ef2087220c7256a8632676b/1610725229572-5EA0TKE75S5X5E7PAE95/ke17ZwdGBToddI8pDm48kO2-licg-vKfFfo1BeyjyANZw-zPPgdn4jUwVcJE1ZvWhcwhEtWJXoshNdA9f1qD7aP-VOPYnKXVfGNi7anTLYGN8Mif59rLxgnX5ZwixZ3uJgZqXBh1H-PKEWXE0LmUqg/Reitinger+Headshot.png?format=300w'),
      list(layout = "hanging", id = "CD23", title = "M, Content Design", name = "Emily Howard", color = "#007ad0", image = 'https://ca.slack-edge.com/TGVBHJUDA-U01P47PRBT6-1c97d54bedd9-512'),
      list(layout = "hanging", id = "CD24", title = "M, Content Design", name = "TBH", color = "#49b3a7", image = 'https://wellesleysocietyofartists.org/wp-content/uploads/2015/11/image-not-found.jpg'),
      list(layout = "hanging", id = "CD31", title = "M, Content Design", name = "TBH", color = "#49b3a7", image = 'https://wellesleysocietyofartists.org/wp-content/uploads/2015/11/image-not-found.jpg'),
      list(layout = "hanging", id = "SLP1", title = "M, Partnerships", name = "Lindsay Romano", color = "#007ad0", image = 'https://ca.slack-edge.com/TGVBHJUDA-ULADAR1AL-61dbb1919921-512'),
      list(layout = "hanging", id = "SLP2", title = "M, Partnerships", name = "Cara Gromm", color = "#007ad0", image = 'https://ca.slack-edge.com/TGVBHJUDA-U01B5F5DGHW-15019727df5e-512'),
      list(layout = "hanging", id = "SLP3", title = "M, Partnerships", name = "Jenn Becker", color = "#007ad0", image = 'https://wellesleysocietyofartists.org/wp-content/uploads/2015/11/image-not-found.jpg'),
      list(width = 300, offset = -500, id = "Consultant1", title = "HR Consultant", name = "Naa Yirenkyi", color = "#ff89ff", image = 'https://wellesleysocietyofartists.org/wp-content/uploads/2015/11/image-not-found.jpg')
    ),
    colorByPoint = F,
    dataLabels = list(color = 'black', style = list(fontFamily = "Open Sans Bold")),
    borderColor = 'white',
    nodeWidth = 300, # Since it is inverted remember this is actually height
    linkLineWidth = 9,
    nodePadding = 60
  ) %>%
  hc_tooltip(outside = T))

# Save widget
saveWidget(org_structure, here("HTML/structure.html"))

# Webshots with image resizing
# webshot(url = here("Images/structure.html"),
#         file = here("Images/org_structure.png"),
#         vwidth = 4100,
#         vheight = 2400, delay = 5, zoom = 5)
webshot2::webshot(url = here("HTML/structure.html"),
        file = here("Images/org_structure_lowres.png"),
        vwidth = 5000,
        vheight = 3800, delay = 5, zoom = 1)

# webshot2::webshot(url = here("Images/structure.html"),
#                   file = here("Images/test.png"),
#                   vwidth = 4100,
#                   vheight = 2400, delay = 5, zoom = 1.5) #%>%
  # resize("50%") %>%
  # shrink()

# Upload to google drive
# googledrive::drive_upload(here("Images/org_structure.png"), overwrite = T)
googledrive::drive_upload(here("Images/org_structure_lowres.png"),
                          "org_structure_small.png",
                          overwrite = T)
