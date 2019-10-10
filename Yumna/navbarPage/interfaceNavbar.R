navbarPage(title = "RED-CLUWE", theme = shinytheme("darkly"), id="redcluwe",
           ###Home###
           tabPanel("Home", value = "tabHome",
                    jumbotron("RED-CLUWE", "Menampilkan tabel IO dan Satelit, membandingkan skenario BAU dan AKSI", button = FALSE)),
navbarMenu("Pengaturan",
          tabPanel("Data Diri", icon = icon("database"),
                   selectInput("provinsi", "Pilih provinsi", choices = c(`Aceh`="Aceh",`Jambi`="Jambi",`Riau`="Riau")),
                   textInput("nama", "Nama Lengkap", value = ""),
                   textInput("user", "Username", value = ""),
                   textInput("pass", "Password", value = "")
                   )),
navbarMenu("Historis"),
navbarMenu("Skenario Bisnis Seperti Biasa"),
navbarMenu("Skenario Intervensi"),
tabPanel("Petunjuk", value = "tabHelp"))