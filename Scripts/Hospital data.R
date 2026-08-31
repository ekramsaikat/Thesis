library(readr)
library(readxl)
library(dplyr)

#import data 
Renal_Admission_2021<- read_csv("D:/Thesis/Hospital_Indoor_Renal_2021.csv")

Renal_Admission_2022<- read_csv("D:/Thesis/Hospital_Indoor_Renal_2022.csv")

Renal_Admission_2023<- read_csv("D:/Thesis/Hospital_indoor_Renal_2023.csv")

Renal_Admission_2024<- read_csv("D:/Thesis/Hospital_Inddor_Renal_2024.csv")

Renal_Admission_2025<- read_excel("D:/Thesis/Hospital Indoor patient_renal.csv.xlsx")



#sob gula data combine korechilam

Renal_Admission_All <- bind_rows(
  Renal_Admission_2021,
  Renal_Admission_2022,
  Renal_Admission_2023,
  Renal_Admission_2024,
  Renal_Admission_2025
)


library(dplyr)

Renal_Admission_2021 <- Renal_Admission_2021 %>%
  mutate(
    `Reporting Date` = as.character(`Reporting Date`),
    `Last updated on` = as.character(`Last updated on`),
    `Scheduled date` = as.character(`Scheduled date`),
    `In Date of admission pt` = as.character(`In Date of admission pt`)
  )

Renal_Admission_2022 <- Renal_Admission_2022 %>%
  mutate(
    `Reporting Date` = as.character(`Reporting Date`),
    `Last updated on` = as.character(`Last updated on`),
    `Scheduled date` = as.character(`Scheduled date`),
    `In Date of admission pt` = as.character(`In Date of admission pt`)
  )

Renal_Admission_2023 <- Renal_Admission_2023 %>%
  mutate(
    `Reporting Date` = as.character(`Reporting Date`),
    `Last updated on` = as.character(`Last updated on`),
    `Scheduled date` = as.character(`Scheduled date`),
    `In Date of admission pt` = as.character(`In Date of admission pt`)
  )

Renal_Admission_2024 <- Renal_Admission_2024 %>%
  mutate(
    `Reporting Date` = as.character(`Reporting Date`),
    `Last updated on` = as.character(`Last updated on`),
    `Scheduled date` = as.character(`Scheduled date`),
    `In Date of admission pt` = as.character(`In Date of admission pt`)
  )

Renal_Admission_2025 <- Renal_Admission_2025 %>%
  mutate(
    `Reporting Date` = as.character(`Reporting Date`),
    `Last updated on` = as.character(`Last updated on`),
    `Scheduled date` = as.character(`Scheduled date`),
    `In Date of admission pt` = as.character(`In Date of admission pt`)
  )




#21 chara sob data bad dibo
Renal_Admission_2021 <- Renal_Admission_2021 %>%
  filter(
    format(`In Date of admission pt`, "%Y") == "2021"
  )

#check kortesi 21 chara ar data ache kinaa
Renal_Admission_2021 %>%
  mutate(year = format(`In Date of admission pt`, "%Y")) %>%
  count(year)



#ebar 5 year er 5 ta dataset bind korbo
library(dplyr)

Renal_Admission_All <- bind_rows(
  Renal_Admission_2021,
  Renal_Admission_2022,
  Renal_Admission_2023,
  Renal_Admission_2024,
  Renal_Admission_2025
)


#final data set er range dekhsi
range(
  Renal_Admission_All$`In Date of admission pt`,
  na.rm = TRUE
)


#Range dekhe dekhi 1938 er datao ache..ekhon koyta ache ektu check dibo
Renal_Admission_All %>%
  filter(
    `In Date of admission pt` < as.POSIXct("2021-01-01", tz = "UTC") |
      `In Date of admission pt` >= as.POSIXct("2026-01-01", tz = "UTC")
  ) %>%
  nrow()

#kon year e koyta ache seta dekhbo


Renal_Admission_All %>%
  filter(
    `In Date of admission pt` < as.POSIXct("2021-01-01", tz = "UTC") |
      `In Date of admission pt` >= as.POSIXct("2026-01-01", tz = "UTC")
  ) %>%
  mutate(year = format(`In Date of admission pt`, "%Y")) %>%
  count(year) %>%
  arrange(year)


library(dplyr)

Renal_Admission_All <- Renal_Admission_All %>%
  mutate(
    `In Date of admission pt` = if_else(
      format(`In Date of admission pt`, "%Y") == "2020",
      as.POSIXct(`Reporting Date`, tz = "UTC"),
      `In Date of admission pt`
    )
  ) %>%
  filter(
    `In Date of admission pt` >= as.POSIXct("2021-01-01", tz = "UTC"),
    `In Date of admission pt` < as.POSIXct("2026-01-01", tz = "UTC")
  ) %>%
  arrange(`In Date of admission pt`)

Renal_Admission_All %>%
  mutate(year = format(`In Date of admission pt`, "%Y")) %>%
  count(year) %>%
  arrange(year)



library(writexl)

write_xlsx(
  Renal_Admission_All,
  "Renal_Admission_Final_2021_2025.xlsx"
)


#Unique organization code and nam ber kortesi

org_mapping <- Renal_Admission_All %>%
  distinct(
    `Organisation unit code`,
    `Organisation unit name`
  ) %>%
  arrange(`Organisation unit code`)

View(org_mapping)



Renal_Admission_All %>%
  distinct(
    `Organisation unit code`,
    `Organisation unit name`
  ) %>%
  arrange(`Organisation unit code`)


org_list <- Renal_Admission_All %>%
  distinct(
    `Organisation unit code`,
    `Organisation unit name`
  ) %>%
  arrange(`Organisation unit code`)

nrow(org_list)



org_list <- Renal_Admission_All %>%
  distinct(
    `Organisation unit code`,
    `Organisation unit name`
  ) %>%
  arrange(`Organisation unit code`)

View(org_list)

library(writexl)

write_xlsx(
  org_list,
  "Organisation_List_524.xlsx"
)



org_codes <- Renal_Admission_All %>%
  distinct(`Organisation unit code`) %>%
  filter(!is.na(`Organisation unit code`))

install.packages(c("rvest", "dplyr", "purrr", "stringr"))

library(rvest)
library(dplyr)
library(purrr)
library(stringr)


url <- "https://hrm.dghs.gov.bd/public/facility-registry/reports/organization-list?is_active=1&page=1&submit=Run"

page <- read_html(url)

tables <- html_table(page, fill = TRUE)

length(tables)




head(tables[[1]])




Renal_Admission_All <- Renal_Admission_All %>%
  left_join(
    dghs_mapping %>%
      select(Code, District) %>%
      rename(
        `Organisation unit code` = Code,
        District_from_Organisation = District
      ),
    by = "Organisation unit code"
  )



org_list <- Renal_Admission_All %>%
  distinct(
    `Organisation unit code`,
    `Organisation unit name`
  ) %>%
  arrange(`Organisation unit code`)


library(writexl)

write_xlsx(
  org_list,
  "Organisation_List_524.xlsx"
)

undebugall()




library(readxl)

dghs_mapping <- read_excel("Report-2026-08-31 00_58_18.xlsx")



names(dghs_mapping)


library(dplyr)

# Code-এর format একই করা
dghs_mapping <- dghs_mapping %>%
  mutate(Code = as.character(Code))

Renal_Admission_All <- Renal_Admission_All %>%
  mutate(`Organisation unit code` =
           as.character(`Organisation unit code`))

# District mapping করা
Renal_Admission_All <- Renal_Admission_All %>%
  left_join(
    dghs_mapping %>%
      select(Code, District) %>%
      distinct(Code, .keep_all = TRUE) %>%
      rename(
        `Organisation unit code` = Code,
        District_from_Organisation = District
      ),
    by = "Organisation unit code"
  )

Renal_Admission_All %>%
  select(
    `Organisation unit code`,
    `Organisation unit name`,
    District_from_Organisation
  ) %>%
  distinct() %>%
  View()


sum(is.na(Renal_Admission_All$District_from_Organisation))

Renal_Admission_All %>%
  select(
    `Organisation unit code`,
    `Organisation unit name`,
    District_from_Organisation
  ) %>%
  distinct() %>%
  View()


Renal_Admission_All %>%
  filter(
    is.na(`Organisation unit code`) |
      `Organisation unit name` %in% c(
        "Institute of Child & Mother Health (ICMH) Matuail",
        "Haragacha 31 bed Hospital, Rangpur"
      )
  ) %>%
  select(
    `Organisation unit name`,
    `Organisation unit code`,
    District
  ) %>%
  distinct()

Q
nrow(Renal_Admission_All)



Renal_Admission_All %>%
  filter(grepl("Matuail|Haragacha", `Organisation unit name`, ignore.case = TRUE)) %>%
  select(
    `Organisation unit name`,
    `Organisation unit code`,
    District
  ) %>%
  distinct()


nrow(Renal_Admission_All)


Renal_Admission_All %>%
  filter(grepl("Matuail|Haragacha", `Organisation unit name`, ignore.case = TRUE))

id <- grepl("Matuail|Haragacha", Renal_Admission_All$`Organisation unit name`, ignore.case = TRUE)

Renal_Admission_All[id, c(
  "Organisation unit name",
  "Organisation unit code",
  "District"
)]

"District" %in% names(Renal_Admission_All)

Q

options(error = NULL)
names(Renal_Admission_All)


"District_from_Organisation" %in% names(Renal_Admission_All)

Renal_Admission_All[
  grepl(
    "Matuail|Haragacha",
    Renal_Admission_All$`Organisation unit name`,
    ignore.case = TRUE
  ),
  c(
    "Organisation unit name",
    "Organisation unit code",
    "District_from_Organisation"
  )
]


unique(
  Renal_Admission_All$`Organisation unit name`[
    grepl(
      "Matuail|Haragacha",
      Renal_Admission_All$`Organisation unit name`,
      ignore.case = TRUE
    )
  ]
)

Renal_Admission_All$District_from_Organisation[
  Renal_Admission_All$`Organisation unit name` ==
    "Institute of Child & Mother Health (ICMH) Matuail"
] <- "Dhaka"

Renal_Admission_All$District_from_Organisation[
  Renal_Admission_All$`Organisation unit name` ==
    "Haragacha 31 bed Hospital, Rangpur"
] <- "Rangpur"



Renal_Admission_All[
  Renal_Admission_All$`Organisation unit name` %in% c(
    "Institute of Child & Mother Health (ICMH) Matuail",
    "Haragacha 31 bed Hospital, Rangpur"
  ),
  c(
    "Organisation unit name",
    "Organisation unit code",
    "District_from_Organisation"
  )
]

Renal_Admission_All$District_from_Organisation[
  Renal_Admission_All$`Organisation unit name` ==
    "Institute of Child & Mother Health (ICMH) Matuail"
][1]



Renal_Admission_All$District_from_Organisation[
  Renal_Admission_All$`Organisation unit name` ==
    "Haragacha 31 bed Hospital, Rangpur"
][1]

Renal_Admission_All <- Renal_Admission_All %>%
  rename(District = District_from_Organisation)



"District" %in% names(Renal_Admission_All)


Renal_Admission_All$`In Date of admission pt`[1:20]


library(writexl)

write_xlsx(
  Renal_Admission_All,
  "Renal_Admission_All_District_Added.xlsx"
)
getwd()

library(readxl)
Renal_Admission_Final_2021_2025 <- read_excel("Renal_Admission_Final_2021_2025.xlsx")
View(Renal_Admission_Final_2021_2025)


library(dplyr)

Renal_Admission_Final_2021_2025 <- Renal_Admission_Final_2021_2025 %>%
  mutate(
    `In District` = if_else(
      is.na(`in_District`) | trimws(`in_District`) == "",
      District,
      `in_District`
    )
  )

missing <- is.na(Renal_Admission_Final_2021_2025$`In District`) |
  trimws(Renal_Admission_Final_2021_2025$`In District`) == ""



names(Renal_Admission_Final_2021_2025)[
  names(Renal_Admission_Final_2021_2025) == "In District"
] <- "In_District"



Renal_Admission_Final_2021_2025$Division <-
  Report_2026_08_31_00_58_18$Division[
    match(
      Renal_Admission_Final_2021_2025$In_District,
      Report_2026_08_31_00_58_18$District
    )
  ]
table(
  Renal_Admission_Final_2021_2025$Division,
  useNA = "ifany"
)

unique(
  Renal_Admission_Final_2021_2025$In_District[
    is.na(Renal_Admission_Final_2021_2025$Division)
  ]
)

Renal_Admission_Final_2021_2025$In_District[
  Renal_Admission_Final_2021_2025$In_District == "Chapai Nawabganj"
] <- "Chapainawabganj"

Renal_Admission_Final_2021_2025$Division <-
  Report_2026_08_31_00_58_18$Division[
    match(
      Renal_Admission_Final_2021_2025$In_District,
      Report_2026_08_31_00_58_18$District
    )
  ]
table(
  Renal_Admission_Final_2021_2025$Division,
  useNA = "ifany"
)

library(writexl)

write_xlsx(
  Renal_Admission_Final_2021_2025,
  "Renal_Admission_Final_2021_2025_District_Division.xlsx"
)
