library(here)
library(tidyverse)
library(MCOE)
library(googlesheets4)
library(scales)
library(RColorBrewer)
library(ggtext)
library(glue)
library(janitor)
library(readxl)

options(scipen = 999)

yr.curr = 2025

con <- mcoe_sql_con()

gsheet <- "https://docs.google.com/spreadsheets/d/1J6SYEaGQJrNfYKegi7HZ_NgjMvYXgFa2fCaMxZK-U7M/edit#gid=0"


dash_census <- tbl(con, "DASH_CENSUS") %>%
  filter(countyname == "Monterey", rtype == "D", reportingyear == 2024) %>%
  #           head(20) %>%
  collect()


# districts <- c("Greenfield",
#                "King City",
#                "Salinas Union",
#                "Santa Rita"
# )
#
# districts.list <- paste0(districts, collapse = "|")
#
#

groups <- dash_census %>%
  filter(subgrouptotal >= 30)


foster <- dash_census %>%
  filter(studentgroup == "FOS" & subgrouptotal >= 15 & subgrouptotal <= 30)


groups <- bind_rows(groups, foster) %>%
  select(districtname, studentgroup)


write_sheet(groups, gsheet)


dash2 <- tbl(con, "DASH_ALL") %>%
  collect() %>%
  mutate(
    indicator2 = recode(
      indicator,
      "ela" = "<br><img src='icons/1ela.png' width='40' /><br>4 -  ELA",
      "math" = "<br><img src='icons/2math.png' width='40' /><br>4 -  Math",
      "elpi" = "<br><img src='icons/3elpi.png' width='40' /><br>4 - ELPI",
      "grad" = "<br><img src='icons/4grad.png' width='40' /><br>5 - Grad",
      "chronic" = "<br><img src='icons/5chronic.png' width='40' /><br>5 - Chronic <br>Absenteeism",
      "susp" = "<br><img src='icons/6suspend.png' width='40' /><br>6 - Suspension"
    )
  )


da.districts23 <- readxl::read_xlsx(
  here("data", "assistancestatus24.xlsx"),
  sheet = "District and COE 2024",
  skip = 5
) %>%
  filter(Countyname == "Monterey") %>%
  select(CDS, AssistanceStatus2023)


dash.mry <- dash2 %>%
  filter(
    countyname == "Monterey" | rtype == "X",
    reportingyear == yr.curr

    #  rtype == "D"
  ) %>%
  mutate(
    indicator2 = recode(
      indicator,
      "ELA" = "4 -  <br>ELA",
      "MATH" = "4 -  <br>Math",
      "ELPI" = "4 - <br>English <br>Learner <br>Progress",
      "GRAD" = "5 - <br>Grad",
      "CHRO" = "5 - <br>Chronic <br>Absenteeism",
      "SUSP" = "6 - <br>Suspension",
      "CCI" = "8 - <br>College <br>Career <br>Readiness"
    )
  ) %>%
  #  mutate(studentgroup = if_else(indicator == "ELPI", "EL", studentgroup ) ) %>%
  #  mutate(color = if_else(indicator == "CCI", statuslevel , color )) %>%
  mutate(
    studentgroup.long.split = case_match(
      studentgroup,
      "ALL" ~ "All \nStudents",
      "AA" ~ "Black/African\nAmerican",
      "AI" ~ "American Indian \nAlaskan Native",
      "AS" ~ "Asian",
      "EL" ~ "English \nLearners",
      "LTEL" ~ "Long-Term \nEnglish \nLearners",
      "HI" ~ "Hispanic",
      "FI" ~ "Filipino",
      "PI" ~ "Pacific \nIslander",
      "FOS" ~ "Foster Youth",
      "HOM" ~ "Homeless\nYouth",
      "SED" ~ "Socioeconomically\nDisadvantaged",
      "SWD" ~ "Students with\nDisabilities",
      "MR" ~ "Two or More\nRaces",
      "WH" ~ "White"
    )
  )

write_rds(dash.mry, "dash-mry.rds")
