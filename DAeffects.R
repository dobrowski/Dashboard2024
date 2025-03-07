

schools %>%
    filter(str_detect(District, "Penin")) %>%
    select(CDSCode, District, School ) %>%
    distinct() %>%
    clipr::write_clip()

sss <- "https://docs.google.com/spreadsheets/d/11p4b2ymZWkCvTbJnomrAM935fBarpuhKMLXLCyBgf1U/edit?gid=583322800#gid=583322800"

da.schools <- read_sheet(sss,
           sheet = "DA Support Schools"
) %>%
    mutate(cds = as.character(cds))

da.schools.list <- unique(da.schools$cds)


dash.da.schools <- tbl(con,"DASH_ALL") %>%
    filter(cds %in% da.schools.list  ) %>%
    collect () 


joint <- dash.da.schools %>%
    select(cds, indicator, studentgroup, reportingyear, change, currstatus, priorstatus) %>%
    right_join(da.schools) %>%
    pivot_wider(names_from = reportingyear, 
                values_from = c(priorstatus, change, currstatus),
                names_vary =  "slowest")


write_sheet(joint, 
            "https://docs.google.com/spreadsheets/d/11p4b2ymZWkCvTbJnomrAM935fBarpuhKMLXLCyBgf1U/edit?gid=583322800#gid=583322800",
           sheet = "DA Support Schools Results"
)


joint %>%
    group_by(indicator) %>%
    summarise(yr2023 = mean(c(change_2023), na.rm = TRUE),
              yr2024 = mean(c(change_2024), na.rm = TRUE),
              ) %>%
    write_sheet(sss,
                sheet = "DA Support Schools Summary"
    )




dash2 %>%
    filter(countyname == "Monterey",
           studentgroup == "EL",
           indicator == "ELA",
           reportingyear == "2024",
           rtype == "D") %>%
    select(cds, districtname ,indicator, studentgroup, reportingyear, change, currstatus, priorstatus) %>%
#    right_join(da.schools) %>%
    pivot_wider(names_from = reportingyear, 
                values_from = c(priorstatus, change, currstatus),
                names_vary =  "slowest") %>%
    left_join(da.districts23, by = c("cds" = "CDS")) %>%
    write_sheet(sss,
                sheet = "EL Data for Districts"
    )
