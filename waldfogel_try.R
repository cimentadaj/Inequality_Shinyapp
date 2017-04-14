all_surveys <- read_csv("./shiny/all_data.csv")

pisa_subsetter <- function(data, country_choose) {
  data %>%
    filter(country == country_choose &
           survey == "PISA")
}

waldfogel <-
  pisa_subsetter(all_data, "Canada") %>%
  `[[`("score")

pisa_subsetter(all_surveys, "Canada") %>%
  mutate(ses2 = as.factor(ses2),
         waldfogel_coding = waldfogel) %>%
  ggplot(aes(score, rank, colour = as.factor(ses2))) +
  geom_point(alpha = 0.7, size = 2, shape = 20) + # dots are for my coding
  geom_point(aes(waldfogel_coding, rank,
                 colour = as.factor(ses2)), # triangle is for waldfogels coding
             alpha = 0.7, size = 2, shape = 2) +
  ylab("Student ranking") +
  xlab("Math test score") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_color_discrete(name = "Parent's education",
                       labels = c("Low educated",
                                  "Middle educated",
                                  "High educated")) +
  scale_alpha(guide = F) +
  facet_wrap(~ year)

ggsave(filename = "australia.png")


####

parent_edu <- "MISCED|FISCED"
edu_recode <- "1:3 = 1; 4:6 = 2; 7 = 3; else = NA"
# edu_recode <- "1:3 = 1; 4:5 = 2; 6:7 = 3; else = NA"
cnt <- "CNT"

pisa_table <- function(df, parent_edu, edu_recode, cnt, pisa2015 = F, pisa2000 = F) {
  if (pisa2000) cnt <- "COUNTRY"
  
  if (pisa2015) {
    
    df <- base::subset(df, !is.na(HISCED))
    df[, "country"] <- as.character(df[, cnt])
    df$ses2 <- car::recode(as.numeric(df[, "HISCED"]), edu_recode)
    
    pv_data <- pisa2015.table("ses2", by = "country", data = df)
    
    return(pv_data)
  }
  
  names(df)[grep(parent_edu, names(df))] <- c("dad_isced", "mom_isced")
  df[, "country"] <- as.character(df[, cnt])
  
  df$ses <- pmax(as.numeric(df$mom_isced), as.numeric(df$dad_isced), na.rm = T)
  df$ses2 <- car::recode(df$ses, edu_recode)
  
  # PQFISCED - father edu
  # 0 None
  # 1 ISCED 3A 
  # 2 ISCED 4 
  # 3 ISCED 5B 
  # 4 ISCED 5A, 6 
  # 9 Missing
  
  # PQMISCED - mother edu
  # 0 None
  # 1 ISCED 3A 
  # 2 ISCED 4 
  # 3 ISCED 5B 
  # 4 ISCED 5A, 6 
  # 9 Missing
  
  data <- subset(df, !is.na("country") & !is.na("ses2"))
  
  pv_data <- pv_data <- pisa.table("ses2", by = "country", data = df)
  
  pv_data
}


pisa_fun <- lapply(c(paste0("student", seq(2003, 2012, by = 3)), "ppisa2015"),
                   function(x) {
                     
                     pisa2015 <- ifelse(x == "ppisa2015", T, F)
                     pisa2000 <- ifelse(x == "math2000", T, F)
                     
                     df <- get(x)
                     message("Data loaded correctly")
                     data <- pisa_table(df, parent_edu, edu_recode, cnt, pisa2015, pisa2000)
                     data$year <- gsub("[a-z]", "", x)
                     data$country <- as.character(data$country)
                     
                     data
                   })

pisa_waldfogel <- lapply(c(paste0("student", seq(2003, 2012, by = 3)), "ppisa2015"),
                         function(x) {
                           
                           pisa2015 <- ifelse(x == "ppisa2015", T, F)
                           pisa2000 <- ifelse(x == "math2000", T, F)
                           
                           df <- get(x)
                           message("Data loaded correctly")
                           data <- pisa_table(df, parent_edu, edu_recode, cnt, pisa2015, pisa2000)
                           data$year <- gsub("[a-z]", "", x)
                           data$country <- as.character(data$country)
                           
                           data
                         })

pisa_datasets <-
  list(pisa_fun, pisa_waldfogel) %>%
  map(~ map2(.x, seq(2003, 2015, 3),
             ~ .x %>% mutate(country = cimentadaj::pisa_countrynames[country],
                             year = .y) %>%
               filter(country %in% c("United States", "Canada", "United Kingdom", "Australia")) %>%
               select(-Freq, - Std.err.))) %>%
  map(~ reduce(.x, rbind)) %>%
  map(~ .x %>% spread(year, Percentage))
