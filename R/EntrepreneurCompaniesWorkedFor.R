#' Companies that YC and TC entrepreneurs have worked for
#'
#' Companies that YC and TC entrepreneurs have worked for
#'
#' @param out.file
#' @param min.num is the minimum number of companies that have to show in the dataset 
#' @return Writes to a table
#' @export

EntrepreneurCompaniesWorkedFor <- function(out.file, min.num = 9){
    data("experience")
    data("company")
    df.company <- 
        inner_join(experience, company, 
             by = c("CompanyID" = "CompanyId", "src" = "src")) %>%
         mutate(CompanyName = plyr::mapvalues(CompanyName, "McKinsey & Company", "McKinsey")) %>%
         mutate(CompanyName = plyr::mapvalues(CompanyName, "Microsoft Corporation", "Microsoft")) %>%
         group_by(CompanyName) %>%
         summarise(n = n()) %>%
         arrange(desc(n)) 

    sink(tempfile())
    stargazer::stargazer(df.company %>% filter(n > min.num & !CompanyName %in% c("Y Combinator", "TechCrunch", "TechStars")),
          title = "Employers of Y Combinator / Techcrunch founders",
          summary = FALSE,
          label = "tab:companies", 
          column.labels = c("Company", "N"), 
          out = out.file, 
          rownames = FALSE)
    sink()
}
