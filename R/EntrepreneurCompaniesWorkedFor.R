#' Companies that YC and TC entrepreneurs have worked for
#'
#' Companies that YC and TC entrepreneurs have worked for
#'
#' @param out.file
#' @param min.num is the minimum number of companies that have to show in the dataset 
#' @return Writes to a table
#' @export

EntrepreneurCompaniesWorkedFor <- function(out.file, min.num = 9){
    data("df_incub_company")
    sink(tempfile())
    stargazer::stargazer(df_incub_company %>% filter(n > min.num & !CompanyName %in% c("Y Combinator", "TechCrunch", "TechStars")),
          title = "Employers of Y Combinator / Techcrunch founders",
          summary = FALSE,
          label = "tab:companies", 
          column.labels = c("Company", "N"), 
          out = out.file, 
          rownames = FALSE)
    sink()
}
