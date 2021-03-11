# Functions Created by Jacob Johnson

NA_count <- function(x, ...) {
  sum(is.na(x), ...)
}


summary_more <- function(df) {
  summary_return <- df %>% 
    map_df(., typeof) %>% 
    t() %>% 
    data.frame(Type = .) %>% 
    cbind(., df %>% map_df(., class) %>% t() %>% data.frame(Class = .)) %>% 
    cbind(., df %>% map_df(., n_distinct) %>% t() %>% data.frame(n_Distinct = .))  %>% 
    cbind(., df %>% map_df(., .f = function(x) { sum(is.na(x), na.rm = T) } ) %>% t() %>% data.frame(NA_count = .)) %>% 
    mutate(FieldName = names(df)) %>% 
    select(FieldName, everything()) %>%
    cbind(., describe(df))
  print(summary_return, right = F)
}
