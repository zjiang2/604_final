#helper function(s)

clean_data <- function(df, drop=TRUE) {
  # clean column names
  df <- df %>% rename(
    vax_rate_18_plus = Series_Complete_18PlusPop_Pct,
    vax_rate_65_plus = Series_Complete_65PlusPop_Pct,
    vax_rate_overall = Series_Complete_Pop_Pct
  )
  if (drop) {
    drop_cols <- c("Series_Complete_18Plus", "Series_Complete_65Plus", "Series_Complete_Yes")
    df <- df %>% select(-one_of(drop_cols))
  }
  # drop counties that are missing data on vaccination rates
  df <- df %>% filter(!is.na(vax_rate_18_plus))
  # drop counties that are missing 2020 election data
  df <- df %>% filter(!is.na(percentage20_Donald_Trump))
  # group counties into low- and high-vaccination categories
  return(df)
}

split_data <- function(df, random_seed=604) {
  set.seed(random_seed)
  spec = c(train = .75, test = .15, validate = .10)
  # randomly shuffle rows
  df <- df[sample(nrow(df)),]
  g = sample(cut(
    seq(nrow(df)), 
    nrow(df)*cumsum(c(0,spec)),
    labels = names(spec)
  ))
  cci = split(df, g)
  return(cci)
}

# Get prediction accuracy
pred_acc <- function(pred, actual){
  return(mean(pred==actual))
}

