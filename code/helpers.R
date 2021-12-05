#helper function(s)

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