set.seed(12345L)

# create_split:
# Construct an group index vector for cross validation splitting into K groups
create_split <- function(data, K) {
  indices <- rep(seq_len(K), times = nrow(data) / K, size = nrow(data))
  sample(indices, size = nrow(mydata), replace = FALSE)
}

# Read and the lab 6 data:
TMINallobs <- read.csv(file = "data/TMINallobs.csv",
                       header = TRUE,
                       stringsAsFactors = FALSE)
ggplot(TMINallobs %>%
         filter(DecYear >= 2005, DecYear < 2007)) +
  geom_point(aes(DecYear, Value)) +
  facet_wrap(vars(Name), nrow = 3)

# To simplify the example we shorten the data to a nice round number:
mydata <- TMINallobs[1:10000, ]

# Try to construct a data split
thesplit <- create_split(mydata, 5)
unique(thesplit)
# [1] 3 10  2  5  4  6  1  8  9  7

# If this was someone else's function, we might use debugonce()
# to step into it to find out what's wrong (we expected values between 1 and 5):
debugonce(create_split)
create_split(mydata, 5)
# Press Enter to run each line in turn. See ?browser for more commands.
