# Mutate: Create, modify, and delete columns


df <- read.csv("./data/ear_summarized.csv")

# Grouped tibbles

# Normalized through global mean
df %>%
  select(date, var, weight) %>%
  mutate(weight_norm = weight / mean(weight, na.rm=TRUE))

# normalized through groupe mean
df %>%
  select(date, var, weight) %>%
  group_by(var) %>%
  mutate(weight_norm = weight / mean(weight, na.rm=TRUE))

# Newly created variables are available immediately
starwars %>%
  select(name, mass) %>%
  mutate(
    mass2 = mass * 2,
    mass2_squared = mass2 * mass2
  )

# As well as adding new variables, you can use mutate() to
# remove variables and modify existing variables.
starwars %>%
  select(name, height, mass, homeworld) %>%
  mutate(
    mass = NULL, # remove mass
    height = height
  )

# Use across() with mutate() to apply a transformation
# to multiple columns in a tibble.
starwars %>%
  select(name, homeworld, species) %>%
  mutate(across(!name, as.factor))
# see more in ?across

# Window functions are useful for grouped mutates:
starwars %>%
  select(name, mass, homeworld) %>%
  group_by(homeworld) %>%
  mutate(rank = min_rank(desc(mass)))
# see `vignette("window-functions")` for more details

# By default, new columns are placed on the far right.
df <- tibble(x = 1, y = 2)
df %>% mutate(z = x + y)
df %>% mutate(z = x + y, .before = 1)
df %>% mutate(z = x + y, .after = x)

# By default, mutate() keeps all columns from the input data.
df <- tibble(x = 1, y = 2, a = "a", b = "b")
df %>% mutate(z = x + y, .keep = "all") # the default
df %>% mutate(z = x + y, .keep = "used")
df %>% mutate(z = x + y, .keep = "unused")
df %>% mutate(z = x + y, .keep = "none")

# Grouping ----------------------------------------
# The mutate operation may yield different results on grouped
# tibbles because the expressions are computed within groups.
# The following normalises `mass` by the global average:
starwars %>%
  select(name, mass, species) %>%
  mutate(mass_norm = mass / mean(mass, na.rm = TRUE))

# Whereas this normalises `mass` by the averages within species
# levels:
starwars %>%
  select(name, mass, species) %>%
  group_by(species) %>%
  mutate(mass_norm = mass / mean(mass, na.rm = TRUE))

# Indirection ----------------------------------------
# Refer to column names stored as strings with the `.data` pronoun:
vars <- c("mass", "height")
mutate(starwars, prod = .data[[vars[[1]]]] * .data[[vars[[2]]]])

# Learn more in ?rlang::args_data_masking


# across: Apply a function (or functions) across multiple columns

gdf <-
  tibble(g = c(1, 1, 2, 3), v1 = 10:13, v2 = 20:23) %>%
  group_by(g)

set.seed(1)

# Outside: 1 normal variate
n <- rnorm(1)
gdf %>% mutate(across(v1:v2, ~ .x + n))
df %>% 
  select(var, weight, TT) %>% 
  group_by(var)%>% 
  mutate(across(weight:TT, ~ .x + n), val = n)

# Inside a verb: 3 normal variates (ngroup)
gdf %>% mutate(n = rnorm(1), across(v1:v2, ~ .x + n))
df %>% 
  select(var, weight, TT) %>% 
  group_by(var)%>% 
  mutate(n = rnorm(1),across(weight:TT, ~ .x + n), val = n) %>%
  print(n=30)

# Inside `across()`: 6 normal variates (ncol * ngroup)
gdf %>% mutate(across(v1:v2, ~ .x + rnorm(1)))
df %>% 
  select(var, weight, TT) %>% 
  group_by(var)%>% 
  mutate(across(weight:TT, ~ .x + rnorm(1)), val = rnorm(1)) %>%
  print(n=30)
