library(tidyverse)
library(Momocs)

# lf should be a list of file paths, typically obtained with list.files
lf  <- list.files("~/Desktop/txts/", full=T)                # full paths
lf0 <- lf %>% 
  str_split("/") %>% 
  map_chr(~.x[length(.x)]) %>% 
  str_replace("\\.[[:alpha:]]{3}$", "")# extract short, polished, names from them

map2_df(lf, lf0,
     # read files
     ~read_delim(.x,
                 delim="\t",
                 col_names = c("ldk", "x", "y"),
                 col_types = cols("c", "d", "d")) %>% 
       # add an id based on filenames
       mutate(id=.y) %>% 
       # reorder columns
       select(id, everything())) %>% 
  # fill x,y with NA when missing ldk
  complete(crossing(id, ldk)) %>% 
  # reorder 
  arrange(id, ldk) %>%
  # and split
  split(.$id) %>% 
  # turn into a list of rownamed matrices
  map(~.x %>% select(x, y) %>% as.matrix() %>% `row.names<-`(.x$ldk)) -> coo

# easy to turn into an array
coo %>% l2a
