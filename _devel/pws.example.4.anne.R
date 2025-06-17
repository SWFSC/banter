rm(list = ls())
library(tidyverse)
library(banter)
data(train.data)

# add unique id for each bp detected
bp <- train.data$detectors$bp |> 
  mutate(bp.id = as.character(1:n()))

# pairwise distance of all bp detections
bp.dist <- bp |> 
  select(-event.id, -call.id) |> 
  column_to_rownames("bp.id") |> 
  dist() |> 
  as.matrix()

# unique events
events <- unique(bp$event.id)

# data frame of unique pairs of events
event.pairs <- events |> 
  combn(2) |> 
  t() |> 
  as.data.frame() |> 
  setNames(c("event.1", "event.2"))

# add column for mean distance between pairs of events
event.pairs$dist <- apply(event.pairs, 1, function(x) {
  id.1 <- bp |> 
    filter(event.id == x[1]) |> 
    pull(bp.id)
  id.2 <- bp |> 
    filter(event.id == x[2]) |> 
    pull(bp.id)
  mean(bp.dist[id.1, id.2])
})

# holder pairwise distance matrix for events
event.dist <- matrix(
  0, 
  nrow = length(events), 
  ncol = length(events),
  dimnames = list(events, events)
)

# fill distance matrix
for(i in 1:nrow(event.pairs)) {
  i1 <- event.pairs$event.1[i]
  i2 <- event.pairs$event.2[i]
  event.dist[i1, i2] <- event.dist[i2, i1] <- event.pairs$dist[i]
}

# filled distance matrix
str(event.dist)
