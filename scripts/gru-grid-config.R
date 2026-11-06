# Full factorial axes. Dropout is one shared regularization setting for input
# and head dropout, and for between-layer dropout when there is >1 GRU layer.
# Keep the search ranges together so a reader can see the experimental question at a
# glance. This defines a full factorial grid: every listed value is crossed with every
# value on the other axes.
gru_grid_axes <- function(profile='compact') {
  stopifnot(profile %in% c('compact','broader','depth-check'))
  # depth-check reproduces all earlier fits to verify the complete pipeline
  # without claiming to execute the broader width/learning-rate search.
  list(num_layers=1:3,hidden_size=if(profile=='depth-check')32L else c(32L,64L),
    learning_rate=if(profile=='depth-check').001 else c(.001,.003),
    dropout=if(profile=='broader')c(0,.1) else .1,
    weight_decay=if(profile=='broader')c(0,1e-4) else 1e-4)
}
