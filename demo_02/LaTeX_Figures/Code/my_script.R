
# This is a simple R program to produce a figure.

# Generate a random variable.
epsilon <- rnorm(1000)

# Plot a histogram.
fig_ext <- 'eps'
fig_dir <- 'Figures'
fig_file_name <- sprintf('name_of_figure.%s', fig_ext)
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)

setEPS()
postscript(out_file_name)

hist(epsilon, col = 'blue')

dev.off()


