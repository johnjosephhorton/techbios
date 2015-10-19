# techbios

Tools for working with LinkedIn biographical data for people working in tech, namely Stanford CS degree holders for use "A Theory of Silicon Valley"

## Installation

### Prerequesties:

Certain SQL-related files need to be installed. On Arch Linux:

```
pacman -S postgresql-libs
```

Currently package is available via `github`:

```{r}
install.packages("devtools")
devtools::install_github("johnjosephhorton/techbios",
                         build_vignettes = TRUE)
```

## Documentation

Extended documentation is available in package vignette:

```{r}
vignette("techbios")
```
