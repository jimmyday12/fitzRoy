# Docker Support

``` r
library(fitzRoy)
```

## Docker Support

**Building the Docker Image**

To build the Docker image run the following from the root of the
repository.

- `docker build -t jimmyday12/fitzroy:latest -f docker/rstudio/Dockerfile .`
- To start [RStudio](https://posit.co/) with Fitzroy ready to use:
  - `docker run -d -p 8787:8787 -e PASSWORD={YOUR PASSWORD} --name fitzroy jimmyday12/fitzroy:latest`
    and open `http://localhost:8787`. \*(Username: `rstudio`, Password:
    `{YOUR PASSWORD}`)
- To start an R terminal prompt with fitzRoy ready to use:
  - `docker run -it jimmyday12/fitzroy:latest R` to start with an R
    terminal prompt.
  - Run [`quit()`](https://rdrr.io/r/base/quit.html) to exit the
    container
