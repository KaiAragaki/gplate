
<style type="text/css">
span > img {
 float:left;
}
</style>
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gp <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->
<!-- badges: end -->

**gp** is a package meant to provide introduce a **g**rammar of
**p**lates.

Microwell plates are usually arranged in visually meaningful ways but
are not tidy data, and their manipulation to and from a tidy form is
cumbersome. gp is aimed at both developers that create packages that
ingest and produce plate data as well as for interactive operating on
microwell data.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KaiAragaki/gp")
```

## Creating plates with gp

Creating a plate plot is simple:

``` r
library(gp)
gp(rows = 8, cols = 16) |>
  gp_plot(as.factor(row))
```

<img src="man/figures/README-example-1.png" width="100%" /> We can add
rectangular ‘sections’ to our plates:

``` r
gp(rows = 16, cols = 24) |> 
  gp_sec("my_section", nrow = 12, ncol = 6, start_corner = "tl", flow = "row") |> 
  gp_plot(my_section)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

These sections can have sections of their own, just by layering one
`gp_sec` on the other.

``` r
gp(rows = 16, cols = 24) |> 
  gp_sec("my_section", nrow = 12, ncol = 6, start_corner = "tl", flow = "row") |> 
  gp_sec("secsec", nrow = 3, ncol = 3, start_corner = "tl", flow = "col") |> 
  gp_plot(secsec)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

`gp` also has a theme - `gp_mini_theme` - that lets you create
sparkline-esque plates.

``` r
mini_plot_cols <- gp(8, 12) |> 
  gp_sec(name = "primers", ncol = 3) |> 
  gp_plot(primers) + 
  gp_mini_theme()
ggplot2::ggsave(filename = "./man/figures/mini-plot_cols.png", 
                plot = mini_plot_cols,
                height = 0.5, 
                width = 0.75, 
                units = "in", 
                scale = 3.5)

mini_plot_rows <- gp(8, 12) |> 
  gp_sec(name = "primers", nrow = 3) |> 
  gp_plot(primers) + 
  gp_mini_theme()
ggplot2::ggsave(filename = "./man/figures/mini-plot_rows.png", 
                plot = mini_plot_rows,
                height = 0.5, 
                width = 0.75, 
                units = "in", 
                scale = 3.5)
```

For instance:

> You should align your primers in columns
> <img src="man/figures/mini-plot_cols.png" width="75" /> instead of in
> rows <img src="man/figures/mini-plot_rows.png" width="75" />

# Vocabulary

## Axes

There are only two axes when defining plates: rows and columns

-   `row`: The absolute row of the plate, starting at the **top** of the
    plate. This axis corresponds most closely with the physical plate.
-   `col`: The absolute column of the plate, starting at the **left** of
    the plate. This axis corresponds most closely with the physical
    plate.

### Modifiers

These axes can have modifier(s) appended to their name, like:

-   `sec`: section. This is the axis number of a given
    [section](#sections).
-   `rel`: relative. This flips the axes such that the starting corner
    is now (1, 1)
-   `par`: parent. This indicates whether the `sec` refers to the
    current section or the parent section.

`par` will not appear without `sec`, but `sec` can appear without `par`.
Order is important. A fully loaded column will look like
`row_sec_par_rel`. KAINOTE(columns where?)

## Sections

A section is a rectangular field of wells. A plate is section - the
largest section. Sections can have sections of their own - these are
child sections. All sections except the plate have parent sections.
Sections must be - at most - the same size as their parent section. **As
of writing, sections must all be the same size, but this will likely
change**.

Sections can have attributes:

-   `padding`
-   `margin`

## Lanes

A lane is used to define a grid of sections, and is usually more than
one well wide (otherwise it is just a row or column). Both lanes are
only used to define sections if `wrap = FALSE`. If `wrap = TRUE`, then
sections are allowed to `flow` off an edge in the specified direction
(`flow = "row"` or `flow = "col"`) and continue on the next line.

# TODO

-   [x] Break Sections: Be able to specify if only ‘whole integer’
    sections should be allowed to appear
    -   wrap = TRUE needs to be put through this stuff too.
-   [ ] Margin/Padding
    -   Should probably JUST do margin for simplicity sake.
-   [x] Real plotting functions. Also a miniplotting function for inline
    plotting (a la sparklines)
-   [x] pkgdown
-   [ ] update hex
-   [ ] Allow user to supply a vector for rows/cols for section
    definition
-   [ ] Data layering? Need to think about how to best do this.
