# ggfigurestudio

Edit a ggplot in a local Shiny app. Click plot elements to change their appearance, move labels and annotations by hand, and export the result at a fixed size. Save a figure project when you want to reopen the edits later.

## Install from GitHub

Run the following in R.

```r
install.packages("remotes")
remotes::install_github("BaskervilleDog/ggfigurestudio", upgrade = "never")
library(ggfigurestudio)
```

## Tutorials and examples

### Edit and export your first plot

Run this in RStudio to open a scatter plot in the editor.

```r
library(ggfigurestudio)
library(ggplot2)

p <- ggplot(iris, aes(Sepal.Length, Petal.Length, colour = Species)) +
  geom_point() +
  labs(title = "Iris measurements",
       x = "Sepal length (cm)", y = "Petal length (cm)")

fig <- edit_figure(figure_project(p, width = 180, height = 110))
```

1. Click the title. In **Selection**, change **Text**, then click **Apply display settings**.
2. Click a point to choose its species group. In **Appearance**, change **Size** and click **Apply group style**.
3. Click **Return edited project**. In RStudio, the result appears in **Plots** when you use its default graphics device. Run the following in the console to save the editable project and export the figure.

```r
saveRDS(fig, "iris-project.rds")
export_figure(fig, "iris.svg")
export_figure(fig, "iris.png", dpi = 300)
```

Reopen it later with `edit_figure(readRDS("iris-project.rds"))`.

### Edit an ANOVA equation or add a regression label

```r
anova_fig <- edit_figure(gallery_example("anova_one_way"))
```

Click the equation above the treatment means. In **Selection → Content**, keep **Text format** set to **Math (plotmath)** and replace **Text** with the expression below. Click **Apply text edit**. Drag the equation to reposition it.

```r
paste("One-way ANOVA: ", Y[ij] == mu + tau[i] + epsilon[ij])
```

For a regression example, open `edit_figure(gallery_example("regression_linear"))`. Try styling the confidence band, then add a separate equation using **Layers → Add label**. Set **Centre x** to `7` and **Centre y** to `10`, choose **Math (plotmath)**, and enter `paste("Model: ", hat(y) == beta[0] + beta[1]*x)` in **Text**. Click **Apply layer change**. The **Plotmath reference** link beneath the equation hint lists more symbols and layouts.

### Style stacked bars and move a count label

```r
bars <- edit_figure(gallery_example("stacked_bar"))
```

1. Click a coloured box away from its count. Choose **Selected segment** to change one box or **All segments in layer** to apply a style across the layer.
2. Under **Figure → Label placement**, enable **Connect moved labels** and choose **Original position**.
3. Drag a count away from its starting spot. The connector points back to the original position. Choose **Previous position** if you want the connector to start at the spot the label occupied before its latest move.

### Arrange two plots in one image

```r
panel <- figure_composition(
  list(ANOVA = gallery_example("anova_one_way"),
       Regression = gallery_example("regression_linear")),
  width = 280, height = 150, ncol = 2, gap = 5
)
combined <- edit_figure(panel)
export_figure(combined, "experiment-figure.pdf")
```

In **Figure → Multi-plot image**, pick a plot and click **Edit selected plot**. Edit its title or equation, then switch to the other plot. Turn on **Preview combined image** to check the layout. Click **Return edited project** before running the export.

For more plots, open the **Examples** tab or run `gallery_examples()` for the general gallery catalogue.

## Develop from a local checkout

Open `ggfigurestudio.Rproj` in RStudio. If you are setting up the project on a new computer, install its core dependencies once.

```r
install.packages(c(
  "pkgload", "ggplot2", "shiny", "jsonlite", "svglite",
  "systemfonts", "rlang", "withr", "RColorBrewer"
))
```

Run this from the package folder, then try any of the tutorials above.

```r
pkgload::load_all()
```

The dimensions are in millimetres. Use `edit_figure(p)` for the default size of 180 × 110 mm, or `edit_figure()` to start with the built-in CO₂ example.

For a local installation, run `install.packages(".", repos = NULL, type = "source")` from the package folder after installing the dependencies. Then use `library(ggfigurestudio)` in later R sessions. Both loading methods make the package overview available at `?ggfigurestudio`.

## Find a control

Use **Night mode** in the header to switch the editor interface between light and dark. To change the colours of the plot itself, choose a theme under **Figure → Theme**.

| Tab | What you can do |
| --- | --- |
| **Figure** | Set the plot size within the image, customize the theme and grid lines, and add connectors to moved labels. |
| **Layers** | Add reference lines, shapes, text, and labels. Change the geometry of a layer or delete it. |
| **Selection** | Edit the element you clicked in the preview. The controls depend on what you selected. |
| **Examples** | Open a built-in plot and try the suggested edit. |

### Move elements and add connectors

Drag a movable element, or click it and use the arrow keys. The **Arrow-key nudge (mm)** field defaults to 0.5 mm per press. Click an axis to nudge it as a whole, or click a tick label or axis title to move that item. Drag a legend entry to move it individually, or drag the legend background to move the legend. Use **Ctrl+Z** or **Cmd+Z** to undo the latest move or layer deletion in the current session.

For crowded text, enable **Figure → Label placement → Connect moved labels**. Under **Connect to**, choose **Original position** to connect back to the label's starting point, or **Previous position** to connect to where it was just before its latest drag or nudge. To place a custom anchor, first move the label to the spot where the line should start, then drag it to its final position. For example, open **Stacked bars with counts** and move a count away from its segment.

### Edit text, axes, and facets

Click a plot title, subtitle, caption, axis title, legend entry, or facet strip to see the available display settings. Click text or a boxed label in the plot panel to open its appearance controls, including **Font family** and **Font style**. Font families come from the fonts detected on your computer. The selection determines the scope. A facet title's wording belongs to its facet value, while theme styling follows the strip element.

Click a primary numeric axis, a tick label, or a blank part of a panel to open tick options. Enter a comma-separated sequence such as `0, 250, 500, 750`, or enter `0 to 1100 by 100`. Click **Apply facet ticks**. An end value sets an upper bound, and the sequence stops at the last step within that bound. A break outside the displayed range can extend the panel. Clear the field and apply to restore the original breaks and range for that axis. This works on supported Cartesian axes in single panels, `facet_wrap()`, and `facet_grid()`.

### Style marks and choose palettes

Click the mark you want to change. For grouped points, lines, ribbons, and similar marks, **Apply group style** styles the selected ggplot group. **Reset group style** restores its mapped or fixed appearance. For stacked bars, heatmaps, and map polygons, use **Apply to** to switch between the selected segment, cell, or area and all marks of that type in the layer. A layer-wide change replaces individual overrides for the fields you change.

When an eligible mark has a variable mapped to colour or fill, look for **Palette** in **Selection**. For bar segments, tiles, and map areas, choose the whole layer to show the palette menu. Use categorical palettes for groups and continuous palettes for numeric values. For example, try Set2 for genotype groups, or Viridis for a heatmap or enrichment dot plot. Palette changes affect the mapped scale across layers that share it. **Original** restores the plot's palette. The colorspace choices require the optional `colorspace` package.

Discrete legend keys reflect supported group style changes when the key can represent the style consistently. Continuous colour bars show the mapped scale. Use a palette to change that scale and its guide together.

### Add annotations and edit layers

In **Layers**, choose an operation and click **Apply layer change**. Vertical lines use an x position and horizontal lines use a y position. Set the centre of a box, circle, ellipse, text, or label in axis units. For example, use a number on a numeric axis, a category name on a discrete axis, or `YYYY-MM-DD` on a date axis. Width and height for shapes are in millimetres. Equal dimensions produce a circle. Shapes can have attached text, rotation, and independent fill and border opacity.

**Add to facets** places an annotation in the chosen panels. Leaving the selection empty places it in every panel. To edit an existing copy, click it and use **Apply edits to → This facet**, **Selected facets**, or **All copies**. Those scopes work with both style and position fields. **Default scope** edits the clicked position and uses the style default set in **Layers → Facet styling**. Dragging or nudging always moves the clicked copy. Deleting the annotation layer removes all its copies.

For a geometry change, choose **Change layer geometry**, select the source layer and target geom, then apply. The target geom needs compatible data and mappings. **Keep current statistic and position adjustment** is checked by default. Clear it to use the target geom's defaults. For example, change a regular raster grid to **Tiles** to select and style its cells.

### Write equations and mix math with text

In **Layers**, choose **Add text** or **Add label**. Set **Text format** to **Math (plotmath)**, enter an expression in **Text**, set its position, and click **Apply layer change**. To revise it, click the annotation in the preview and use **Selection → Content**.

| Label | Enter in the **Text** field |
| --- | --- |
| Quadratic regression model | `hat(y) == beta[0] + beta[1]*x + beta[2]*x^2` |
| One-way ANOVA model | `Y[ij] == mu + tau[i] + epsilon[ij]` |
| Regression with a text prefix | `paste("Fitted model: ", hat(y) == 2.4 + 1.8*x)` |
| ANOVA result with text | `paste("Treatment effect: ", F[2*","*27] == 4.31, ", ", p == 0.024)` |

Inside `paste()`, quote ordinary text and punctuation. Keep the math outside quotes. Use `==` to display an equals sign, `x^2` for a power, `x[i]` for a subscript, and `frac(a, b)` for a fraction. The numbers in the mixed labels are examples. Replace them with your model results.

The editor uses R's plotmath syntax. Click **Plotmath reference** below the equation hint to open the syntax guide in a new tab, or run `?plotmath` in R. [R plotmath reference](https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html)

### Build a custom theme

In **Figure**, choose a preset and click **Apply theme**. The list includes ggplot2 themes and optional presets from `cowplot`, `ggpubr`, and `ggthemes`. Applying a preset replaces the current theme styling. Choose **Theme at opening** to restore the starting theme. Use **Grid lines** to control major and minor grid visibility independently.

Open **Custom theme**, search for an element such as `plot.title`, `axis.text.x`, or `legend.position`, and choose its properties. The editor lists theme elements registered by your installed ggplot2. Choose **Inherited** for a property, **Inherit element** to clear the entire element setting, or **Hide element** to suppress a drawable element. Use **Apply element** to commit property changes. **Use a full R value** accepts ggplot2 and grid expressions such as `element_text(size = 14, face = "bold")`.

Click **Download theme** to save a reusable theme. Add it to another plot in R.

```r
p + readRDS("ggfigurestudio-theme.rds")
```

## Save, reopen, and export

Use **Figure → Plot size** to set the plot's width and height as percentages of the image. For a 180 × 110 mm image, setting both to 80% gives the plot layout 144 × 88 mm, centred in the same image. This fits the entire plot layout, including titles, axes, and legends. Text sizes and arrow-key nudge distances still use physical units. Use 100% for both dimensions to fill the image.

Save and export the returned project, `fig`, to include manual positions, palette choices, dimensions, and plot settings. Projects saved under the former package name, `ggfigureedit`, can be reopened with `edit_figure(readRDS(...))`, then saved again with `saveRDS()`.

```r
saveRDS(fig, "iris-project.rds")

export_figure(fig, "iris.svg")
export_figure(fig, "iris.pdf")
export_figure(fig, "iris.png", dpi = 300)
export_figure(fig, "iris.tiff", dpi = 600)

# In a later R session, load the package before reopening the saved project.
fig <- edit_figure(readRDS("iris-project.rds"))
```

Use SVG or PDF for vector output, and PNG or TIFF when you need a raster file. The filename extension selects the format, and `dpi` controls raster resolution. Export uses the project dimensions. For PDF, installed font families selected for annotations and text layers use Cairo when it is available.

To redraw a returned project in R, use the commands below. Pass `show = FALSE` to `edit_figure()` to skip the automatic display after returning.

```r
grid::grid.newpage()
grid::grid.draw(render_figure(fig))
```

## Combine plots in one image

Pass a named list of ggplots or saved figure projects to the editor.
Use the composition constructor to set the image size, columns, and gap.

```r
combined <- figure_composition(
  list(Scatter = gallery_example("scatter"), Bars = gallery_example("stacked_bar")),
  width = 260, height = 140, ncol = 2, gap = 5
)
combined <- edit_figure(combined)
export_figure(combined, "combined.svg")
saveRDS(combined, "combined-project.rds")
```

1. In **Figure → Multi-plot image**, choose a plot and click **Edit selected plot**. Each plot keeps its own settings when you switch.
2. Set **Columns** and **Gap (mm)**, then click **Apply layout**. Plots fill the grid from left to right in list order.
3. Turn on **Preview combined image** to check the grid. Click **Return edited project** while editing one of its plots or viewing the combined image. Open the saved composition later with `edit_figure(readRDS("combined-project.rds"))`.

For the default image size and grid, you can also use `edit_figure(list(Scatter = p1, Bars = p2))` with your own plots.

## Explore the galleries

In **Examples**, select a gallery and a plot, then click **Open example**. **Back to starting figure** switches back to your own plot. The editor keeps each figure's edits while it is open, and **Return edited project** returns the figure currently in the preview.

| Gallery | Try this | R catalogue |
| --- | --- | --- |
| **General plots** | Style one stacked segment and then all segments, edit a violin and its boxplot separately, or recolour a heatmap. | `gallery_examples()` |
| **Bioinformatics** | Style a PCA group, or apply a fill palette to the enrichment dot plot. | `bioinfo_examples()` |
| **Plant breeding and quantitative genetics** | Edit genotype lines and points in a reaction norms plot, or recolour field plots and LD tiles. | `breeding_examples()` |
| **Maps** | Select a county polygon, apply a palette to its layer, or convert a raster grid to editable tiles. | `map_examples()` |

The catalogue lists example IDs and required packages where applicable. Open any example by ID or row number with the corresponding singular function. For example, run `edit_figure(breeding_example("reaction_norm"))`. Its x-axis places site labels at each environment's numeric mean yield, so the gaps between sites vary.

Under **General plots**, look for **ANOVA**, **Regression**, and **Experimental designs**. **One-way ANOVA with mean intervals** (`anova_one_way`) and **Linear regression with confidence band** (`regression_linear`) include equations you can select, move, and edit. The other examples cover a two-way interaction, quadratic, logistic and exponential decay fits, regression residuals, randomized blocks, and paired before-and-after measurements. To open the linear example directly, run `edit_figure(gallery_example("regression_linear"))`.

Built-in examples use bundled, simulated, or illustrative data. Package integrations include plots from `DESeq2`, `ggpubr`, `metan`, `statgenSTA`, `sf`, and `terra`, plus examples that use `ggrepel` and `rrBLUP`. Install the optional package named by the app to open a fixture that requires it.

## Compatibility

The editor accepts ggplot objects, including those returned by other R packages. What you can select depends on the geom and coordinate system. Standard Cartesian marks and `geom_sf()` polygons have direct selection. Examples with polar coordinates and repelled text can be rendered and exported, and their click-editing options depend on the layers. The map gallery uses standard Cartesian geoms for selectable stations and transects. Raw raster pixels become individually selectable after conversion to tiles.

Manual positions, styling overrides, and annotations are tied to elements in the current plot. Check them after changing the underlying data, layer order, or grouping in R.

## Troubleshooting

- If the Viewer still shows an earlier preview after you click **Return edited project**, open RStudio's **Plots** pane. Use the redraw commands above to display `fig` again and `export_figure(fig, "figure.svg")` to export its edits.
- If an example or theme reports a missing package, install the package named in the message and reopen the example or apply the theme again.
- If a change fails, read the message in the active tab. For example, when switching a layer to tiles, check whether the source layer provides the x and y positions the new geom requires.

## R reference

| Help topic | What it covers |
| --- | --- |
| `?ggfigurestudio` | The editor workflow and controls |
| `?edit_figure` | Project creation, editing, rendering, and export |
| `?example_figure` | Built-in plots and gallery functions |
| `?list_aesthetics` | Inspecting available aesthetics and mappings in R |

## Example references

The general examples draw on the [R Graph Gallery ggplot2 guide](https://r-graph-gallery.com/ggplot2-package.html). The bioinformatics gallery draws on [EnhancedVolcano](https://bioconductor.org/packages/EnhancedVolcano), [DESeq2](https://bioconductor.org/packages/DESeq2), [PCAtools](https://bioconductor.org/packages/PCAtools), [enrichplot](https://bioconductor.org/packages/enrichplot), [ggbio](https://bioconductor.org/packages/ggbio), [ggtree](https://bioconductor.org/packages/ggtree), and [ComplexHeatmap](https://jokergoo.github.io/ComplexHeatmap-reference/book/a-single-heatmap.html).

The breeding examples draw on [statgenSTA](https://biometris.github.io/statgenSTA/articles/statgenSTA.html), [statgenGxE](https://biometris.github.io/statgenGxE/), [metan](https://doi.org/10.1111/2041-210X.13384), [Piepho and Möhring's heritability paper](https://pmc.ncbi.nlm.nih.gov/articles/PMC2147938/), the [GAPIT manual](https://zzlab.net/GAPIT/previous/gapit_help_document_20230810.pdf), the [LDheatmap paper](https://www.jstatsoft.org/article/view/v016c03), and [Endelman's rrBLUP paper](https://doi.org/10.3835/plantgenome2011.08.0024).
