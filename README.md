# Not So Short Introduction to R

This is a renderable Quarto book scaffold.

## Why this fixes the error

Quarto book projects require every file listed under `book: chapters:` in `_quarto.yml` to exist. This scaffold includes placeholder `.qmd` files for all chapters so the book can render while the manuscript is still being written.

## Render commands

From the project root, run:

```bash
quarto render
```

For HTML only:

```bash
quarto render --to html
```

For PDF only:

```bash
quarto render --to pdf
```

## Important note

The file `assets/apa.csl` is a placeholder CSL file included to avoid a missing-file error. Replace it with the official APA 7 CSL file before final publication if you need strict APA output.
