---
image: /front-img/default.png
imagerect:
imagemrgvt: "0%"
imagemrghz: "0%"
draft: true
weight: 1
contenttype: article
description: "Front page summary"
output:
  blogdown::html_page:
    keep_md: true
    md_extensions: +raw_attribute
---
```{r echo=FALSE, child='../../static/chunks/init.Rmd'}
```

# Header 1

<!-- this needs to become a shortcode -->
<img
  id='front-img' src='/front-img/default.png'
  class='post-inset-image'
/>

# Conclusions

<!-- this needs to become a shortcode -->
<!-- this is populated by JS in feedback.html partial -->
<p id='feedback-cont'></p>

# Appendix

## Acknowledgments

## Session Info
