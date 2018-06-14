Rscript -e "library(knitr);knit('./res-doc.rnw')" 1> knitr-output.log 2>&1

(@latex -synctex=1 "res-doc.tex" && bibtex "res-doc" && latex "res-doc.tex" && latex "res-doc.tex" && dvips "res-doc.dvi" && ps2pdf "res-doc.ps") 1> latex-output.log 2>&1
