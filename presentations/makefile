all: data model cpue

data:
	Rscript -e "rmarkdown::render('pcod-data.Rmd')"

model:
	Rscript -e "rmarkdown::render('pcod-model.Rmd')"

cpue:
	Rscript -e "rmarkdown::render('pcod-cpue.Rmd')"

cpue-figs:
	# mkdir figures
	cp ../index/knitr-figs/cpue*.png figures/

