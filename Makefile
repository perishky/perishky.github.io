
HTML=r/managing-data/slides.html r/robust-scripts/slides.html

all: $(HTML)

%.html: %.rmd templates/remark-min.html
	bash scripts/remark.sh $? > $@

clean:
	rm $(HTML)
