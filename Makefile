all: build

build: hakyll
	./hakyll build

hakyll: hakyll.hs
	ghc --make hakyll.hs

new:
	@./new_post.sh

publish: build
	git stash save
	git checkout publish
	mkdir _source
	find . -maxdepth 1 ! -name _source ! -name . ! -name .git -exec mv '{}' _source/ \;
	cp -r _source/_site/* ./
	rm -fr _source
	git add . &&  git commit -m "Publish" || true
	rm -fr ./*
	git push pub publish:master
	git checkout master
	git checkout -- .
	git stash pop || true
