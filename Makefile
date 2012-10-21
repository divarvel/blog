all: build

build: hakyll
	./hakyll build

hakyll: hakyll.hs
	ghc --make hakyll.hs

new:
	@./new_post.sh

publish: build
	#ToDo
