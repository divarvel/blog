all: build

build: hakyll
	./hakyll build

hakyll: hakyll.hs
	ghc --make hakyll.hs
	./hakyll clean

new:
	@./new_post.sh

publish: build
	git add .
	git stash save
	git checkout publish || git checkout --orphan publish
	find . -maxdepth 1 ! -name '.' ! -name '.git*' ! -name '_site' -exec rm -rf {} +
	find _site -maxdepth 1 -exec mv {} . \;
	rmdir _site
	git add -A && git commit -m "Publish" || true
	git push -f git+ssh://git@push.clever-cloud.com/app_dac7568e-d269-4a05-9bb9-a118a4099fb3.git publish:master
	git checkout master
	git clean -fdx
	git stash pop || true

preview: hakyll
	./hakyll preview -p 9000

clean: hakyll
	./hakyll clean
