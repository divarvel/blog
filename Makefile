all: build

cabal.sandbox.config:
	cabal sandbox init --sandbox=../hakyll-sandbox

build: dist/build/blog/blog
	./dist/build/blog/blog build

dist/build/blog/blog: Main.hs cabal.sandbox.config
	cabal build
	./dist/build/blog/blog clean

new:
	@./new_post.sh

publish:
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


preview: dist/build/blog/blog
	./dist/build/blog/blog preview -p 9000

clean: dist/build/blog/blog
	./dist/build/blog/blog clean
