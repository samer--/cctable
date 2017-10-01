PACKNAME=cctable
VER=$(shell cat VERSION)
PACKFILE=release/$(PACKNAME)-$(VER).tgz
EPACKFILE=release\/$(PACKNAME)-$(VER).tgz
ZIPFILE=release/$(PACKNAME)-$(VER).zip
DOWNLOAD=https:\/\/raw.githubusercontent.com\/samer--\/cctable\/master\/$(EPACKFILE)

main:

packdir:
	mkdir -p $(PACKNAME) $(PACKNAME)/prolog 
	sed -e "s/<VER>/$(VER)/g" < pack.pl | sed -e "s/<PACKNAME>/$(PACKNAME)/g" | sed -e "s/<DOWNLOAD>/$(DOWNLOAD)/g" > $(PACKNAME)/pack.pl
	(sed -e "s/cctable_trie_kp/cctable/" < prolog/cctable_trie_kp.pl && echo ':- reexport(library(ccmacros)).') > $(PACKNAME)/prolog/cctable.pl
	cp -p prolog/ccmacros.pl $(PACKNAME)/prolog/ccmacros.pl
	# cp -p README $(PACKNAME)
	# cp -p ../COPYING* $(PACKNAME)

pack: packdir
	COPYFILE_DISABLE=1 tar czf $(PACKFILE) $(PACKNAME)
	rm -rf $(PACKNAME)
	git add $(PACKFILE)

zip: packdir
	zip -r $(ZIPFILE) $(PACKNAME)
	rm -rf $(PACKNAME)
	git add $(ZIPFILE)

push:
	git push

install:
	swipl -g "pack_install('$(PACKFILE)',[upgrade(true),interactive(false)]), halt"

install-git:
	(git commit $(PACKFILE) || echo 'Local up to date') && (git push || echo 'Remote up to date')
	swipl -g "pack_property($(PACKNAME),download(D)), pack_install(D,[upgrade(true),interactive(false)]), halt"
	chmod -w $(PACKFILE)

dist-clean:
