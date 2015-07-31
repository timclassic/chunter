VERSION=0.6.2
COMPONENT=chunter
SUFFIX:=$(shell echo $(SUFFIX))
BASENAME:=$(COMPONENT)-$(VERSION)$(SUFFIX)
.PHONY: package clean

clean:
	rm *.gz

package:
	cp install.sh $(BASENAME)
	cd .. && tar -cf ../../_build/prod/rel/$(BASENAME).tar $(COMPONENT)
	uuencode $(BASENAME).tar $(BASENAME).tar >> $(BASENAME)
	[ -f $(BASENAME).gz ] && rm $(BASENAME).gz || true
	rm  $(BASENAME).tar
	gzip -9 $(BASENAME)
