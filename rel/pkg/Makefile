VERSION=0.3.0
COMPONENT=chunter
BASENAME=$(COMPONENT)-$(VERSION)$(SUFFIX)
.PHONY: package clean

clean:
	rm *.gz

package:
	cp install.sh $(BASENAME)
	cd .. && tar -cf pkg/$(BASENAME).tar $(COMPONENT)
	uuencode $(BASENAME).tar $(BASENAME).tar >> $(BASENAME)
	gzip $(BASENAME)
