VERSION ?= 0.1.0.0

EXECS = smhi-api \
		smhi-scraper

COMMON_LIBS = /etc/protocols \
			  /etc/services \
			  /lib/x86_64-linux-gnu/libnss_files.so.2 \
			  /lib/x86_64-linux-gnu/libnss_dns.so.2

define TARGETS_template
$$($(1)_ROOT)$(2): $$($(1)_ROOT) $(2)
	cp -L --parents $(2) $($(1)_ROOT)
endef

define VARIABLES_template
$(1)_EXE_PATH := $(shell stack exec which $(1))
$(1)_ROOT := $(1)-root
$(1)_LIBS := $(shell ldd .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/smhi-scraper/smhi-scraper | rev | cut -d ' ' -f2 | rev) $(COMMON_LIBS)
$(1)_TARGETS := $$(patsubst %, $$($(1)_ROOT)%, $$($(1)_LIBS))

$$($(1)_ROOT):
	@mkdir -p $$($(1)_ROOT)

$$($(1)_ROOT)/$(1): $$($(1)_ROOT)
	stack build
	cp $$($(1)_EXE_PATH) $$($(1)_ROOT)

$$(foreach lib,$$($(1)_LIBS),$$(eval $$(call TARGETS_template,$(1),$$(lib))))

$(1): $$($(1)_ROOT) $$($(1)_TARGETS) $$($(1)_ROOT)/$(1)
	tar -cC $$($(1)_ROOT) . | docker import - $(1):$(VERSION)
	docker tag $(1):$(VERSION) $(1):latest

endef

$(foreach sub,$(EXECS),$(eval $(call VARIABLES_template,$(sub))))

default: all

all: $(EXECS) smhi-nginx

smhi-nginx:
	docker build -t smhi-nginx:latest -f Dockerfile.nginx .
