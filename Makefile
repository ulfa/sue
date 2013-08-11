PROJECT = sue
DIALYZER = dialyzer
REBAR = rebar
REPO = ../../../../repository
REPOSRC = ../../repository
TARGET = ~/projects/erlang

all: app

tar: app 
	cd rel; tar cvf $(REPO)/$(PROJECT).$(VERSION).tar $(PROJECT)

tarall: app 
	cd ..; tar cvf $(REPOSRC)/$(PROJECT).src.$(VERSION).tar $(PROJECT) --exclude .git/* --exclude log/*

cpall: tarall
	cd ..;scp $(REPOSRC)/$(PROJECT).src.$(VERSION).tar $(USR)@$(HOST):$(TARGET)

cp: tar
	 cd ..;scp $(REPOSRC)/$(PROJECT).$(VERSION).tar $(USR)@$(HOST):$(TARGET)

release: app
	@$(REBAR) generate

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

tests: clean app eunit ct

eunit:
	@$(REBAR) eunit skip_deps=true

ct:
	@$(REBAR) ct

build-plt:
	@$(DIALYZER) --build_plt --output_plt .ct_dialyzer.plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze:
	@$(DIALYZER) --src src --plt .sue_dialyzer.plt \
		-Wbehaviours -Werror_handling \
		-Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true
