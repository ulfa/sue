PROJECT = sue
DIALYZER = dialyzer
REBAR = rebar
REPO = ../../../repository

all: app

tar: 
	cd rel; tar cvf $(REPO)/$(PROJECT).$(filter-out $@,$(MAKECMDGOALS)).tar $(PROJECT)
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
