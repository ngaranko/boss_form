ERL=erl
REBAR=./rebar
DB_CONFIG_DIR=priv/test_db_config

.PHONY: deps get-deps

all:
	@$(REBAR) get-deps
	@$(REBAR) compile

boss_db:
	@$(REBAR) compile skip_deps=true

clean:
	@$(REBAR) clean

get-deps:
	@$(REBAR) get-deps

deps:
	@$(REBAR) compile
