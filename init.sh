#!/bin/sh
erl -sname chat_over_ws -pa deps/*/ebin -pa ebin -eval "application:start(crypto), application:start(compiler), application:start(syntax_tools), application:start(cowboy), application:start(chat_over_ws)."
