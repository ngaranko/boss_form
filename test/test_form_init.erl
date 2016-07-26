-module(test_form_init).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("../include/boss_form.hrl").


form_init_returns_form_record_test() ->
    NewForm = boss_form:new(test_form, []),
    ?assert(NewForm#boss_form.module =:= test_form).

form_init_returns_form_record_with_intial_data_test() ->
    InitialData = [{username, <<"test">>}, {password, <<"test">>}],
    NewForm = boss_form:new(test_form, InitialData),
    ?assert(NewForm#boss_form.data =:= InitialData).
