-module(test_form_is_valid).
-include_lib("eunit/include/eunit.hrl").
-include("../include/boss_form.hrl").
-compile(export_all).


form_is_not_valid_if_data_required_test() ->
    NewForm = boss_form:new(test_form, []),
    {ValidationResult, FormRecordWithErrors} = boss_form:is_valid(NewForm),

    ?assert(ValidationResult =:= error),
    ExpectedFormErrors = [{username, "This field is required"},
                          {password, "This field is required"}],
    ?assertEqual(ExpectedFormErrors, FormRecordWithErrors#boss_form.errors).

form_is_valid_if_required_data_set_test() ->
    NewForm = boss_form:new(test_form, [{username, "testtest"},
                                        {password, "testtest"}]),
    {ValidationResult, CleanedData} = boss_form:is_valid(NewForm),

    ?assert(ValidationResult =:= ok),
    ?assertEqual([{username, "testtest"},
                  {password, "testtest"}], CleanedData#boss_form.cleaned_data).

form_is_not_valid_if_field_length_is_too_small_test() ->
    NewForm = boss_form:new(test_form, [{username, "testtest"},
                                        {password, "boo"}]),
    {ValidationResult, FormRecordWithErrors} = boss_form:is_valid(NewForm),

    ?assert(ValidationResult =:= error),
    ExpectedFormErrors = [{password, "Field length is less than 5"}],
    ?assertEqual(ExpectedFormErrors, FormRecordWithErrors#boss_form.errors).

form_is_not_valid_if_field_validation_failed_test() ->
    NewForm = boss_form:new(test_form, [{username, "testtest"},
                                        {password, "xxxxxx"}]),

    {ValidationResult, FormRecordWithErrors} = boss_form:is_valid(NewForm),
    ?assert(ValidationResult =:= error),
    ?assertEqual([{password, "Kaboom: \"1\"."}], FormRecordWithErrors#boss_form.errors).
