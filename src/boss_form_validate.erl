-module(boss_form_validate).
-export([field/3, required/2, min_length/2, max_length/2]).
-include("../include/boss_form.hrl").


field(FormRecord, FieldName, Options) ->
    Value = case proplists:get_value(type, Options, char_field) of
                file_field ->
                    boss_form_request:file_field_value(
                      FieldName,
                      FormRecord#boss_form.files,
                      proplists:get_value(initial, Options, []));
                _Other ->
                    boss_form_request:field_value(
                      FieldName,
                      FormRecord#boss_form.data,
                      proplists:get_value(initial, Options, undefined))
            end,

    case clean_field(Value, Options, proplists:get_value(type, Options, char_field)) of
        {error, ErrorData} ->
            {error, [FieldName, ErrorData]};
        OtherValue ->
            post_cleanup_validation(FormRecord, FieldName, OtherValue, Options)
    end.

%% Check if field is required and has request value
required(Options, Value) ->
    case proplists:get_value(required, Options, false) of
        true when Value =:= undefined ->
            error;
        true when Value =:= "" ->
            error;
        _ ->
            ok
    end.

%% Check field min length
min_length(Options, Value) ->
    case proplists:get_value(min_length, Options, false) of
        MinLength when is_integer(MinLength), Value =/= "", Value =/= undefined ->
            case MinLength > string:len(Value) of
                true ->
                    {error, lists:flatten(io_lib:format("Field length is less than ~p", [MinLength]))};
                false ->
                    ok
            end;
        _ ->
            ok
    end.

%% Check field max lenght
max_length(Options, Value) ->
    case proplists:get_value(max_length, Options, false) of
        MaxLength when is_integer(MaxLength), Value =/= "", Value =/= undefined ->
            case MaxLength < string:len(Value) of
                true ->
                    {error, lists:flatten(io_lib:format("Field length is more than ~s", [MaxLength]))};
                false ->
                    ok
            end;
        _ ->
            ok
    end.


%% INTERNAL


%% Apply form field validation, if any
apply_form_validation(FormRecord, FieldName, Value, Options) ->
    FormModule = FormRecord#boss_form.module,
    Exports = FormModule:module_info(exports),
    ValidationFunc = list_to_atom(string:concat("validate_", atom_to_list(FieldName))),
    case proplists:is_defined(ValidationFunc, Exports) of
        true ->
            FormModule:ValidationFunc(Value, Options);
        false ->
            ok
    end.

clean_field(Value, Options, FieldType) ->
    Cleaner = list_to_atom(string:concat(atom_to_list(FieldType), "_clean")),
    Exports = boss_form_fields:module_info(exports),
    case proplists:get_value(Cleaner, Exports, 1) of
        1 -> apply(boss_form_fields, Cleaner, [Value]);
        2 -> apply(boss_form_fields, Cleaner, [Value, Options])
    end.

post_cleanup_validation(FormRecord, FieldName, Value, Options) ->
    ErrorMessages = proplists:get_value(error_messages, Options, []),
    case required(Options, Value) of
        error ->
            {error, {FieldName, proplists:get_value(requried, ErrorMessages, "This field is required")}};
        ok ->
            case min_length(Options, Value) of
                ok ->
                    case max_length(Options, Value) of
                        ok ->
                            case apply_form_validation(FormRecord, FieldName, Value, Options) of
                                ok ->
                                    {ok, FieldName, Value};
                                {ok, UpdatedValue} ->
                                    {ok, FieldName, UpdatedValue};
                                {error, ErrorMessage} ->
                                    {error, {FieldName, ErrorMessage}}
                            end;
                        {error, Message} ->
                            {error, {FieldName, proplists:get_value(max_length, ErrorMessages, Message)}}
                    end;
                {error, Message} ->
                    {error, {FieldName, proplists:get_value(min_length, ErrorMessages, Message)}}
            end
    end.
