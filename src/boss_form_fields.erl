-module(boss_form_fields).
-compile(export_all).


%% Sets default input value
input_element(Type, Name, undefined, Options) ->
    input_element(Type, Name, proplists:get_value(default, Options, ""), Options);
input_element(Type, Name, Value, Options) ->
    io_lib:format("<input type='~s' name='~s' value='~s' />", [Type, Name, Value]).

char_field(Name, Options, Value) ->
    input_element(text, Name, Value, Options).

password_field(Name, Options, Value) ->
    input_element(password, Name, Value, Options).