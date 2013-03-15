-module(boss_form_fields).
-compile(export_all).

boolean_field(Name, Options, Value) ->
    boss_form_widget:widget(checkbox_input, Name, Options, Value).

boolean_field_clean(Value) ->
    case Value of
        "" -> false;
        "0" -> false;
        undefined -> false;
        Value -> true
    end.

float_field(Name, Options, Value) ->
    Val = case erlang:is_binary(Value) of
        true -> erlang:list_to_float(erlang:bitstring_to_list(Value));
        false -> Value
    end,
    EndVal = case erlang:is_float(Val) of
        true -> io_lib:format(proplists:get_value(format, Options, "~.2f"), [Val]);
        false -> Val
    end,
    boss_form_widget:widget(text_input, Name, Options, EndVal).

float_field_clean(undefined) ->
    0.00;
float_field_clean(Value) ->
    case string:to_float(Value) of
        {error, no_float} ->
            try list_to_integer(Value) of
                AnyValue ->
                    float(AnyValue)
            catch
                error:badarg ->
                    {error, "Field value is not correct"}
            end;
        {F, _Rest} -> F
    end.

char_field(Name, Options, Value) ->
    boss_form_widget:widget(text_input, Name, Options, Value).

char_field_clean(Value) ->
    %% TODO: Add cleaning
    Value.

choice_field(Name, Options, Value) ->
    boss_form_widget:widget(select, Name, Options, Value).

choice_field_clean(Value) ->
    Value.