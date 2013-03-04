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

char_field(Name, Options, Value) ->
    boss_form_widget:widget(text_input, Name, Options, Value).

char_field_clean(Value) ->
    %% TODO: Add cleaning
    Value.

choice_field(Name, Options, Value) ->
    boss_form_widget:widget(select, Name, Options, Value).

choice_field_clean(Value) ->
    Value.