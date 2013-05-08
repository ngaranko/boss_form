-module(login_form, [InitialData, Errors]).
-compile(export_all).

form_fields() ->
    [
     {username, [{type, char_field},
                 {label, "Username"},
                 {required, true},
                 {html_options, [{class, "input-block-level"},
                                {placeholder, "Email address"}]}]},
     {password, [{type, char_field},
                 {widget, password_input},
                 {label, "Password"},
                 {required, true},
                 {html_options, [{class, "input-block-level"},
                                {placeholder, "Password"}]}]},
     {remember_me, [{type, boolean_field},
                    {label, "Remember me"}]}, %% Note: this field isn't required, so no need to say so
     {dummy_float_field_with_custom_widget, [{type, float_field},
                                             {label, "Dummy float"},
                                             {widget, {my_custom_widgets, bootstrap_price_input}}]}
    ].



%% Proxies
data() ->
    InitialData.

errors() ->
    Errors.

fields() ->
    boss_form:fields(form_fields(), InitialData).

as_table() ->
    boss_form:as_table(form_fields(), InitialData, Errors).


%% Validators
validate_password(_Options, RequestData) ->
    case string:len(proplists:get("password", RequestData, "")) > 5 of
        true -> ok;
        false -> {error, "Password is too short"}
    end.